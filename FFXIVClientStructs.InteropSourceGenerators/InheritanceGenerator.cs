using System.Collections.Immutable;
using System.Reflection;
using System.Runtime.InteropServices;
using FFXIVClientStructs.InteropGenerator;
using FFXIVClientStructs.InteropSourceGenerators.Extensions;
using FFXIVClientStructs.InteropSourceGenerators.Models;
using LanguageExt;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using static FFXIVClientStructs.InteropSourceGenerators.DiagnosticDescriptors;

namespace FFXIVClientStructs.InteropSourceGenerators;

[Generator]
internal sealed class InheritanceGenerator : IIncrementalGenerator {
    private const string InheritanceAttributeName = "FFXIVClientStructs.Interop.Attributes.InheritanceAttribute`1";

    public void Initialize(IncrementalGeneratorInitializationContext context) {
        IncrementalValuesProvider<(Validation<DiagnosticInfo, StructInfo> StructInfo,
            Seq<Validation<DiagnosticInfo, InheritanceInfo>> InheritanceInfos)> structAndInheritanceInfos =
            context.SyntaxProvider
                .ForAttributeWithMetadataName(
                    InheritanceAttributeName,
                    static (node, _) => node is StructDeclarationSyntax {
                        AttributeLists.Count: > 0
                    },
                    static (context, _) => {
                        StructDeclarationSyntax structSyntax = (StructDeclarationSyntax)context.TargetNode;
                        INamedTypeSymbol symbol = (INamedTypeSymbol)context.TargetSymbol;
                        return (Struct: StructInfo.GetFromSyntax(structSyntax),
                            Info: InheritanceInfo.GetFromRoslyn(structSyntax, symbol));
                    });

        IncrementalValuesProvider<Validation<DiagnosticInfo, StructWithInheritanceInfos>> structWithInheritanceInfos =
            structAndInheritanceInfos.Select(static (item, _) => {
                var (structInfo, inheritanceInfos) = item;
                return (structInfo, inheritanceInfos.Traverse(info => info)).Apply(static (si, ii) =>
                    new StructWithInheritanceInfos(si, ii));
            });

        context.RegisterSourceOutput(structWithInheritanceInfos,
            (sourceContext, item) => {
                item.Match(
                    Fail: diagnosticInfos => {
                        diagnosticInfos.Iter(dInfo => sourceContext.ReportDiagnostic(dInfo.ToDiagnostic()));
                    },
                    Succ: structWithInheritanceInfo => {
                        structWithInheritanceInfo.AddSource(sourceContext);
                    });
            });
    }

    internal sealed record InheritanceInfo(StructInfo StructInfo, string TypeName, int Offset, Seq<FieldInfoWithOffset> FieldInfos) {
        public static Seq<Validation<DiagnosticInfo, InheritanceInfo>> GetFromRoslyn(StructDeclarationSyntax structSyntax, INamedTypeSymbol namedTypeSymbol) {
            Validation<DiagnosticInfo, StructInfo> validStructInfo = StructInfo.GetFromSyntax(structSyntax);

            var inheritances = namedTypeSymbol.GetAttributeDatasByTypeName(InheritanceAttributeName.Replace("`1", "")).Select(attribute => {
                Option<AttributeData> optAttr = attribute;
                var offset = optAttr.GetValidAttributeArgument<int>("Offset", 0, InheritanceAttributeName, namedTypeSymbol);
                var structType = attribute.AttributeClass!.TypeArguments.First();
                var members = structType.GetMembers().Where(t => t is IFieldSymbol).Cast<IFieldSymbol>().Select(FieldInfoWithOffset.GetFromRoslyn).ToSeq();
                var structTypeName = optAttr
                    .Bind<string>(attrData => attrData.AttributeClass!.TypeArguments.First().GetFullyQualifiedNameWithGenerics())
                    .ToValidation(
                        DiagnosticInfo.Create(
                            AttributeGenericTypeArgumentInvalid,
                            namedTypeSymbol,
                            InheritanceAttributeName));
                return (offset, structTypeName, members.Traverse(fiwo => fiwo));
            });

            return inheritances.Select(inheritance => {
                var (offsetVal, structType, fiwo) = inheritance;
                return (validStructInfo, structType, offsetVal, fiwo).Apply((structInfo, typeName, offset, fiwo) => new InheritanceInfo(structInfo, typeName, offset, fiwo));
            });
        }

        public string RenderSource() {
            IndentedStringBuilder builder = new();

            builder.AppendLine($"// {TypeName} {Offset:X}");
            StructInfo.RenderStart(builder);

            FieldInfos.Iter(fiwo => {
                fiwo.RenderSource(builder, Offset);
            });

            StructInfo.RenderEnd(builder);

            return builder.ToString();
        }

        public string GetFileName() {
            return StructInfo.Name + "." + TypeName.Split('.')[^1] + ".Inheritance.g.cs";
        }
    }

    private sealed record StructWithInheritanceInfos(StructInfo StructInfo, Seq<InheritanceInfo> InheritanceInfos) {
        public void AddSource(SourceProductionContext sourceContext) {
            InheritanceInfos.Iter(info => {
                sourceContext.AddSource(info.GetFileName(), info.RenderSource());
            });
        }
    }

    internal sealed record FieldInfoWithOffset(string Name, string TypeName, int Offset) {
        public static Validation<DiagnosticInfo, FieldInfoWithOffset> GetFromRoslyn(IFieldSymbol fieldSymbols) {
            Validation<DiagnosticInfo, IFieldSymbol> validSymbol = Validation<DiagnosticInfo, IFieldSymbol>.Success(fieldSymbols);

            Validation<DiagnosticInfo, string> validType = Validation<DiagnosticInfo, string>.Success(fieldSymbols.Type.GetFullyQualifiedNameWithGenerics());

            Option<AttributeData> offsetOpt = fieldSymbols.GetAttributes().FirstOrDefault(attr => attr.AttributeClass?.Name == "FieldOffsetAttribute");

            Validation<DiagnosticInfo, int> validOffset = offsetOpt.GetValidAttributeArgument<int>("offset", 0, "System.Runtime.InteropServices.FieldOffset", fieldSymbols);

            return (validSymbol, validType, validOffset).Apply((symbol, type, offset) =>
                               new FieldInfoWithOffset(symbol.Name, type, offset));
        }

        public void RenderSource(IndentedStringBuilder builder, int offset) {
            builder.AppendLine($"[FieldOffset(0x{(offset + Offset):X})] public {TypeName} {Name};");
        }
    }
}
