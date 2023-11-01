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

    internal sealed record InheritanceInfo(StructInfo StructInfo, string TypeName, int Offset, Seq<FieldInfoWithOffset> FieldInfos, Seq<FuncInfo> FuncInfos) {
        public static Seq<Validation<DiagnosticInfo, InheritanceInfo>> GetFromRoslyn(StructDeclarationSyntax structSyntax, INamedTypeSymbol namedTypeSymbol) {
            Validation<DiagnosticInfo, StructInfo> validStructInfo = StructInfo.GetFromSyntax(structSyntax);

            var inheritances = namedTypeSymbol.GetAttributeDatasByTypeName(InheritanceAttributeName.Replace("`1", "")).Select(attribute => {
                Option<AttributeData> optAttr = attribute;
                var offset = optAttr.GetValidAttributeArgument<int>("offset", 0, InheritanceAttributeName, namedTypeSymbol);
                var structType = attribute.AttributeClass!.TypeArguments.First();
                var members = structType.GetMembers();
                var methods = members.Where(t => t is IMethodSymbol).Cast<IMethodSymbol>().Select(FuncInfo.GetFromRoslyn).ToSeq();
                var fields = members.Where(t => t is IFieldSymbol).Cast<IFieldSymbol>().Select(FieldInfoWithOffset.GetFromRoslyn).ToSeq();
                var structTypeName = optAttr
                    .Bind<string>(attrData => attrData.AttributeClass!.TypeArguments.First().GetFullyQualifiedNameWithGenerics())
                    .ToValidation(
                        DiagnosticInfo.Create(
                            AttributeGenericTypeArgumentInvalid,
                            namedTypeSymbol,
                            InheritanceAttributeName));
                return (offset, structTypeName, fields.Traverse(fiwo => fiwo), methods.Traverse(vfi => vfi));
            });

            return inheritances.Select(inheritance => {
                var (offsetVal, structType, fiwo, vfi) = inheritance;
                return (validStructInfo, structType, offsetVal, fiwo, vfi).Apply((structInfo, typeName, offset, fiwo, vfi) => new InheritanceInfo(structInfo, typeName, offset, fiwo, vfi));
            });
        }

        public string RenderSource() {
            IndentedStringBuilder builder = new();

            builder.AppendLine($"// {TypeName} {Offset:X}");
            StructInfo.RenderStart(builder);

            FieldInfos.Iter(fiwo => {
                fiwo.RenderSource(builder, Offset, StructInfo.Name);
            });

            // This does not work in the current state of Source Generators because its not compiled yet and thus not in the assembly for Roslyn to find for other Source Generators
            // FuncInfos.Iter(vfi => {
            //     vfi.RenderSource(builder);
            // });

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
        public static Validation<DiagnosticInfo, FieldInfoWithOffset> GetFromRoslyn(IFieldSymbol fieldSymbol) {
            Validation<DiagnosticInfo, IFieldSymbol> validSymbol = Validation<DiagnosticInfo, IFieldSymbol>.Success(fieldSymbol);

            Validation<DiagnosticInfo, string> validType = Validation<DiagnosticInfo, string>.Success(fieldSymbol.Type.GetFullyQualifiedNameWithGenerics());

            Option<AttributeData> offsetOpt = fieldSymbol.GetAttributes().FirstOrDefault(attr => attr.AttributeClass?.Name == "FieldOffsetAttribute");

            Validation<DiagnosticInfo, int> validOffset = offsetOpt.GetValidAttributeArgument<int>("offset", 0, "System.Runtime.InteropServices.FieldOffset", fieldSymbol);

            return (validSymbol, validType, validOffset).Apply((symbol, type, offset) =>
                               new FieldInfoWithOffset(symbol.Name, type, offset));
        }

        public void RenderSource(IndentedStringBuilder builder, int offset, string structName) {
            builder.AppendLine($"[FieldOffset(0x{(offset + Offset):X})] public {TypeName} {(Name != structName ? Name : TypeName.Split('.')[^1])};");
        }
    }

    internal sealed record FuncInfo(string Name, string TypeName, object? Val, bool VFunc) {
        public static Validation<DiagnosticInfo, FuncInfo> GetFromRoslyn(IMethodSymbol methodSymbol) {
            Validation<DiagnosticInfo, IMethodSymbol> validSymbol = Validation<DiagnosticInfo, IMethodSymbol>.Success(methodSymbol);

            Validation<DiagnosticInfo, string> validType = Validation<DiagnosticInfo, string>.Success(methodSymbol.ReturnType.GetFullyQualifiedNameWithGenerics());

            Option<AttributeData> attrData = methodSymbol.GetFirstAttributeDataByTypeName("FFXIVClientStructs.Interop.Attributes.VirtualFunctionAttribute");

            if (attrData.IsNone) {
                attrData = methodSymbol.GetFirstAttributeDataByTypeName("FFXIVClientStructs.Interop.Attributes.MemberFunctionAttribute");
                if (attrData.IsNone) {
                    return (validSymbol, validType).Apply((symbol, type) =>
                        new FuncInfo(symbol.Name, type, null, false));
                }
                Validation<DiagnosticInfo, string> validSig = attrData.GetValidAttributeArgument<string>("signature", 0, "FFXIVClientStructs.Interop.Attributes.MemberFunctionAttribute", methodSymbol);

                return (validSymbol, validType, validSig).Apply((symbol, type, sig) =>
                    new FuncInfo(symbol.Name, type, sig, false));
            } else {
                Validation<DiagnosticInfo, uint> validOffset = attrData.GetValidAttributeArgument<uint>("index", 0, "FFXIVClientStructs.Interop.Attributes.VirtualFunctionAttribute", methodSymbol);

                return (validSymbol, validType, validOffset).Apply((symbol, type, offset) =>
                    new FuncInfo(symbol.Name, type, offset, true));
            }

        }

        public void RenderSource(IndentedStringBuilder builder) {
            if (Val is null)
                return;
            builder.AppendLine($"[{(VFunc ? $"VirtualFunction({Val})" : $"MemberFunction(\"{Val}\")")}] public partial {TypeName} {Name}();");
        }
    }
}
