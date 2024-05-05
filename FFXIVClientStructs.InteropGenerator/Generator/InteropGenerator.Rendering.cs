﻿using FFXIVClientStructs.InteropGenerator.Helpers;
using FFXIVClientStructs.InteropGenerator.Models;

namespace FFXIVClientStructs.InteropGenerator.Generator;

public sealed partial class InteropGenerator {
    private static string RenderStructInfo(StructInfo structInfo) {
        using IndentedTextWriter writer = new();
        // write file header
        writer.WriteLine("// <auto-generated/>");

        // write namespace 
        if (structInfo.Namespace.Length > 0) {
            writer.WriteLine($"namespace {structInfo.Namespace};");
            writer.WriteLine();
        }

        // write opening struct hierarchy in reverse order
        // note we do not need to specify the accessibility here since a partial declared with no accessibility uses the other partial
        for (var i = structInfo.Hierarchy.Length - 1; i >= 0; i--) {
            writer.WriteLine($"unsafe partial struct {structInfo.Hierarchy[i]}");
            writer.WriteLine("{");
            writer.IncreaseIndent();
            ;
        }

        // write closing struct hierarchy
        for (var i = 0; i < structInfo.Hierarchy.Length; i++) {
            writer.DecreaseIndent();
            writer.WriteLine("}");
        }

        return writer.ToString();
    }
}