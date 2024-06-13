using FFXIVClientStructs.FFXIV.Component.GUI;

namespace FFXIVClientStructs.FFXIV.Client.UI;

// Client::UI::AddonImage
//   Component::GUI::AtkUnitBase
//     Component::GUI::AtkEventListener
[Addon("_Image, _Image3")]
[GenerateInterop]
[Inherits<AtkUnitBase>]
[StructLayout(LayoutKind.Explicit, Size = 0x290)]
public unsafe partial struct AddonImage {
    [FieldOffset(0x260)] public AtkResNode* ResNode1; // Both AtkResNode's appear to be the same node
    [FieldOffset(0x268)] public AtkResNode* ResNode2;
    [FieldOffset(0x270)] public AtkImageNode* ImageNode;

    [FieldOffset(0x27C)] public ushort Width;
    [FieldOffset(0x280)] public ushort Height;
}
