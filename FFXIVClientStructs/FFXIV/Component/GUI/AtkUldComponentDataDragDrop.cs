namespace FFXIVClientStructs.FFXIV.Component.GUI;

[GenerateInterop]
[Inherits<AtkUldComponentDataBase>]
[StructLayout(LayoutKind.Explicit, Size = 0x10)]
public unsafe partial struct AtkUldComponentDataDragDrop {
    [FieldOffset(0x0C), FixedSizeArray] internal FixedSizeArray1<uint> _nodes;
}
