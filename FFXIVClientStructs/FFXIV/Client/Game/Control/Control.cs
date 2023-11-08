using FFXIVClientStructs.FFXIV.Client.Game.Character;

namespace FFXIVClientStructs.FFXIV.Client.Game.Control;

[StructLayout(LayoutKind.Explicit, Size = 0x5A60)]
[Inheritance<CameraManager>, Inheritance<TargetSystem>(0x180)]
public unsafe partial struct Control {
    [FieldOffset(0x5AE8)] public uint LocalPlayerObjectId;
    [FieldOffset(0x5AF0)] public BattleChara* LocalPlayer;

    [StaticAddress("4C 8D 35 ?? ?? ?? ?? 85 D2", 3)]
    public static partial Control* Instance();
}
