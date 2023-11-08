using FFXIVClientStructs.FFXIV.Client.Game.Event;

namespace FFXIVClientStructs.FFXIV.Client.Game.InstanceContent;

[StructLayout(LayoutKind.Explicit, Size = 0xC48)]
[Inheritance<Director>]
public partial struct ContentDirector {
    [FieldOffset(0xC08)] public float ContentTimeLeft;
}
