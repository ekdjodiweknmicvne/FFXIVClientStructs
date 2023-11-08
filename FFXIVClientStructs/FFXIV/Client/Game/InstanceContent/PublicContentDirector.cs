using FFXIVClientStructs.FFXIV.Application.Network.WorkDefinitions;

namespace FFXIVClientStructs.FFXIV.Client.Game.InstanceContent;

[StructLayout(LayoutKind.Explicit, Size = 0x1090)]
[Inheritance<ContentDirector>]
public unsafe partial struct PublicContentDirector {
    [MemberFunction("40 53 57 48 83 EC 78 48 8B D9 48 8D 0D")]
    public static partial nint HandleEnterContentInfoPacket(EnterContentInfo* packet);
}
