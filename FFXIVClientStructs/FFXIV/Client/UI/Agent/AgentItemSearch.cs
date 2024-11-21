using FFXIVClientStructs.FFXIV.Client.Game;
using FFXIVClientStructs.FFXIV.Client.System.String;

namespace FFXIVClientStructs.FFXIV.Client.UI.Agent;

// Client::UI::Agent::AgentItemSearch
//   Client::UI::Agent::AgentInterface
//     Component::GUI::AtkModuleInterface::AtkEventInterface
// ctor "48 89 5C 24 ?? 48 89 6C 24 ?? 48 89 74 24 ?? 48 89 7C 24 ?? 41 56 48 83 EC 20 33 ED C6 41 08 00 48 89 69 18"
[Agent(AgentId.ItemSearch)]
[GenerateInterop]
[Inherits<AgentInterface>]
[StructLayout(LayoutKind.Explicit, Size = 0x3880)]
public unsafe partial struct AgentItemSearch {
    [FieldOffset(0x98)] public StringHolder* StringData;
    // [FieldOffset(0xA2C), FixedSizeArray] internal FixedSizeArray100<uint> _unkUints;
    [FieldOffset(0xA29)] public bool ListingPageLoaded;
    [FieldOffset(0xBBC), FixedSizeArray] internal FixedSizeArray100<uint> _listingPageItemIds;
    [FieldOffset(0xD50)] public uint ListingPageItemCount;
    [FieldOffset(0xD58), FixedSizeArray] internal FixedSizeArray100<ListingItem> _listingPageItems;
    [FieldOffset(0x3120)] public byte ListingCurrentPage;
    [FieldOffset(0x3121)] public byte ListingPageCount;
    [FieldOffset(0x3384)] public uint ResultItemId;
    [FieldOffset(0x338C)] public uint ResultSelectedIndex;
    [FieldOffset(0x3398)] public InventoryItem ResultHoveredItem;
    [FieldOffset(0x33A4), Obsolete("Use ResultHoveredItem.GetSlot()")] public ushort ResultHoveredIndex;
    [FieldOffset(0x33A8), Obsolete("Use ResultHoveredItem.GetItemId()/GetBaseItemId()")] public uint ResultHoveredItemId;
    [FieldOffset(0x33AC), Obsolete("Use ResultHoveredItem.GetQuantity()")] public uint ResultHoveredCount;
    [FieldOffset(0x33B4), Obsolete("Use ResultHoveredItem.IsHighQuality()")] public byte ResultHoveredHQ;
    // [FieldOffset(0x3858)] public uint* ItemBuffer;
    // [FieldOffset(0x3860)] public uint ItemCount;
    [FieldOffset(0x3871)] public bool IsPartialSearching;
    // [FieldOffset(0x386D)] public bool IsItemPushPending;

    [StructLayout(LayoutKind.Explicit, Size = 0x98)]
    public struct StringHolder {
        // [FieldOffset(0x10)] public int Unk90Size;
        [FieldOffset(0x28)] public Utf8String SearchParam;
        // [FieldOffset(0x90)] public nint Unk90Ptr;
    }

    [StructLayout(LayoutKind.Explicit, Size = 0x20)]
    public struct ListingItem {
        [FieldOffset(0x08)] public uint ItemId;

        [FieldOffset(0x18)] public ushort Index;
        [FieldOffset(0x1A)] public ushort OfferCount;
        [FieldOffset(0x1C)] public ushort Demand;
    }
}
