using FFXIVClientStructs.FFXIV.Client.System.String;
using FFXIVClientStructs.FFXIV.Component.GUI;

namespace FFXIVClientStructs.FFXIV.Client.UI;

// Client::UI::AddonGuildLeve
//   Component::GUI::AtkUnitBase
//     Component::GUI::AtkEventListener
[Addon("GuildLeve")]
[GenerateInterop]
[Inherits<AtkUnitBase>]
[StructLayout(LayoutKind.Explicit, Size = 0x1900)]
public unsafe partial struct AddonGuildLeve {
    [FieldOffset(0x228)] public AtkComponentTreeList* AtkComponentTreeList228;
    [FieldOffset(0x230)] public AtkComponentRadioButton* FieldcraftButton;
    [FieldOffset(0x238)] public AtkComponentRadioButton* TradecraftButton;

    [FieldOffset(0x248), CExporterUnion("Button1")] public AtkComponentRadioButton* CarpenterButton;
    [FieldOffset(0x250), CExporterUnion("Button2")] public AtkComponentRadioButton* BlacksmithButton;
    [FieldOffset(0x258), CExporterUnion("Button3")] public AtkComponentRadioButton* ArmorerButton;
    [FieldOffset(0x260)] public AtkComponentRadioButton* GoldsmithButton;
    [FieldOffset(0x268)] public AtkComponentRadioButton* LeatherworkerButton;
    [FieldOffset(0x270)] public AtkComponentRadioButton* WeaverButton;
    [FieldOffset(0x278)] public AtkComponentRadioButton* AlchemistButton;
    [FieldOffset(0x280)] public AtkComponentRadioButton* CulinarianButton;

    [FieldOffset(0x248), CExporterUnion("Button1")] public AtkComponentRadioButton* MinerButton;
    [FieldOffset(0x250), CExporterUnion("Button2")] public AtkComponentRadioButton* BotanistButton;
    [FieldOffset(0x258), CExporterUnion("Button3")] public AtkComponentRadioButton* FisherButton;

    [FieldOffset(0x288)] public AtkResNode* AtkResNode288;

    [FieldOffset(0x290), CExporterUnion("Text1")] public Utf8String CarpenterString;
    [FieldOffset(0x2F8), CExporterUnion("Text2")] public Utf8String BlacksmithString;
    [FieldOffset(0x360), CExporterUnion("Text3")] public Utf8String ArmorerString;
    [FieldOffset(0x3C8)] public Utf8String GoldsmithString;
    [FieldOffset(0x430)] public Utf8String LeatherworkerString;
    [FieldOffset(0x498)] public Utf8String WeaverString;
    [FieldOffset(0x500)] public Utf8String AlchemistString;
    [FieldOffset(0x568)] public Utf8String CulinarianString;

    [FieldOffset(0x290), CExporterUnion("Text1")] public Utf8String MinerString;
    [FieldOffset(0x2F8), CExporterUnion("Text2")] public Utf8String BotanistString;
    [FieldOffset(0x360), CExporterUnion("Text3")] public Utf8String FisherString;

    [FieldOffset(0x5D0)] public AtkComponentButton* JournalButton;
    [FieldOffset(0x5D8)] public AtkTextNode* AtkTextNode298;
    [FieldOffset(0x5E0)] public AtkComponentBase* AtkComponentBase290;
    [FieldOffset(0x5E8)] public AtkComponentBase* AtkComponentBase298;

    [FieldOffset(0x18A0)] public AtkAddonControl AddonControl;
}
