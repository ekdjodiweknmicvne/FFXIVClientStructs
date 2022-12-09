﻿using FFXIVClientStructs.FFXIV.Client.System.Framework;
using FFXIVClientStructs.FFXIV.Component.GUI;

namespace FFXIVClientStructs.FFXIV.Client.UI.Agent;
// Client::UI::Agent::AgentModule

// size = 0xC10
// ctor E8 ? ? ? ? 48 8B 85 ? ? ? ? 49 8B CF 48 89 87
[StructLayout(LayoutKind.Explicit, Size = 0xD38)]
public unsafe partial struct AgentModule
{
    public static AgentModule* Instance() => Framework.Instance()->GetUiModule()->GetAgentModule();
    
    [FieldOffset(0x0)] public void* vtbl;
    [FieldOffset(0x8)] public UIModule* UIModule;
    [FieldOffset(0x10)] public byte Initialized;
    [FieldOffset(0x11)] public byte Unk_11;
    [FieldOffset(0x14)] public uint FrameCounter;
    [FieldOffset(0x18)] public float FrameDelta;

    [FieldOffset(0x20)] public AgentInterface* AgentArray; // 417 pointers patch 6.20

    [FieldOffset(0xD28)] public UIModule* UIModulePtr;
    [FieldOffset(0xD30)] public AgentModule* AgentModulePtr;

    [MemberFunction("E8 ?? ?? ?? ?? 83 FE 0D")]
    public partial AgentInterface* GetAgentByInternalID(uint agentID);

    public AgentInterface* GetAgentByInternalId(AgentId agentId)
    {
        return GetAgentByInternalID((uint) agentId);
    }

    public AgentHUD* GetAgentHUD()
    {
        return (AgentHUD*) GetAgentByInternalId(AgentId.Hud);
    }

    public AgentHudLayout* GetAgentHudLayout()
    {
        return (AgentHudLayout*) GetAgentByInternalId(AgentId.HudLayout);
    }

    public AgentTeleport* GetAgentTeleport()
    {
        return (AgentTeleport*) GetAgentByInternalId(AgentId.Teleport);
    }

    public AgentLobby* GetAgentLobby()
    {
        return (AgentLobby*) GetAgentByInternalId(AgentId.Lobby);
    }

    public AgentMap* GetAgentMap()
    {
        return (AgentMap*) GetAgentByInternalId(AgentId.Map);
    }

    public AgentScreenLog* GetAgentScreenLog()
    {
        return (AgentScreenLog*) GetAgentByInternalId(AgentId.ScreenLog);
    }

    public AgentItemSearch* GetAgentItemSearch()
    {
        return (AgentItemSearch*) GetAgentByInternalId(AgentId.ItemSearch);
    }

    public AgentRetainerList* GetAgentRetainerList()
    {
        return (AgentRetainerList*) GetAgentByInternalId(AgentId.RetainerList);
    }

    public AgentRevive* GetAgentRevive()
    {
        return (AgentRevive*) GetAgentByInternalId(AgentId.Revive);
    }

    public AgentSalvage* GetAgentSalvage()
    {
        return (AgentSalvage*) GetAgentByInternalId(AgentId.Salvage);
    }

    public AgentMonsterNote* GetAgentMonsterNote()
    {
        return (AgentMonsterNote*)GetAgentByInternalId(AgentId.MonsterNote);
    }

    public AgentMJIPouch* GetAgentMJIPouch()
    {
        return (AgentMJIPouch*)GetAgentByInternalId(AgentId.MJIPouch);
    }

    public AgentAozContentBriefing* GetAgentAozContentBriefing()
    {
        return (AgentAozContentBriefing*)GetAgentByInternalId(AgentId.AozContentBriefing);
    }

    public AgentAozContentResult* GetAgentAozContentResult()
    {
        return (AgentAozContentResult*)GetAgentByInternalId(AgentId.AozContentResult);
    }

    public AgentDeepDungeonStatus* GetAgentDeepDungeonStatus()
    {
	    return (AgentDeepDungeonStatus*)GetAgentByInternalId(AgentId.DeepDungeonStatus);
    }

    public AgentDeepDungeonMap* GetAgentDeepDungeonMap()
    {
	    return (AgentDeepDungeonMap*)GetAgentByInternalId(AgentId.DeepDungeonMap);
    }
}

public enum AgentId : uint {
    Lobby = 0,
    CharaMake = 1,
    MovieStaffList = 2, // this is the addon name, no idea what the agent actually is, shows up when playing the EW cutscene in title
    Cursor = 3,
    Hud = 4,
    ChatLog = 5,
    Inventory = 6,
    ScenarioTree = 7,
    EventFade = 8,
    Context = 9,
    InventoryContext = 10,
    Config = 11, //ConfigSystem
    ConfigLog = 12,
    ConfigLogColor = 13,
    Configkey = 14,
    ConfigCharacter = 15,
    ConfigPadcustomize = 16,
    ChatConfig = 17,
    HudLayout = 18,
    Emote = 19,
    Macro = 20,
    // TargetCursor,
    TargetCircle = 21,
    GatheringNote = 22,
    RecipeNote = 23,
    RecipeTree = 24,
    RecipeMaterialList = 25,
    RecipeProductList = 26,
    FishingNote = 27,
    FishGuide = 28,
    FishRecord = 29,
    Journal = 31,
    ActionMenu = 32,
    Marker = 33,
    Trade = 34,
    ScreenLog = 35,
    // NPCTrade,
    Request = 36,
    Status = 37,
    Map = 38,
    Loot = 39, //NeedGreed
    Repair = 40,
    
    Materialize = 42,
    MateriaAttach = 43,
    MiragePrism = 44,
    Colorant = 45,
    Howto = 46,
    HowtoNotice = 47,
    ContentsTutorial = 48,
    Inspect = 49,
    Teleport = 50,
    TelepotTown = 51, // Aethernet
    ContentsFinder = 52,
    ContentsFinderSetting = 53,
    Social = 54,
    SocialBlacklist = 55,
    SocialFriendList = 56,
    Linkshell = 57,
    SocialPartyMember = 58,

    // PartyInvite,
    SocialSearch = 60,
    SocialDetail = 61,
    LetterList = 62,
    LetterView = 63,
    LetterEdit = 64,
    ItemDetail = 65,
    ActionDetail = 66,
    Retainer = 67,
    Return = 68,
    Cutscene = 69,
    CutsceneReplay = 70,
    MonsterNote = 71,
    ItemSearch = 72, //MarketBoard
    GoldSaucerReward = 73,
    FateProgress = 74, //Shared FATE
    Catch = 75,
    FreeCompany = 76,

    // FreeCompanyOrganizeSheet,
    FreeCompanyProfile = 78,

    FreeCompanyProfileEdit = 79,
    // FreeCompanyInvite,
    FreeCompanyInputString = 81,
    FreeCompanyChest = 82,
    FreeCompanyExchange = 83,
    FreeCompanyCrestEditor = 84,
    FreeCompanyCrestDecal = 85,

    // FreeCompanyPetition = 85,
    ArmouryBoard = 87,
    HowtoList = 88,
    Cabinet = 89, 
    LegacyItemStorage = 90,
    GrandCompanyRank = 91,
    GrandCompanySupply = 92,
    GrandCompanyExchange = 93,
    Gearset = 94,
    SupportMain = 95,
    SupportList = 96,
    SupportView = 97,
    SupportEdit = 98,
    Achievement = 99,
    // CrossEditor,
    LicenseViewer = 101,
    ContentsTimer = 102,
    MovieSubtitle = 103,
    PadMouseMode = 104,
    RecommendList = 105,
    Buddy = 106,
    ColosseumRecord = 107, // PVP Results
    CloseMessage = 108,
    CreditPlayer = 109,
    CreditScroll = 110,
    CreditCast = 111,
    CreditEnd = 112,
    CreditCutCast = 113,
    Shop = 114,
    Bait = 115,
    Housing = 116,
    HousingHarvest = 117,
    HousingSignboard = 118,
    HousingPortal = 119,
    HousingTravellersNote = 120,
    HousingPlant = 121,
    PersonalRoomPortal = 122,
    HousingBuddyList = 123,
    TreasureHunt = 124,
    Salvage = 125,
    LookingForGroup = 126,
    ContentsMvp = 127,
    VoteKick = 128,
    VoteGiveUp = 129,
    VoteTreasure = 130,
    PvpProfile = 131,
    ContentsNote = 132,
    ReadyCheck = 133,
    FieldMarker = 134,
    CursorLocation = 135,
    CursorRect = 136,
    RetainerStatus = 137,
    RetainerTask = 138,
    
    RelicNotebook = 141,
    RelicSphere = 142,
    TradeMultiple = 143,
    RelicSphereUpgrade = 144,

    Relic2Glass = 147,
    Minigame = 148,
    Tryon = 149,
    AdventureNotebook = 150, 
    ArmouryNotebook = 151,
    MinionNotebook = 152,
    MountNotebook = 153,
    ItemCompare = 154, 
    DailyQuestSupply = 155,
    MobHunt = 156,
    //PatchMark = 157, ?
    HousingWithdrawStorage = 158,
    WeatherReport = 159,

    LoadingTips = 161,
    Revive = 162,

    ChocoboRace = 164,

    GoldSaucerMiniGame = 166,
    TrippleTriad = 167,

    LotteryDaily = 175,
    AetherialWheel = 176,
    LotteryWeekly = 177,
    GoldSaucer = 178,
    TripleTriadCoinExchange = 179,
    ShopExchangeCoin = 180, //MGP Exchange
    JournalAccept = 181,
    JournalResult = 182,
    LeveQuest = 183,
    CompanyCraftRecipeNoteBook = 184,

    AirShipExploration = 186,

    AirShipExplorationDetail = 188,

    SubmersibleExplorationDetail = 192,
    CompanyCraftMaterial = 193,
    AetherCurrent = 194,
    FreeCompanyCreditShop = 195,
    Currency = 196,
    PuryfyItemSelector = 197, //Aetherial Reduction

    LovmParty = 199,
    LovmRanking = 200,
    LovmNamePlate = 201,
    CharacterTitle = 202,
    CharacterTitleSelect = 203,
    LovmResult = 204,
    LovmPaletteEdit = 205,
    SkyIslandFinder = 206, //Exploratory Missions
    SkyIslandFinderSetting = 207,

    ItemContextCustomize = 210,
    BeginnersMansionProblem = 211, //Hall of the Novice
    DpsChallenge = 212, //Stone, Sky, Sea
    PlayGuide = 213,
    WebLauncher = 214,
    WebGuidance = 215,
    Orchestrion = 216,
    BeginnerChatList = 217, //Novice Network

    ReturnerDialog = 220,
    OrchestrionInn = 221,
    HousingEditContainer = 222,
    ConfigPartyListRoleSort = 223,
    RecommendEquip = 224,
    YkwNote = 225, //yokai watch medallium
    ContentsFinderMenu = 226,
    RaidFinder = 227,
    GcArmyExpedition = 228,
    GcArmyMemberList = 229,

    DeepDungeonInspect = 231,
    DeepDungeonMap = 232,
    DeepDungeonStatus = 233,
    DeepDungeonSaveData = 234,
    DeepDungeonScore = 235,
    GcArmyTraining = 236,
    GcArmyMenberProfile = 237,
    GcArmyExpeditionResult = 238,
    GcArmyCapture = 239,
    GcArmyOrder = 240,
    MansionSelectRoom = 241,
    OrchestrionPlayList = 242,
    CountDownSettingDialog = 243,
    WeeklyBingo = 244, //Wondrous Tails
    WeeklyPuzzle = 245, //Faux Hollows

    PvPDuelRequest = 247,
    PvPHeader = 248,

    AquariumSetting = 252,

    DeepDungeonMenu = 254,

    ItemAppraisal = 257, //DeepDungeon Appraisal
    ItemInspection = 258, //Lockbox
    RecipeItemContext = 259, // context menus for RecipeTree and RecipeList, constructor inlined
    ContactList = 260,

    Snipe = 265,
    MountSpeed = 266,

    PvpTeam = 282,

    EurekaElementalHud = 284,
    EurekaElementalEdit = 285,
    EurekaChainInfo = 286,

    TeleportHousingFriend = 290,
    ContentMemberList = 291,
    InventoryBuddy = 292,
    ContentsReplayPlayer = 293,
    ContentsReplaySetting = 294,
    MiragePrismPrismBox = 295, //Glamour Dresser
    MiragePrismPrismItemDetail = 296,
    MiragePrismMiragePlate = 297, //Glamour Plates

    Fashion = 301,

    HousingGuestBook = 304,

    ReconstructionBox = 307,
    ReconstructionBuyback = 308,
    CrossWorldLinkShell = 309,

    Description = 311, //Frontline/Bozja Description
    AozNotebook = 316, //Bluemage Spells

    Emj = 319, //Mahjong

    EmjIntro = 322,
    AozContentBriefing = 323, //Masked Carnivale
    AozContentResult = 324,

    WorldTravel = 325,
    RideShooting = 326, //Airforce One

    Credit = 328,
    EmjSetting = 329, //Mahjong Settings
    RetainerList = 330,

    Dawn = 335, //Trust
    DawnStory = 336, //Duty Support
    HousingCatalogPreview = 337,

    QuestRedo = 340,
    QuestRedoHud = 341,

    CircleList = 343, //Fellowships
    CircleBook = 344,

    CircleFinder = 349,

    MentorCondition = 351,
    PerformanceMetronome = 352,
    PerformanceGamepadGuide = 353,

    PerformanceReadyCheck = 355,

    HwdAetherGauge = 359,

    HwdScore = 361,

    HwdMonument = 363,
    McGuffin = 364, //Collection
    CraftActionSimulator = 365,

    MycWarResultNotebook = 372,
    MycInfo = 373, //Bozja Info
    MycItemBox = 374, //Bozja Lost Finds Cache
    MycItemBag = 375, //Bozja Lost Finds Holster

    MycBattleAreaInfo = 377, //Bozja Recruitment

    OrnamentNoteBook = 379, //Accessories

    TourismMenu = 381,

    StarlightGiftBox = 383,
    SpearFishing = 384,
    Omikuji = 385,

    AkatsukiNote = 387, //Unending Codex
    ExHotbarEditor = 388,
    BannerList = 389, // Portraits
    BannerEditor = 390, // Portrait Editor
    BannerUpdateView = 391,
    PvPMap = 392,

    CharaCard = 394, // AdventurerPlate
    CharaCardDesignSetting = 395,
    CharaCardProfileSetting = 396,

    PvPMKSIntroduction = 398,
    MJIHud = 399,  // Island Sanctuary
    MJIPouch = 400,
    MJIRecipeNoteBook = 401,
    MJICraftSchedule = 402,
    MJICraftSales = 403,
    MJIAnimalManagement = 404,
    MJIFarmManagement = 405,
    MJIGatheringHouse = 406,
    MJIBuilding = 407,
    MJIGatheringNoteBook = 408,
    MJIDisposeShop = 409,
    MJIMinionManagement = 410,
    MJIMinionNoteBook = 411, 
    MJIBuildingMove = 412,
    MJIEntrance = 413,
    ArchiveItem = 414,
    VVDNotebook = 415,
    VVDFinder = 416
}
