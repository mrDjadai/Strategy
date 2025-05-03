unit Window;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Objects, FMX.Layouts, DataTypes,
  FMX.Media, System.ImageList, FMX.ImgList, FMX.Styles.Objects;

type
  TForm2 = class(TForm)
    OPLabel: TLabel;
    OP: TLabel;
    CharacterPanel: TImage;
    Map: TLayout;
    KeyPressTimer: TTimer;
    RightMenu: TLayout;
    DownMenu: TLayout;
    SkipRound: TButton;
    PlayerIndicator: TImage;
    RoundPrefix: TLabel;
    RoundText: TLabel;
    ActionsPrefix: TLabel;
    ActionsText: TLabel;
    Skill1Button: TButton;
    Skill2Button: TButton;
    AttackButton: TButton;
    MPButton: TButton;
    MoneyPrefix: TLabel;
    MoneyText: TLabel;
    MainMenu: TLayout;
    StartButton: TButton;
    PrepairLayout: TLayout;
    BuyMoneyPrefix: TLabel;
    BuyMoneyText: TLabel;
    BuyRoundSkip: TButton;
    PlacerPanel: TLayout;
    WinPanel: TPanel;
    WinnerText: TLabel;
    WinnerIndicator: TImage;
    CharactersOrigin: TLayout;
    BuildingsOrigin: TLayout;
    MusicPlayer: TMediaPlayer;
    AudioLooper: TTimer;
    MovePlayer: TMediaPlayer;
    WinPlayer: TMediaPlayer;
    ClickPlayer: TMediaPlayer;
    DiePlayer: TMediaPlayer;
    BuildingPlacePlayer: TMediaPlayer;
    CharacterPlacePlayer: TMediaPlayer;
    DemolishPlayer: TMediaPlayer;
    KapkanPlayer: TMediaPlayer;
    PortalPlayer: TMediaPlayer;
    BuyPlayer: TMediaPlayer;
    IncorrectPlayer: TMediaPlayer;
    SelectPlayer: TMediaPlayer;
    Audio: TLayout;
    EscPlayer: TMediaPlayer;
    SpeedLabel: TLabel;
    HPLabel: TLabel;
    ArmorLabel: TLabel;
    Speed: TLabel;
    Hp: TLabel;
    Armor: TLabel;
    BonusDicesLabel: TLabel;
    BonusDices: TLabel;
    ErrorPanel: TPanel;
    NoCharError: TLabel;
    NoMapsError: TLabel;
    ErrorHeader: TLabel;
    NoCellError: TLabel;
    CubeOrigin: TLayout;
    MenuExitButton: TButton;
    ExitBar: TProgressBar;
    ExitTimer: TTimer;
    WinExit: TButton;
    GameExiter: TButton;
    NoParametrsError: TLabel;
    MusicBar: TTrackBar;
    SoundBar: TTrackBar;
    MusicLabel: TLabel;
    SoundLabel: TLabel;
    NextMapPage: TButton;
    LastMapPage: TButton;
    NoBuildingsError: TLabel;
    SliderPlayer: TMediaPlayer;
    BuyPanel: TImage;
    MapPanel: TImage;
    BG: TImage;
    procedure OpenGame(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState);
    procedure CheckPressKey(Sender: TObject);
    procedure SkipRoundClick(Sender: TObject);
    procedure MPButonClick(Sender: TObject);
    procedure AttackButtonClick(Sender: TObject);
    procedure Skill1ButtonClick(Sender: TObject);
    procedure Skill2ButtonClick(Sender: TObject);
    procedure StartGame(Sender: TObject);
    procedure BuyRoundSkipClick(Sender: TObject);
    procedure TryLoopAudio(Sender: TObject);
    procedure MenuExitButtonClick(Sender: TObject);
    procedure ExitTimerTimer(Sender: TObject);
    procedure ExitToMenu(Sender: TObject);
    procedure GameExiterClick(Sender: TObject);
    procedure SoundBarChange(Sender: TObject);
    procedure MusicBarChange(Sender: TObject);
    procedure LastMapPageClick(Sender: TObject);
    procedure NextMapPageClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure OnChooseMap(Sender: TObject);
    procedure TryBuyCharacter(Sender: TObject);
    procedure SelectToPlace(Sender: TObject);
  public
    procedure DeleteAnimation(Sender: TObject);
  end;

  CharButton = class(TButton)
  public
  var
    cost: integer;
    id: integer;
  end;

  PlacerButton = class(TButton)
  private
    _count: integer;
    procedure SetCount(c: integer);
  public
  var
    txt: TLabel;
    isBuilding: boolean;
    id: integer;
    price: integer;
    property Count: integer read _count write SetCount;
  end;

var
  Form2: TForm2;
  minMapX, minMapY: integer;
  charPlacers: Array of PlacerButton;
  buildPlacers: Array of PlacerButton;
  money, roundMoney: integer;

  mapList: Array of TButton;
  mapPage: integer;

procedure ShowPlacersCount(player: TPlayer);

const
  useConsole = false;

implementation

uses CellManager, Winapi.Windows, CharacterDataVisualisator,
  CharacterManager, PlayerManager, buildingManager, System.IOUtils,
  SettingsManager;
{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

const
  mapMovingSpeed = 4; // движение карты

var
  pressedA, pressedW, pressedD, pressedS: boolean;
  wantExit: boolean;

procedure TForm2.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if MainMenu.Visible = false then
  begin
    ClearPlayerData();
    DeleteMap();
  end;

  for var p in players do
    p.Free;
end;

procedure TForm2.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  if Key = vkEscape then
  begin
    if TargetSelectionMode then
    begin
      TargetSelectionMode := false;
      selectedSkill.timeAfterUse := selectedSkill.reloadTime;
      selectedSkill := nil;
      Form2.SkipRound.Enabled := true;
      UnselectMap();
      SetActionCount(GetActionCount() + 1);
      CharacterDataVisualisator.ReDraw();
      EscPlayer.CurrentTime := 0;
      EscPlayer.Play();
    end;
    Key := 0;
  end;
  case KeyChar of
    'W', 'w', 'Ц', 'ц':
      pressedW := true;
    'A', 'a', 'Ф', 'ф':
      pressedA := true;
    'S', 's', 'Ы', 'ы':
      pressedS := true;
    'D', 'd', 'В', 'в':
      pressedD := true;
  end;
end;

procedure TForm2.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  case KeyChar of
    'W', 'w', 'Ц', 'ц':
      pressedW := false;
    'A', 'a', 'Ф', 'ф':
      pressedA := false;
    'S', 's', 'Ы', 'ы':
      pressedS := false;
    'D', 'd', 'В', 'в':
      pressedD := false;
  end;
end;

procedure TForm2.GameExiterClick(Sender: TObject);
begin
  Form2.Close();
end;

procedure InitAudio();
begin
  with Form2 do
  begin
    MovePlayer.FileName := ExtractFilePath(ParamStr(0)) +
      'Resourses\Audio\Other\Move.wav';

    ClickPlayer.FileName := ExtractFilePath(ParamStr(0)) +
      'Resourses\Audio\Other\Click.wav';

    WinPlayer.FileName := ExtractFilePath(ParamStr(0)) +
      'Resourses\Audio\Other\Win.wav';

    DiePlayer.FileName := ExtractFilePath(ParamStr(0)) +
      'Resourses\Audio\Other\Die.wav';

    DemolishPlayer.FileName := ExtractFilePath(ParamStr(0)) +
      'Resourses\Audio\Other\Demolish.wav';

    KapkanPlayer.FileName := ExtractFilePath(ParamStr(0)) +
      'Resourses\Audio\Other\Kapkan.wav';

    CharacterPlacePlayer.FileName := ExtractFilePath(ParamStr(0)) +
      'Resourses\Audio\Other\PlaceCharacter.wav';

    BuildingPlacePlayer.FileName := ExtractFilePath(ParamStr(0)) +
      'Resourses\Audio\Other\PlaceBuilding.wav';

    PortalPlayer.FileName := ExtractFilePath(ParamStr(0)) +
      'Resourses\Audio\Other\Portal.wav';

    BuyPlayer.FileName := ExtractFilePath(ParamStr(0)) +
      'Resourses\Audio\Other\Buy.wav';

    IncorrectPlayer.FileName := ExtractFilePath(ParamStr(0)) +
      'Resourses\Audio\Other\Incorrect.wav';

    SelectPlayer.FileName := ExtractFilePath(ParamStr(0)) +
      'Resourses\Audio\Other\Select.wav';

    EscPlayer.FileName := ExtractFilePath(ParamStr(0)) +
      'Resourses\Audio\Other\Esc.wav';

    SliderPlayer.FileName := ExtractFilePath(ParamStr(0)) +
      'Resourses\Audio\Other\Slider.wav';
  end;
end;

procedure TForm2.CheckPressKey(Sender: TObject);
var
  edge: TMapEdge;
begin
  // Движение карты (оставляем как было)
  if pressedW then
    Map.Position.Y := Map.Position.Y + mapMovingSpeed;
  if pressedS then
    Map.Position.Y := Map.Position.Y - mapMovingSpeed;
  if pressedA then
    Map.Position.X := Map.Position.X + mapMovingSpeed;
  if pressedD then
    Map.Position.X := Map.Position.X - mapMovingSpeed;

  edge := CheckMapEdges();

  // Коррекция позиции (инвертируем направление)
  case edge of
    meLeft: Map.Position.X := Map.Position.X + mapMovingSpeed;
    meRight: Map.Position.X := Map.Position.X - mapMovingSpeed;
    meTop: Map.Position.Y := Map.Position.Y - mapMovingSpeed;
    meBottom: Map.Position.Y := Map.Position.Y + mapMovingSpeed;
    meTopLeft:
      begin
        Map.Position.X := Map.Position.X + mapMovingSpeed;
        Map.Position.Y := Map.Position.Y - mapMovingSpeed;
      end;
    meTopRight:
      begin
        Map.Position.X := Map.Position.X - mapMovingSpeed;
        Map.Position.Y := Map.Position.Y - mapMovingSpeed;
      end;
    meBottomLeft:
      begin
        Map.Position.X := Map.Position.X + mapMovingSpeed;
        Map.Position.Y := Map.Position.Y + mapMovingSpeed;
      end;
    meBottomRight:
      begin
        Map.Position.X := Map.Position.X - mapMovingSpeed;
        Map.Position.Y := Map.Position.Y + mapMovingSpeed;
      end;
  end;
end;

const
  cButtonScaleX = 200;
  cButtonScaleY = 40;

  cButtonOffsetX = 300;
  cButtonOffsetY = 80;

  cButtonStartX = 100;
  cButtonStartY = 65;

  cButtonColumns = 3;

procedure CreateBuyList();
var
  f: textFile;
  line: string;
  cNum: integer;
begin
  // создание списка для покупки персов
  var
    chars: TStringDynArray;
  chars := GetCharList();

  cNum := 0;
  for var c in chars do
  begin
    var
      b: CharButton;
    b := CharButton.Create(Form2);
    b.Parent := Form2.BuyPanel;

    b.Height := cButtonScaleY;
    b.Width := cButtonScaleX;

    AssignFile(f, c);
    Reset(f);
    Readln(f, line);
    Readln(f, line);
    b.Text := line;

    Readln(f, line);
    Readln(f, line);
    b.Hint := line;
    b.ShowHint := true;

    Readln(f, line);
    Readln(f, line);
    Readln(f, line);
    Readln(f, line);
    b.cost := StrToInt(line);
    b.Text := b.Text + ' (' + line + ')';
    b.id := cNum;

    CloseFile(f);

    b.Name := 'charButton' + IntToStr(b.id);
    b.OnClick := Form2.TryBuyCharacter;

    b.Position.Y := (cNum div cButtonColumns) * cButtonOffsetY + cButtonStartY;
    b.Position.X := (cNum mod cButtonColumns) * cButtonOffsetX + cButtonStartX;
    Inc(cNum);
  end;
end;

procedure TForm2.ExitToMenu(Sender: TObject);
begin
  DeleteMap();
  ClearPlayerData();
  HideMoveIndicators();
  with Form2 do
  begin
    RightMenu.Visible := false;
    DownMenu.Visible := false;
    MainMenu.Visible := true;
    PrepairLayout.Visible := false;
  end;

  for var item1 in buildPlacers do
    if item1 <> nil then
    begin
      item1.Visible := true;
      item1.txt.Visible := true;
    end;
  WinPanel.Visible := false;
end;

procedure TForm2.MenuExitButtonClick(Sender: TObject);
begin
  Form2.ClickPlayer.CurrentTime := 0;
  Form2.ClickPlayer.Play();

  if wantExit then
  begin
    ExitToMenu(Sender);
  end
  else
  begin
    ExitTimer.Enabled := true;
    ExitBar.Value := 100;
    ExitBar.Visible := true;

    wantExit := true;
  end;
end;

procedure TForm2.ExitTimerTimer(Sender: TObject);
begin
  ExitBar.Value := ExitBar.Value - 1;

  if ExitBar.Value = 0 then
  begin
    ExitTimer.Enabled := false;
    ExitBar.Visible := false;

    wantExit := false;
  end;
end;

procedure TForm2.MPButonClick(Sender: TObject);
begin
  GetCell(selectedCharacter).character.BuyMP();
  Form2.ClickPlayer.CurrentTime := 0;
  Form2.ClickPlayer.Play();
end;

const
  mapPageCount = 4;

procedure SetMapPage(num: integer);
begin
  mapPage := num;

  Form2.LastMapPage.Enabled := num > 0;
  Form2.NextMapPage.Enabled := num < Length(mapList) div mapPageCount;

  for var b in mapList do
    b.Visible := false;

  for var i := mapPageCount * num to mapPageCount * (num + 1) - 1 do
    if i < Length(mapList) then
      mapList[i].Visible := true;
end;

procedure TForm2.LastMapPageClick(Sender: TObject);
begin
  SetMapPage(mapPage - 1);
end;

procedure TForm2.NextMapPageClick(Sender: TObject);
begin
  SetMapPage(mapPage + 1);
end;

const
  mapButtonHeight = 50;

function CreateMapList(): integer;
var
  maps: TStringDynArray;
  line: string;
  f: textFile;
  mapNum: integer;
begin
  with Form2 do
  begin
    maps := GetMapList();
    SetLength(mapList, Length(maps));

    mapNum := 0;

    for var M in maps do
    begin
      var
        b: TButton;
      b := TButton.Create(Form2);
      b.Parent := Form2.MapPanel;
      b.Height := mapButtonHeight;
      b.Width := MapPanel.Width;

      AssignFile(f, ExtractFilePath(ParamStr(0)) + 'Resourses\Maps\' + M
        + '.txt');
      Reset(f);
      Readln(f, line);
      b.Text := line;
      CloseFile(f);

      b.Name := M;
      b.OnClick := Form2.OnChooseMap;

      b.Position.Y := mapButtonHeight * (mapNum mod mapPageCount);
      mapList[mapNum] := b;
      Inc(mapNum);
    end;
    SetMapPage(0);
  end;

  result := mapNum;
end;

const
  buyButtonScaleX = 80;
  buyButtonScaleY = 30;
  buyButtonOffset = 40;
  buyButtonStart = -150;
  buyLabelOffset = 100;

procedure CreateBuyButtons();
var
  f: textFile;
  line: string;
  cNum: integer;
  b: PlacerButton;
  lb: TLabel;
begin
  var
    chars: TStringDynArray;
  chars := GetCharList();

  SetLength(charPlacers, 0);
  cNum := 0;
  for var c in chars do
  begin
    lb := TLabel.Create(Form2);
    lb.Parent := Form2.PlacerPanel;

    lb.Height := buyButtonScaleY;

    b := PlacerButton.Create(Form2);
    b.Parent := Form2.PlacerPanel;

    b.Height := buyButtonScaleY;
    b.Width := buyButtonScaleX;

    AssignFile(f, c);
    Reset(f);
    Readln(f, line);
    Readln(f, line);
    b.Text := line;

    Readln(f, line);
    Readln(f, line);
    b.Hint := line;
    b.ShowHint := true;

    b.isBuilding := false;
    b.id := cNum;

    Readln(f, line);
    Readln(f, line);
    Readln(f, line);
    Readln(f, line);
    b.price := StrToInt(line);

    CloseFile(f);

    b.Name := 'charPlacer' + IntToStr(b.id);
    b.OnClick := Form2.SelectToPlace;

    b.Position.Y := cNum * buyButtonOffset;
    b.Position.X := buyButtonStart;

    lb.Position.Y := b.Position.Y;
    lb.Position.X := b.Position.X + buyLabelOffset;
    Inc(cNum);

    SetLength(charPlacers, Length(charPlacers) + 1);
    charPlacers[Length(charPlacers) - 1] := b;

    b.txt := lb;
  end;

  SetLength(buildPlacers, 0);

  for var i := 0 to Length(defaulBuildingsCount) - 1 do
  begin
    SetLength(buildPlacers, Length(buildPlacers) + 1);
    buildPlacers[Length(buildPlacers) - 1] := nil;

    if defaulBuildingsCount[i] > 0 then
    begin
      lb := TLabel.Create(Form2);
      lb.Parent := Form2.PlacerPanel;

      lb.Height := buyButtonScaleY;

      b := PlacerButton.Create(Form2);
      b.Parent := Form2.PlacerPanel;

      b.Height := buyButtonScaleY;
      b.Width := buyButtonScaleX;

      b.Text := GetBuildingName(i, line);
      b.Hint := line;
      b.ShowHint := true;

      b.isBuilding := true;
      b.id := i;

      b.Name := 'buildingPlacer' + IntToStr(b.id);
      b.OnClick := Form2.SelectToPlace;

      b.Position.Y := cNum * buyButtonOffset;
      b.Position.X := buyButtonStart;

      lb.Position.Y := b.Position.Y;
      lb.Position.X := b.Position.X + buyLabelOffset;
      Inc(cNum);

      b.txt := lb;

      buildPlacers[Length(buildPlacers) - 1] := b;
    end;
  end;
end;

function TryLoadParametrs(): boolean;
var
  f: textFile;
  code1, code2, code3, code4, code5: integer;
  line: string;
begin
  AssignFile(f, ExtractFilePath(ParamStr(0)) +
    'Resourses\Configs\Parametrs.txt');
  Reset(f);

  Readln(f, line);
  Readln(f, line);
  Val(line, money, code1);

  Readln(f, line);
  Readln(f, line);
  Val(line, roundMoney, code2);

  Readln(f, line);
  Readln(f, line);
  Val(line, curseDamageMultiplier, code3);

  Readln(f, line);
  Readln(f, line);
  Val(line, dangerCellDamage, code4);

  Readln(f, line);
  Readln(f, line);
  Val(line, addedMoneyMultiplier, code5);

  CloseFile(f);

  result := not((code1 > 0) or (code2 > 0) or (code3 > 0) or (code4 > 0) or
    (code5 > 0));
end;

procedure TForm2.OpenGame(Sender: TObject);
var
  mapCount, cellCount: integer;
begin
  pressedW := false;
  pressedA := false;
  pressedS := false;
  pressedD := false;
  wantExit := false;

  Randomize();

  if useConsole then
    AllocConsole();

  CharacterManager.Init();

  cellCount := LoadCells();

  if cellCount > 0 then
  begin
    mapCount := CreateMapList();

    if Length(GetCharList()) = 0 then
    begin
      ErrorPanel.Visible := true;
      NoCharError.Visible := true;
    end;

    if mapCount = 0 then
    begin
      ErrorPanel.Visible := true;
      NoMapsError.Visible := true;
    end;

    if IsBuildingFileValid() = false then
    begin
      ErrorPanel.Visible := true;
      NoBuildingsError.Visible := true;
    end;
  end
  else
  begin
    ErrorPanel.Visible := true;
    NoCellError.Visible := true;
  end;

  if not TryLoadParametrs then
  begin
    ErrorPanel.Visible := true;
    NoParametrsError.Visible := true;
  end;

  if ErrorPanel.Visible = false then
  begin
    buildingManager.Init();
    CreateBuyList();
    CreateBuyButtons();
  end;

  InitAudio();
  SettingsManager.Init();
  CharacterDataVisualisator.Init(OP, CharacterPanel);
end;

procedure TForm2.Skill1ButtonClick(Sender: TObject);
begin
  GetCell(selectedCharacter).character.skill1.Select
    (GetCell(selectedCharacter));
  Form2.ClickPlayer.CurrentTime := 0;
  Form2.ClickPlayer.Play();
end;

procedure TForm2.Skill2ButtonClick(Sender: TObject);
begin
  GetCell(selectedCharacter).character.skill2.Select
    (GetCell(selectedCharacter));
  Form2.ClickPlayer.CurrentTime := 0;
  Form2.ClickPlayer.Play();
end;

procedure TForm2.SkipRoundClick(Sender: TObject);
begin
  NextMove();
  Form2.ClickPlayer.CurrentTime := 0;
  Form2.ClickPlayer.Play();
end;

procedure TForm2.SoundBarChange(Sender: TObject);
begin
  ChangeSound(TTrackBar(Sender).Value);
  if SliderPlayer.State <> TMediaState.Playing then
  begin
    SliderPlayer.CurrentTime := 0;
    SliderPlayer.Play();
  end;
end;

procedure TForm2.MusicBarChange(Sender: TObject);
begin
  ChangeMusic(TTrackBar(Sender).Value);
  if SliderPlayer.State <> TMediaState.Playing then
  begin
    SliderPlayer.CurrentTime := 0;
    SliderPlayer.Play();
  end;
end;

procedure TForm2.StartGame(Sender: TObject);

begin
  Form2.ClickPlayer.Play();
  Form2.MainMenu.Visible := false;
  Form2.PrepairLayout.Visible := true;
  Form2.MapPanel.Visible := true;
  Form2.BuyPanel.Visible := false;

  PlayerManager.Init(money, roundMoney);
end;

procedure TForm2.OnChooseMap(Sender: TObject);
begin
  Form2.ClickPlayer.CurrentTime := 0;
  Form2.ClickPlayer.Play();
  Form2.MapPanel.Visible := false;
  Form2.BuyPanel.Visible := true;
  Form2.KeyPressTimer.Enabled := false;

  CellManager.Init(TButton(Sender).Name);
  prepareMode := true;

  BuyMoneyText.Text := IntToStr(currentPlayer.money);
  Form2.BuyRoundSkip.Enabled := false;
end;

procedure TForm2.TryBuyCharacter(Sender: TObject);
var
  b: CharButton;
begin

  b := CharButton(Sender);
  if currentPlayer.money >= b.cost then
  begin
    currentPlayer.money := currentPlayer.money - b.cost;
    BuyMoneyText.Text := IntToStr(currentPlayer.money);

    if Length(currentPlayer.boughtCharacters) <= b.id then
    begin
      SetLength(currentPlayer.boughtCharacters, b.id + 1);
      currentPlayer.boughtCharacters[b.id] := 0;
    end;
    Inc(currentPlayer.boughtCharacters[b.id]);
    Form2.BuyRoundSkip.Enabled := true;

    Form2.BuyPlayer.CurrentTime := 0;
    Form2.BuyPlayer.Play();
  end
  else
  begin
    Form2.ClickPlayer.CurrentTime := 0;
    Form2.ClickPlayer.Play();
  end;
end;

procedure TForm2.TryLoopAudio(Sender: TObject);
var
  Files: TStringDynArray;
  RandomIndex: integer;
begin
  if MusicPlayer.CurrentTime >= MusicPlayer.Duration then
  begin
    Files := TDirectory.GetFiles(ExtractFilePath(ParamStr(0)) +
      'Resourses\Audio\Music\');
    RandomIndex := Random(Length(Files));

    MusicPlayer.FileName := Files[RandomIndex];
    MusicPlayer.Play();
  end;

end;

procedure TForm2.AttackButtonClick(Sender: TObject);
begin
  Form2.ClickPlayer.CurrentTime := 0;
  Form2.ClickPlayer.Play();
  GetCell(selectedCharacter).character.atack.Select(GetCell(selectedCharacter));
end;

procedure TForm2.SelectToPlace(Sender: TObject);
var
  b: PlacerButton;
begin
  Form2.ClickPlayer.CurrentTime := 0;
  Form2.ClickPlayer.Play();
  b := PlacerButton(Sender);
  if prepareMode then
  begin
    if (placableCharacterId = -1) and (placableBuildingId = -1) then
    begin
      if b.isBuilding then
      begin
        if currentPlayer.BuildingsCount[b.id] > 0 then
        begin
          placableBuildingId := b.id;
          Dec(currentPlayer.BuildingsCount[b.id]);
        end;
      end
      else
      begin
        if (Length(currentPlayer.boughtCharacters) >= b.id + 1) and
          (currentPlayer.boughtCharacters[b.id] > 0) then
        begin
          placableCharacterId := b.id;
          Dec(currentPlayer.boughtCharacters[b.id]);
        end;
      end;
    end;
    ShowPlacersCount(currentPlayer);
  end
  else
  begin
    if (GetActionCount() > 0) and (currentPlayer.money >= b.price) and
      (currentPlayer.portalCell.building <> nil) and
      (currentPlayer.portalCell.character = nil) then
    begin
      Form2.BuyPlayer.CurrentTime := 0;
      Form2.BuyPlayer.Play();
      Form2.PortalPlayer.CurrentTime := 0;
      Form2.PortalPlayer.Play();
      currentPlayer.money := currentPlayer.money - b.price;
      placableCharacterId := b.id;
      SetActionCount(GetActionCount() - 1);
      TryCreateCharacter(currentPlayer.portalCell);
    end;
  end;
end;

procedure TForm2.BuyRoundSkipClick(Sender: TObject);
begin
  Form2.ClickPlayer.CurrentTime := 0;
  Form2.ClickPlayer.Play();
  curPlayer := 1 - curPlayer;
  currentPlayer := players[curPlayer];
  if curPlayer = 1 then
  begin
    BuyMoneyText.Text := IntToStr(currentPlayer.money);
    Form2.BuyRoundSkip.Enabled := false;
  end
  else
  begin
    Form2.DownMenu.Visible := true;
    Form2.RightMenu.Visible := true;
    Form2.KeyPressTimer.Enabled := true;

    Form2.SkipRound.Enabled := false;
    Form2.PrepairLayout.Visible := false;

    ShowPlacersCount(players[0]);
  end;
end;

procedure TForm2.DeleteAnimation(Sender: TObject);
var
  c: TCharacter;
begin
  c := TCharacterAnimation(Sender).c;

  if TCharacterAnimation(Sender).removeFromQueue then
  begin
    c.AnimationQueue.Dequeue;
    c.IsAnimating := false;
    AnimateMoving(c);
  end;
  Sender.Free;
end;

procedure PlacerButton.SetCount(c: integer);
begin
  _count := c;
  txt.Text := IntToStr(c);
end;

procedure ShowPlacersCount(player: TPlayer);
begin
  for var i := 0 to Length(charPlacers) - 1 do
  begin
    if i < Length(player.boughtCharacters) then
      charPlacers[i].SetCount(player.boughtCharacters[i])
    else
      charPlacers[i].SetCount(0);
  end;

  for var i := 0 to Length(buildPlacers) - 1 do
  begin
    if buildPlacers[i] <> nil then
      buildPlacers[i].SetCount(player.BuildingsCount[i]);
  end;

end;

end.
