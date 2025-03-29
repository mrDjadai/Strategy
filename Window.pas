unit Window;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Objects, FMX.Layouts, DataTypes,
  FMX.Media;

type
  TForm2 = class(TForm)
    OPLabel: TLabel;
    OP: TLabel;
    CharacterPanel: TImage;
    Map: TLayout;
    KeyPressTimer: TTimer;
    RightMenu: TLayout;
    BG: TPanel;
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
    MapPanel: TPanel;
    BuyPanel: TPanel;
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
    procedure FormResize(Sender: TObject);
    procedure StartGame(Sender: TObject);
    procedure BuyRoundSkipClick(Sender: TObject);
    procedure TryLoopAudio(Sender: TObject);
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
    function GetCount(): integer;
  public
  var
    txt: TLabel;
    isBuilding: boolean;
    id: integer;
    price: integer;
    property Count: integer read GetCount write SetCount;
  end;

var
  Form2: TForm2;
  minMapX, minMapY: integer;
  charPlacers: Array of PlacerButton;
  buildPlacers: Array of PlacerButton;

procedure ShowPlacersCount(player: TPlayer);

implementation

uses CellManager, Winapi.Windows, CharacterDataVisualisator,
  CharacterManager, PlayerManager, buildingManager, System.IOUtils;
{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

const
  useConsole = true;

  mapMovingSpeed = 2; // движение карты

var
  pressedA, pressedW, pressedD, pressedS: boolean;

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

procedure TForm2.FormResize(Sender: TObject);
begin
  RightMenu.Height := Form2.Height;
  BG.Height := Form2.Height;
end;

procedure TForm2.CheckPressKey(Sender: TObject);
begin
  if pressedW then
    Map.Position.Y := Map.Position.Y + mapMovingSpeed;
  if pressedS then
    Map.Position.Y := Map.Position.Y - mapMovingSpeed;
  if pressedA then
    Map.Position.X := Map.Position.X + mapMovingSpeed;
  if pressedD then
    Map.Position.X := Map.Position.X - mapMovingSpeed;

  if Map.Position.X > 0 then
    Map.Position.X := 0;
  if Map.Position.Y > 0 then
    Map.Position.Y := 0;

  if Map.Position.X < minMapX then
    Map.Position.X := minMapX;
  if Map.Position.Y < minMapY then
    Map.Position.Y := minMapY;
end;

procedure TForm2.MPButonClick(Sender: TObject);
begin
  GetCell(selectedCharacter).character.BuyMP();
end;

procedure TForm2.OpenGame(Sender: TObject);
begin
  pressedW := false;
  pressedA := false;
  pressedS := false;
  pressedD := false;

  Randomize();

  if useConsole then
    AllocConsole();
end;

procedure TForm2.Skill1ButtonClick(Sender: TObject);
begin
  GetCell(selectedCharacter).character.skill1.Select
    (GetCell(selectedCharacter));
end;

procedure TForm2.Skill2ButtonClick(Sender: TObject);
begin
  GetCell(selectedCharacter).character.skill2.Select
    (GetCell(selectedCharacter));
end;

procedure TForm2.SkipRoundClick(Sender: TObject);
begin
  NextMove();
end;

const
  mapButtonHeight = 20;

procedure TForm2.StartGame(Sender: TObject);
var
  f: textFile;
  money, roundMoney: integer;
  maps: TStringDynArray;
  line: string;
  mapNum: integer;
begin
  Form2.StartButton.Visible := false;
  Form2.PrepairLayout.Visible := true;
  Form2.BuyPanel.Visible := false;

  AssignFile(f, ExtractFilePath(ParamStr(0)) +
    'Resourses\Configs\Parametrs.txt');
  Reset(f);

  Readln(f, line);
  Readln(f, line);
  money := StrToInt(line);

  Readln(f, line);
  Readln(f, line);
  roundMoney := StrToInt(line);

  CloseFile(f);

  CharacterManager.Init();
  buildingManager.Init();
  PlayerManager.Init(money, roundMoney);
  CharacterDataVisualisator.Init(OP, CharacterPanel);

  maps := GetMapList();
  mapNum := 0;

  for var Map in maps do
  begin
    var
      b: TButton;
    b := TButton.Create(Form2);
    b.Parent := Form2.MapPanel;
    b.Height := mapButtonHeight;
    b.Width := MapPanel.Width;

    AssignFile(f, ExtractFilePath(ParamStr(0)) + 'Resourses\Maps\' + Map
      + '.txt');
    Reset(f);
    Readln(f, line);
    b.Text := line;
    CloseFile(f);

    b.Name := Map;
    b.OnClick := Form2.OnChooseMap;

    b.Position.Y := mapNum * mapButtonHeight;
    Inc(mapNum);
  end;
end;

const
  cButtonScaleX = 80;
  cButtonScaleY = 15;
  cButtonOffsetX = 40;
  cButtonOffsetY = 15;
  cButtonColumns = 2;

procedure TForm2.OnChooseMap(Sender: TObject);
var
  f: textFile;
  line: string;
  cNum: integer;
begin
  Form2.MapPanel.Visible := false;
  Form2.BuyPanel.Visible := true;
  Form2.KeyPressTimer.Enabled := false;

  CellManager.Init(TButton(Sender).Name);
  prepareMode := true;

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
    b.Text := line;

    Readln(f, line);
    Readln(f, line);
    b.cost := StrToInt(line);
    b.Text := b.Text + ' (' + line + ')';
    b.id := cNum;

    CloseFile(f);

    b.Name := 'charButton' + IntToStr(b.id);
    b.OnClick := Form2.TryBuyCharacter;

    b.Position.Y := (cNum div cButtonColumns) * cButtonOffsetY;
    b.Position.X := (cNum mod cButtonColumns) * cButtonOffsetX;
    Inc(cNum);
  end;
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

    musicPlayer.FileName := Files[RandomIndex];
    MusicPlayer.Play();
  end;

end;

procedure TForm2.AttackButtonClick(Sender: TObject);
begin
  GetCell(selectedCharacter).character.atack.Select(GetCell(selectedCharacter));
end;

const
  buyButtonScaleX = 80;
  buyButtonScaleY = 30;
  buyButtonOffset = 40;
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
    b.Text := line;
    b.isBuilding := false;
    b.id := cNum;

    Readln(f, line);
    Readln(f, line);
    b.price := StrToInt(line);

    CloseFile(f);

    b.Name := 'charPlacer' + IntToStr(b.id);
    b.OnClick := Form2.SelectToPlace;

    b.Position.Y := cNum * buyButtonOffset;
    b.Position.X := 0;

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

      b.Text := GetBuildingName(i);
      b.isBuilding := true;
      b.id := i;

      b.Name := 'buildingPlacer' + IntToStr(b.id);
      b.OnClick := Form2.SelectToPlace;

      b.Position.Y := cNum * buyButtonOffset;
      b.Position.X := 0;

      lb.Position.Y := b.Position.Y;
      lb.Position.X := b.Position.X + buyLabelOffset;
      Inc(cNum);

      b.txt := lb;

      buildPlacers[Length(buildPlacers) - 1] := b;
    end;
  end;

  ShowPlacersCount(players[0]);
end;

procedure TForm2.SelectToPlace(Sender: TObject);
var
  b: PlacerButton;
begin
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
      currentPlayer.money := currentPlayer.money - b.price;
      placableCharacterId := b.id;
      SetActionCount(GetActionCount() - 1);
      TryCreateCharacter(currentPlayer.portalCell);
    end;
  end;
end;

procedure TForm2.BuyRoundSkipClick(Sender: TObject);
begin
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

    CreateBuyButtons();
  end;
end;

procedure TForm2.DeleteAnimation(Sender: TObject);
begin
  Sender.Free;
end;

procedure PlacerButton.SetCount(c: integer);
begin
  _count := c;
  txt.Text := IntToStr(c);
end;

function PlacerButton.GetCount: integer;
begin
  Result := _count;
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
