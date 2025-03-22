unit Window;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Objects, FMX.Layouts;

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
    procedure OpenGame(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState);
    procedure KeyPressTimerTimer(Sender: TObject);
    procedure SkipRoundClick(Sender: TObject);
    procedure MPButonClick(Sender: TObject);
    procedure AttackButtonClick(Sender: TObject);
    procedure Skill1ButtonClick(Sender: TObject);
    procedure Skill2ButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    { Private declarations }
  public
    procedure DeleteAnimation(Sender: TObject);
  end;

var
  Form2: TForm2;
  minMapX, minMapY : integer;

implementation

uses CellManager, Winapi.Windows, CharacterDataVisualisator, DataTypes, CharacterManager, PlayerManager, buildingManager;
{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

const
  useConsole = true;

  mapMovingSpeed = 2;     //движение карты

var pressedA, pressedW, pressedD,pressedS : boolean;

procedure TForm2.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  if Key = vkEscape then
  begin
    if TargetSelectionMode then
    begin
      targetSelectionMode := false;
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
    'W', 'w', 'Ц', 'ц': pressedW := true;
    'A', 'a', 'Ф', 'ф': pressedA := true;
    'S', 's', 'Ы', 'ы': pressedS := true;
    'D', 'd', 'В', 'в': pressedD := true;
  end;
end;

procedure TForm2.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  case KeyChar of
    'W', 'w', 'Ц', 'ц': pressedW := false;
    'A', 'a', 'Ф', 'ф': pressedA := false;
    'S', 's', 'Ы', 'ы': pressedS := false;
    'D', 'd', 'В', 'в': pressedD := false;
  end;
end;

procedure TForm2.FormResize(Sender: TObject);
begin
  RightMenu.Height := form2.Height;
  BG.Height := form2.Height;
end;

procedure TForm2.KeyPressTimerTimer(Sender: TObject);
begin
  if pressedW then
    Map.Position.Y := Map.Position.Y + mapMovingSpeed;
  if pressedS then
    Map.Position.Y := Map.Position.Y - mapMovingSpeed;
  if pressedA then
    Map.Position.X := Map.Position.X + mapMovingSpeed;
  if pressedD then
    Map.Position.X := Map.Position.X - mapMovingSpeed;

  if Map.Position.X  > 0 then
    Map.Position.X := 0;
  if Map.Position.Y  > 0 then
    Map.Position.Y := 0;

  if Map.Position.X  < minMapX then
    Map.Position.X := minMapX;
  if Map.Position.Y  < minMapY then
    Map.Position.Y := minMapY;
end;

procedure TForm2.MPButonClick(Sender: TObject);
begin
  GetCell(selectedCharacter).character.BuyMP();
end;

procedure TForm2.OpenGame(Sender: TObject);
var f : textFile;
money, roundMoney : integer;
line : string;
begin
  pressedW := false;
  pressedA := false;
  pressedS := false;
  pressedD := false;

  Randomize();

  if useConsole then
      AllocConsole();
  AssignFile(f, ExtractFilePath(ParamStr(0)) + 'Resourses\Configs\Parametrs.txt');
  Reset(f);

  Readln(f, line);
  Readln(f, line);
  money := StrToInt(line);

  Readln(f, line);
  Readln(f, line);
  roundMoney := StrToInt(line);

  CloseFile(f);
  PlayerManager.Init(money, roundMoney);
  CharacterManager.Init();
  buildingManager.Init();
  CharacterDataVisualisator.Init(op, CharacterPanel);
  CellManager.Init('test');
end;

procedure TForm2.Skill1ButtonClick(Sender: TObject);
begin
  GetCell(selectedCharacter).character.skill1.Select(GetCell(selectedCharacter));
end;

procedure TForm2.Skill2ButtonClick(Sender: TObject);
begin
  GetCell(selectedCharacter).character.skill2.Select(GetCell(selectedCharacter));
end;

procedure TForm2.SkipRoundClick(Sender: TObject);
begin
  NextMove();
end;

procedure TForm2.AttackButtonClick(Sender: TObject);
begin
  GetCell(selectedCharacter).character.atack.Select(GetCell(selectedCharacter));
end;

procedure TForm2.DeleteAnimation(Sender: TObject);
begin
  Sender.Free;
end;

end.
