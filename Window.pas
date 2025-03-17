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
    OPButton: TButton;
    procedure OpenGame(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar;
      Shift: TShiftState);
    procedure KeyPressTimerTimer(Sender: TObject);
    procedure SkipRoundClick(Sender: TObject);
    procedure OPButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure DeleteAnimation(Sender: TObject);
  end;

var
  Form2: TForm2;

implementation

uses CellManager, DiceManager, Winapi.Windows, CharacterDataVisualisator, DataTypes, CharacterManager, PlayerManager;
{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

const
  testDices : DicesCount = (1,1,1,1,1);
  useConsole = true;

  mapMovingSpeed = 2;

var pressedA, pressedW, pressedD,pressedS : boolean;

procedure TForm2.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  case KeyChar of
    'W', 'w', 'Ö', 'ö': pressedW := true;
    'A', 'a', 'Ô', 'ô': pressedA := true;
    'S', 's', 'Û', 'û': pressedS := true;
    'D', 'd', 'Â', 'â': pressedD := true;
  end;
end;

procedure TForm2.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: WideChar; Shift: TShiftState);
begin
  case KeyChar of
    'W', 'w', 'Ö', 'ö': pressedW := false;
    'A', 'a', 'Ô', 'ô': pressedA := false;
    'S', 's', 'Û', 'û': pressedS := false;
    'D', 'd', 'Â', 'â': pressedD := false;
  end;
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
end;

procedure TForm2.OPButtonClick(Sender: TObject);
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

  PlayerManager.Init();
  CharacterManager.Init();
  CharacterDataVisualisator.Init(op, CharacterPanel);
  CellManager.Init('test');
  DropDices(testDices);
end;

procedure TForm2.SkipRoundClick(Sender: TObject);
begin
  NextMove();
end;

procedure TForm2.DeleteAnimation(Sender: TObject);
begin
  Sender.Free;
end;

end.
