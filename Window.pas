unit Window;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Objects;

type
  TForm2 = class(TForm)
    OPLabel: TLabel;
    OP: TLabel;
    CharacterPanel: TImage;
    procedure OpenGame(Sender: TObject);
  private
    { Private declarations }
  public
    procedure DeleteAnimation(Sender: TObject);
  end;

var
  Form2: TForm2;

implementation

uses CellManager, DiceManager, Winapi.Windows, CharacterDataVisualisator;
{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

const
  testDices : DicesCount = (1,1,1,1,1);
  useConsole = true;

procedure TForm2.OpenGame(Sender: TObject);
begin
  Randomize();
  if useConsole then
      AllocConsole();

  CharacterDataVisualisator.Init(op, CharacterPanel);
  CellManager.Init('test');
  DropDices(testDices);
end;

procedure TForm2.DeleteAnimation(Sender: TObject);
begin
  Sender.Free;
end;

end.
