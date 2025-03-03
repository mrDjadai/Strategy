unit Window;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani;

type
  TForm2 = class(TForm)
    procedure OpenGame(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses CellManager, DiceManager;
{$R *.fmx}

const
  testDices : DicesCount = (1,1,1,1,1);
procedure TForm2.OpenGame(Sender: TObject);
begin
  Randomize();
  CellManager.Init('test');
  DropDices(testDices);
end;

end.
