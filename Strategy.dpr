program Strategy;

uses
  System.StartUpCopy,
  FMX.Forms,
  Window in 'Window.pas' {Form2},
  CellManager in 'CellManager.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
