program Strategy;

uses
  System.StartUpCopy,
  FMX.Forms,
  Window in 'Window.pas' {Form2},
  CellManager in 'CellManager.pas',
  DiceManager in 'DiceManager.pas',
  CharacterManager in 'CharacterManager.pas',
  DataTypes in 'DataTypes.pas',
  Drawer in 'Drawer.pas',
  PlayerManager in 'PlayerManager.pas',
  CharacterDataVisualisator in 'CharacterDataVisualisator.pas',
  SkillManager in 'SkillManager.pas',
  buildingManager in 'buildingManager.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
