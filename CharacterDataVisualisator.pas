unit CharacterDataVisualisator;

interface

uses
  System.SysUtils, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics,
  FMX.StdCtrls, FMX.Objects, DataTypes;

procedure Init(_opText: TLabel; pan: TImage);

procedure SetCaharcter(c: TCharacter);

procedure ReDraw();

function GetCubeText(cubes: DicesCount): string;

implementation

uses CellManager, CharacterManager, Window;

var
  panel: TImage;
  curCharacter: TCharacter;
  moveIndicators: array [0 .. 7] of TImage;

const
  neignourPositions: array [0 .. 7] of Vector2 = ((x: 1; y: 0), (x: 1; y: 1),
    (x: 0; y: 1), (x: - 1; y: 1), (x: - 1; y: 0), (x: - 1; y: - 1), (x: 0;
    y: - 1), (x: 1; y: - 1));

  moveIndicator = 'd1.png';
  moveIndicatorSize = 30;
  moveIndicatorOffset = 50;

procedure Init(_opText: TLabel; pan: TImage);
var
  MyImage: TImage;
begin
  panel := pan;
  panel.Visible := false;

  for var i := 0 to 7 do
  begin
    MyImage := TImage.Create(pan.Parent);
    MyImage.Parent := form2.Map;

    MyImage.Width := moveIndicatorSize;
    MyImage.Height := moveIndicatorSize;
    MyImage.Bitmap.LoadFromFile(ExtractFilePath(ParamStr(0)) +
      'Resourses\Sprites\' + moveIndicator);
    moveIndicators[i] := MyImage;
    MyImage.BringToFront();
    MyImage.Visible := false;
    MyImage.HitTest := false;
  end;
end;

procedure DrawMoveIndicators();
var
  current: Vector2;
begin
  for var i := 0 to 7 do
  begin
    current.x := curCharacter.pos.x + neignourPositions[i].x;
    current.y := curCharacter.pos.y + neignourPositions[i].y;
    if GetCell(current) = nil then
      moveIndicators[i].Visible := false
    else
    begin
      moveIndicators[i].BringToFront();
      moveIndicators[i].Position.x := GetCell(current).Image.Position.x +
        moveIndicatorOffset;
      moveIndicators[i].Position.y := GetCell(current).Image.Position.y +
        moveIndicatorOffset;
      moveIndicators[i].Visible := IsCorrectDest(curCharacter,
        GetCell(current));;
    end;
  end;
end;

procedure HideMoveIndicators();
begin
  for var i := 0 to 7 do
    moveIndicators[i].Visible := false;
end;

procedure SetCaharcter(c: TCharacter);
begin
  curCharacter := c;
  ReDraw();
end;

procedure ReDraw();
begin
  if curCharacter = nil then
  begin
    panel.Visible := false;
    form2.PlacerPanel.Visible := true;
    HideMoveIndicators();
  end
  else
  begin
    form2.OP.Text := IntToStr(curCharacter.movePoints);
    form2.Hp.Text := IntToStr(curCharacter.Hp) + '/' +
      IntToStr(curCharacter.maxHP);
    form2.Speed.Text := IntToStr(curCharacter.Speed);
    form2.Armor.Text := IntToStr(curCharacter.Armor);
    form2.BonusDices.Text := GetCubeText(curCharacter.bonusDices);

    panel.Visible := true;
    form2.PlacerPanel.Visible := false;
    DrawMoveIndicators();
    with form2, curCharacter do
    begin
      attackButton.Enabled := atack.timeAfterUse >= atack.reloadTime;
      skill1Button.Enabled := skill1.timeAfterUse >= skill1.reloadTime;
      skill2Button.Enabled := skill2.timeAfterUse >= skill2.reloadTime;

      attackButton.Text := atack.name;
      skill1Button.Text := skill1.name;
      skill2Button.Text := skill2.name;

      attackButton.Hint := atack.GetToolTip();
      skill1Button.Hint := skill1.GetToolTip();
      skill2Button.Hint := skill2.GetToolTip();
    end;
  end;
end;

function GetCubeText(cubes: DicesCount): string;
begin
  result := '';

  if cubes[0] > 0 then
    result := result + IntToStr(cubes[0]) + 'Ê1';

  if cubes[1] > 0 then
  begin
    if Length(result) > 0 then
      result := result + ', ';
    result := result + IntToStr(cubes[1]) + 'Ê4';
  end;

  if cubes[2] > 0 then
  begin
    if Length(result) > 0 then
      result := result + ', ';
    result := result + IntToStr(cubes[2]) + 'Ê6';
  end;

  if cubes[3] > 0 then
  begin
    if Length(result) > 0 then
      result := result + ', ';
    result := result + IntToStr(cubes[3]) + 'Ê8';
  end;

  if cubes[4] > 0 then
  begin
    if Length(result) > 0 then
      result := result + ', ';
    result := result + IntToStr(cubes[4]) + 'Ê20';
  end;

  if result = '' then
    result := '-'
  else
    result := '(' + result + ')';
end;

end.
