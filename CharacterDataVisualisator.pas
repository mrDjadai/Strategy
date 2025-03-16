unit CharacterDataVisualisator;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Objects, DataTypes;

procedure Init(_opText : TLabel; pan : TImage);

procedure SetCaharcter(c : TCharacter);

procedure ReDraw();

implementation

uses CellManager, CharacterManager, Window;

var
  panel : TImage;
  opText : TLabel;
  curCharacter : TCharacter;
  moveIndicators : array[0..7] of TImage;

const
  neignourPositions : array[0..7] of Vector2 =(
  (x : 1; y : 0),
  (x : 1; y : 1),
  (x : 0; y : 1),
  (x : -1; y : 1),
  (x : -1; y : 0),
  (x : -1; y : -1),
  (x : 0; y : -1),
  (x : 1; y : -1));

  moveIndicator = 'd1.png';
  moveIndicatorSize = 30;
  moveIndicatorOffset = 50;

procedure Init(_opText : TLabel; pan : TImage);
var MyImage : TImage;
begin
  panel := pan;
  opText := _opText;
  panel.Visible := false;

  for var i := 0 to 7 do
  begin
    MyImage := TImage.Create(pan.Parent);
    MyImage.Parent := form2.Map;

    MyImage.Width := moveIndicatorSize;
    MyImage.Height := moveIndicatorSize;
    MyImage.Bitmap.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Resourses\Sprites\' + moveIndicator);
    moveIndicators[i] := MyImage;
    myImage.BringToFront();
    MyImage.Visible := false;
    MyImage.HitTest := false;
  end;
end;

procedure DrawMoveIndicators();
var current : vector2;
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
      moveIndicators[i].Position.X := GetCell(current).Image.Position.X + moveIndicatorOffset;
      moveIndicators[i].Position.Y := GetCell(current).Image.Position.Y + moveIndicatorOffset;
      moveIndicators[i].Visible := IsCorrectDest(curCharacter, GetCell(current));;
    end;
  end;
end;

procedure HideMoveIndicators();
begin
  for var i := 0 to 7 do
    moveIndicators[i].Visible := false;
end;


procedure SetCaharcter(c : TCharacter);
begin
  curCharacter := c;
  ReDraw();
end;


procedure ReDraw();
begin
  if curCharacter = nil then
  begin
    panel.Visible := false;
    HideMoveIndicators();
  end
  else
  begin
    opText.Text := IntToStr(curCharacter.movePoints);
    panel.Visible := true;
    DrawMoveIndicators();
  end;
end;
end.
