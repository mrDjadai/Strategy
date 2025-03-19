unit DiceManager;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, FMX.StdCtrls, DataTypes;

const DicesTypesCount = 5;

type
DiceData = Record
  sprite : string;
  minValue, maxValue : integer;
End;

DiceVisualisator= Record
    image : TImage;
    text : TLabel;
End;

DicesCount = array[0..(DicesTypesCount-1)] of integer;

procedure ClearVisualisation();
function DropDices(dices: DicesCount) : integer;

implementation

uses CellManager, Window, Drawer;

const
  DiceSize = 60;
  DiceTypes : array[0..(DicesTypesCount-1)] of DiceData = (
  (sprite : 'd1.png'; minValue : 0; maxValue : 1),
  (sprite : 'd4.png'; minValue : 1; maxValue : 4),
  (sprite : 'd6.png'; minValue : 1; maxValue : 6),
  (sprite : 'd8.png'; minValue : 1; maxValue : 8),
  (sprite : 'd20.png'; minValue : 1; maxValue : 20)
  );

  Places : array[0..5] of Vector2 = (
  (x : 1000; y : 500),
  (x : 1100; y : 500),
  (x : 1200; y : 500),
  (x : 1000; y : 600),
  (x : 1100; y : 600),
  (x : 1200; y : 600)
  );

var spawnedCubes : array of DiceVisualisator;

procedure ClearVisualisation();
begin
  for var i  := 0 to Length(spawnedCubes) - 1 do
  begin
    spawnedCubes[i].image.Free;
    spawnedCubes[i].text.Free;
  end;
  SetLength(spawnedCubes, 0);;
end;


function DrawDice(sprite : string; val : integer; pos : Vector2) : DiceVisualisator;
var MyImage : TImage;
    MyText : TLabel;
begin
  MyImage := TImage.Create(Form2);
  MyImage.Parent := Form2;

  MyImage.Position.X := pos.x;
  MyImage.Position.Y := pos.y;


  MyImage.Width := DiceSize;
  MyImage.Height := DiceSize;

  MyText := TLabel.Create(Form2);
  MyText.Parent := Form2;

  MyImage.Bitmap.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Resourses\Sprites\' + sprite);
  RandomColorizeImage(MyImage);

  MyText.Position.X := pos.x;
  MyText.Position.Y := pos.y;

  MyText.Text := IntToStr(val);
  MyText.TextSettings.HorzAlign := TTextAlign.Center;
  MyText.TextSettings.VertAlign := TTextAlign.Center;
  MyText.Width := DiceSize;
  MyText.Height := DiceSize;

  result.image := MyImage;
  result.text := MyText;
end;

function DropDices(dices: DicesCount) : integer;
var curPos, cur, sum, i : integer;
begin
  curPos := 0;
  sum := 0;

  ClearVisualisation();

  while (dices[0] > 0) or (dices[1] > 0) or (dices[2] > 0) or (dices[3] > 0) or (dices[4] > 0)  do
  begin
    i := random(4);
   if dices[i] > 0 then
    begin
      cur := DiceTypes[i].minValue +  random(DiceTypes[i].maxValue - DiceTypes[i].minValue);
      Inc(sum, cur);

        SetLength(spawnedCubes, curPos + 1);
        spawnedCubes[curPos] := DrawDice(DiceTypes[i].sprite, cur, places[curPos]);
        dec(dices[i]);
      Inc(curPos);
    end;
  end;

    result := sum;
end;

end.
