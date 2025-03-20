unit DiceManager;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, DataTypes;

const
  DicesTypesCount = 5;

type
  DiceData = Record
    sprite: string;
    minValue, maxValue: integer;
  End;

  DiceVisualisator = Record
    image: TImage;
    text: TLabel;
  End;

  DicesCount = array [0 .. (DicesTypesCount - 1)] of integer;

procedure ClearVisualisation();
function DropDices(dices: DicesCount): integer;

implementation

uses CellManager, Window, Drawer;

const
  DiceSize = 60;
  DiceTypes: array [0 .. (DicesTypesCount - 1)
    ] of DiceData = ((sprite: 'd1.png'; minValue: 0; maxValue: 1),
    (sprite: 'd4.png'; minValue: 1; maxValue: 4), (sprite: 'd6.png';
    minValue: 1; maxValue: 6), (sprite: 'd8.png'; minValue: 1; maxValue: 8),
    (sprite: 'd20.png'; minValue: 1; maxValue: 20));

  firstDiceX = 1000;
  firstDiceY = 500;
  deltaDicePos = 100;
  diceRowCount = 3;

var
  spawnedCubes: array of DiceVisualisator;

procedure ClearVisualisation();
begin
  for var i := 0 to Length(spawnedCubes) - 1 do
  begin
    spawnedCubes[i].image.Free;
    spawnedCubes[i].text.Free;
  end;
  SetLength(spawnedCubes, 0);;
end;

function DrawDice(sprite: string; val: integer; pos: Vector2): DiceVisualisator;
var
  MyImage: TImage;
  MyText: TLabel;
begin
  MyImage := TImage.Create(Form2);
  MyImage.Parent := Form2;

  MyImage.Position.X := pos.X;
  MyImage.Position.Y := pos.Y;

  MyImage.Width := DiceSize;
  MyImage.Height := DiceSize;

  MyText := TLabel.Create(Form2);
  MyText.Parent := Form2;

  MyImage.Bitmap.LoadFromFile(ExtractFilePath(ParamStr(0)) +
    'Resourses\Sprites\' + sprite);
  RandomColorizeImage(MyImage);

  MyText.Position.X := pos.X;
  MyText.Position.Y := pos.Y;

  MyText.text := IntToStr(val);
  MyText.TextSettings.HorzAlign := TTextAlign.Center;
  MyText.TextSettings.VertAlign := TTextAlign.Center;
  MyText.Width := DiceSize;
  MyText.Height := DiceSize;

  result.image := MyImage;
  result.text := MyText;
end;

function DropDices(dices: DicesCount): integer;
var
  cur, sum, i, num: integer;
  curPos: Vector2;
begin
  curPos.X := firstDiceX;
  curPos.Y := firstDiceY;
  sum := 0;
  num := 1;

  ClearVisualisation();

  while (dices[0] > 0) or (dices[1] > 0) or (dices[2] > 0) or (dices[3] > 0) or
    (dices[4] > 0) do
  begin
    i := random(4);
    if dices[i] > 0 then
    begin
      cur := DiceTypes[i].minValue +
        random(DiceTypes[i].maxValue - DiceTypes[i].minValue);
      Inc(sum, cur);

      SetLength(spawnedCubes, num + 1);
      spawnedCubes[num] := DrawDice(DiceTypes[i].sprite, cur, curPos);
      dec(dices[i]);

      if num mod diceRowCount = 0 then
      begin
        Inc(curPos.Y, deltaDicePos);
        curPos.X := firstDiceX;
      end
      else
      begin
        Inc(curPos.X, deltaDicePos);
      end;
      Inc(num);
    end;
  end;

  result := sum;
end;

end.
