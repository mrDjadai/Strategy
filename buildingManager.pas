unit buildingManager;

interface

uses DataTypes;

procedure Init();

function TryBuild(cell : TCellData) : boolean;

implementation

uses PlayerManager, cellManager, System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Objects, FMX.Layouts;

const buildingOffset : vector2 = (x : -10; y : -12);

type
  Wall = class(TBuilding)
    public
      procedure OnBuild(c: TCellData; ow: integer); override;
      procedure OnDemolish(); override;
  end;

  Hospital = class(TBuilding)
    public
      procedure OnStay(); override;
    private
      healAmount : integer;
  end;
var
  buildingCount : integer;

function CreateBuilding(id : integer) : TBuilding;
var f : TextFile;
line, data1, data2 : string;
begin
  case id of
  0 : result := Wall.Create;
  1 : result := Hospital.Create;
  end;

  AssignFile(f, ExtractFilePath(ParamStr(0)) + 'Resourses\Configs\BuildingsData.txt');
  Reset(f);
  for var I := 0 to Id  - 1 do
    for var j := 1 to 6 do
      Readln(f, line);

    Readln(f, line);
    result.name := line;

    Readln(f, line);
    result.sprite := line;

    Readln(f, line);
    result.maxHp := StrToInt(line);

    Readln(f, line);
    result.cost := StrToInt(line);

    Readln(f, data1);
    Readln(f, data2);

    case id of
    0: ;
    1: Hospital(result).healAmount := StrToInt(data1);
    end;
    CloseFile(f);
end;

procedure Build(cell : TCellData; ID : integer);
begin
var b : TBuilding;
    b := CreateBuilding(ID);

    b.pos := cell.decardPos;

    var myImage : TImage;
    myImage := TImage.Create(TComponent(cell.Image).Owner);
    myImage.Parent := TControl(cell.Image).Parent;

    MyImage.Position.x := cell.Image.Position.x + buildingOffset.x;
    MyImage.Position.y := cell.Image.Position.y + buildingOffset.y;
    MyImage.Height := cell.Image.Height;
    MyImage.Width := cell.Image.Width;

    MyImage.HitTest := false;
    b.img := myImage;

    MyImage.BringToFront();


    b.OnBuild(cell, curPlayer);

    cell.ReDraw();
end;

function TryBuild(cell : TCellData) : boolean;
begin
  result := false;

  if cell.cType <> cBlocked then
  begin
    Build(cell, 1);
    result := true;
  end;
end;

procedure Init();
var f : TextFile;
line : string;
i : integer;
begin
  buildingCount := 0;
  AssignFile(f, ExtractFilePath(ParamStr(0)) + 'Resourses\Configs\BuildingsData.txt');
  Reset(f);
  i := 1;

  while (not EOF(f)) do
  begin
    readln(f, line);
    Inc(i);
    if i mod 5 = 0 then
      Inc(buildingCount);
  end;

  CloseFile(f);
end;

procedure Wall.OnBuild(c: TCellData; ow: Integer);
begin
  inherited OnBuild(c, ow);
  c.cType := cBlocked;
end;

procedure Wall.OnDemolish;
begin
  cell.cType := GetCellTypeByName(cell.sprite);
  inherited OnDemolish();
end;

procedure Hospital.OnStay();
begin
  if cell.character <> nil then
    cell.character.HP := cell.character.HP + healAmount;
end;
end.
