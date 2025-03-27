unit buildingManager;

interface

uses DataTypes;

var
  defaulBuildingsCount: TCount;

procedure Init();

function TryBuild(cell: TCellData): boolean; overload;

function TryBuild(cell: TCellData; id: integer): boolean; overload;

function GetBuildingName(id: integer): string;

implementation

uses PlayerManager, cellManager, System.SysUtils, System.Types, System.UITypes,
  System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Objects, FMX.Layouts,
  DiceManager, WIndow;

const
  buildingOffset: vector2 = (x: - 10; y: - 12);

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
    healAmount: integer;
  end;

  Fort = class(TBuilding)
  public
  var
    damage: dicesCount;
    armor: integer;
    procedure OnEnter(); override;
    procedure OnExit(); override;
  end;

  Kapkan = class(TBuilding)
  public
  var
    damage: dicesCount;
    procedure OnEnter(); override;
  end;

var
  buildingCount: integer;

function CreateBuilding(id: integer): TBuilding;
var
  f: TextFile;
  line, data1, data2: string;
begin
  case id of
    0:
      result := Wall.Create;
    1:
      result := Hospital.Create;
    2:
      result := Fort.Create;
    3:
      result := Kapkan.Create;
  end;

  AssignFile(f, ExtractFilePath(ParamStr(0)) +
    'Resourses\Configs\BuildingsData.txt');
  Reset(f);
  for var I := 0 to id - 1 do
    for var j := 1 to 7 do
      Readln(f, line);

  Readln(f, line);
  result.name := line;

  Readln(f, line);
  result.sprite := line;

  Readln(f, line); // кол-во

  Readln(f, line);
  result.maxHp := StrToInt(line);

  Readln(f, line);
  result.cost := StrToInt(line);

  Readln(f, data1);
  Readln(f, data2);

  case id of
    0:
      ;
    1:
      Hospital(result).healAmount := StrToInt(data1);
    2:
      begin
        Fort(result).armor := StrToInt(data1);
        Fort(result).damage := LoadDices(data2);
      end;
    3:
      Kapkan(result).damage := LoadDices(data1);
  end;
  CloseFile(f);
end;

function GetBuildingName(id: integer): string;
var
  f: TextFile;
  data1, data2: string;
begin
  AssignFile(f, ExtractFilePath(ParamStr(0)) +
    'Resourses\Configs\BuildingsData.txt');
  Reset(f);
  for var I := 0 to id - 1 do
    for var j := 1 to 7 do
      Readln(f, result);

  Readln(f, result);
  CloseFile(f);
end;

procedure Build(cell: TCellData; id: integer);
begin
  var
    b: TBuilding;
  b := CreateBuilding(id);

  b.pos := cell.decardPos;

  var
    myImage: TImage;
  myImage := TImage.Create(form2);
  myImage.Parent := form2.BuildingsOrigin;

  myImage.Position.x := cell.Image.Position.x + buildingOffset.x;
  myImage.Position.y := cell.Image.Position.y + buildingOffset.y;
  myImage.Height := cell.Image.Height;
  myImage.Width := cell.Image.Width;

  myImage.HitTest := false;
  b.img := myImage;

  myImage.BringToFront();

  b.OnBuild(cell, curPlayer);

  cell.ReDraw();
  b.ReDraw();
end;

function TryBuild(cell: TCellData): boolean;
begin
  result := TryBuild(cell, placableBuildingId);
end;

function TryBuild(cell: TCellData; id: integer): boolean;
begin
  result := false;

  if (cell.cType <> cBlocked) and (cell.building = nil) then
  begin
    Build(cell, id);
    placableBuildingId := -1;
    result := true;

    if prepareMode then
      TryEndPrepare();
  end;
end;

procedure Init();
var
  f: TextFile;
  line: string;
  I: integer;
begin
  buildingCount := 0;
  AssignFile(f, ExtractFilePath(ParamStr(0)) +
    'Resourses\Configs\BuildingsData.txt');
  Reset(f);
  I := 1;

  while (not EOF(f)) do
  begin
    Readln(f, line);
    Inc(I);
    if I mod 7 = 4 then
    begin
      SetLength(defaulBuildingsCount, Length(defaulBuildingsCount) + 1);
      defaulBuildingsCount[Length(defaulBuildingsCount) - 1] := StrToInt(line);
    end;
    if I mod 7 = 0 then
      Inc(buildingCount);
  end;

  CloseFile(f);
end;

procedure Wall.OnBuild(c: TCellData; ow: integer);
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

procedure Fort.OnEnter();
begin
  cell.character.armor := cell.character.armor + armor;
  cell.character.bonusDices := SumDices(cell.character.bonusDices, damage)
end;

procedure Fort.OnExit();
begin
  cell.character.armor := cell.character.armor - armor;
  cell.character.bonusDices := SubDices(cell.character.bonusDices, damage)
end;

procedure Kapkan.OnEnter();
var curDamage : integer;
begin
  if cell.building.owner <> cell.character.owner then
  begin
    curDamage := DropDices(damage);
    cell.AtackCell(curDamage);
    cell.building := nil;
    self.Free;
  end;
end;

end.
