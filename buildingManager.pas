unit buildingManager;

interface

uses DataTypes;

var
  defaulBuildingsCount: TCount;

procedure Init();

function TryBuild(cell: TCellData): boolean; overload;

function TryBuild(cell: TCellData; id: integer): boolean; overload;

function GetBuildingName(id: integer; out hint: string): string;

function IsBuildingFileValid(): boolean;

implementation

uses
  PlayerManager, CellManager, DiceManager, Window, System.SysUtils, FMX.Objects;

const
  buildingOffset: vector2 = (x: - 10; y: - 12);

type
  Wall = class(TBuilding)
  public
    procedure OnBuild(c: TCellData; ow: integer); override;
    procedure OnDemolish(); override;
    class function CanBeBuilded(cell: TCellData): boolean; override;
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

  Portal = class(TBuilding)
  public
    procedure OnBuild(c: TCellData; ow: integer); override;
  end;

var
  buildingCount: integer;

function CanBeBuilded(id: integer; cell: TCellData): boolean;
begin
  case id of
    0:
      result := Wall.CanBeBuilded(cell);
    1:
      result := Hospital.CanBeBuilded(cell);
    2:
      result := Fort.CanBeBuilded(cell);
    3:
      result := Kapkan.CanBeBuilded(cell);
    4:
      result := Portal.CanBeBuilded(cell);
  end;
end;

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
    4:
      result := Portal.Create;
  end;

  AssignFile(f, ExtractFilePath(ParamStr(0)) +
    'Resourses\Configs\BuildingsData.txt');
  Reset(f);
  for var I := 0 to id - 1 do
    for var j := 1 to 16 do
      Readln(f, line);

  Readln(f, line);
  Readln(f, line);
  result.name := line;

  Readln(f, line);
  Readln(f, line);

  Readln(f, line);
  Readln(f, line);
  result.sprite := line;

  Readln(f, line); // кол-во
  Readln(f, line);

  Readln(f, line);
  Readln(f, line);
  result.maxHp := StrToInt(line);

  Readln(f, line);
  Readln(f, line);
  result.cost := StrToInt(line);

  Readln(f, line);
  Readln(f, data1);
  Readln(f, line);
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
    4:
      defaulBuildingsCount[3] := 1;
  end;
  CloseFile(f);
end;

function GetBuildingName(id: integer; out hint: string): string;
var
  f: TextFile;
  data1, data2: string;
begin
  AssignFile(f, ExtractFilePath(ParamStr(0)) +
    'Resourses\Configs\BuildingsData.txt');
  Reset(f);
  for var I := 0 to id - 1 do
    for var j := 1 to 16 do
      Readln(f, result);

  Readln(f, result);
  Readln(f, result);

  Readln(f, hint);
  Readln(f, hint);

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

  if (cell.cType <> cBlocked) and (cell.building = nil) and
    (CanBeBuilded(id, cell)) then
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
    if I mod 16 = 9 then
    begin
      SetLength(defaulBuildingsCount, Length(defaulBuildingsCount) + 1);
      defaulBuildingsCount[Length(defaulBuildingsCount) - 1] := StrToInt(line);
    end;
    if I mod 16 = 0 then
      Inc(buildingCount);
  end;

  CloseFile(f);
end;

function IsBuildingFileValid(): boolean;
var
  f: TextFile;
  line: string;
  errors, code, t: integer;
begin
  errors := 0;
  try
    try
      AssignFile(f, ExtractFilePath(ParamStr(0)) +
        'Resourses\Configs\BuildingsData.txt');
      Reset(f);
      for var I := 1 to 5 do
      begin
        Readln(f, line);
        Readln(f, line);
        Readln(f, line);
        Readln(f, line);
        Readln(f, line);
        Readln(f, line);
        if not(FileExists(ExtractFilePath(ParamStr(0)) + 'Resourses\Sprites\' +
          line + '0' + '.png') and FileExists(ExtractFilePath(ParamStr(0)) +
          'Resourses\Sprites\' + line + '1' + '.png')) then
          Inc(errors);

        Readln(f, line);
        Readln(f, line);
        Val(line, t, code);
        errors := errors + code;

        Readln(f, line);
        Readln(f, line);
        Val(line, t, code);
        errors := errors + code;

        Readln(f, line);
        Readln(f, line);
        Val(line, t, code);
        errors := errors + code;

        case I of
          1, 5:
            begin
              Readln(f, line);
              Readln(f, line);
              Readln(f, line);
              Readln(f, line);
            end;
          2, 4:
            begin
              Readln(f, line);
              Readln(f, line);
              Val(line, t, code);
              errors := errors + code;
              Readln(f, line);
              Readln(f, line);
            end;
          3:
            begin
              Readln(f, line);
              Readln(f, line);
              Val(line, t, code);
              errors := errors + code;
              Readln(f, line);
              Readln(f, line);
              Val(line, t, code);
              errors := errors + code;
            end;
        end;
      end;
    finally
      CloseFile(f);
    end;
  except
    Inc(errors);
  end;
  result := errors = 0;
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
var
  curDamage: integer;
begin
  if cell.building.owner <> cell.character.owner then
  begin
    curDamage := DropDices(damage);
    cell.AtackCell(curDamage);
    cell.building := nil;
    form2.KapkanPlayer.CurrentTime := 0;
    form2.KapkanPlayer.Play();
    self.Free;
  end;
end;

procedure Portal.OnBuild(c: TCellData; ow: integer);
begin
  inherited OnBuild(c, ow);
  players[ow].portalCell := c;
end;

class function Wall.CanBeBuilded(cell: TCellData): boolean;
begin
  result := cell.character = nil;
end;

end.
