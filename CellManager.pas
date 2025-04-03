unit CellManager;

interface

uses
  System.SysUtils, System.Types, System.Classes,
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Objects,
  DataTypes;

var
  x: integer;
  dangerCellDamage: integer;

type
  SelectionCondition = function(caster, target: TCellData): boolean of object;
  TCellList = Array of TCellData;

procedure DeleteMap();

function GetCellBetween(a, b: TCellData): TCellList; overload;
function GetCellBetween(a, b: Vector3): TCellList; overload;

function LoadCells(): integer;

function GetMapList(): TStringDynArray;

procedure Init(mapName: string);

function GetCell(pos: vector2): TCellData;

function GetMapScale(): vector2;

function GetCellTypeByName(name: string): TCellType;

function GetCellTypeById(id: char): TCellType;

procedure UnselectMap();

procedure SelectMap(cond: SelectionCondition; caster: TCellData);

implementation

uses Window, WinApi.Windows, System.IOUtils;

const
  cellSize = 150;
  cellSpaceX = 130;
  cellSpaceY = 110;

type
  TCellInfo = record
    sprite: string;
    literal: char;
    tp: integer;
    attackBlocker: boolean;
  end;

var
  y: integer;
  map: Array of Array of TCellData;
  cells: Array of TCellInfo;

procedure DeleteMap();
begin
  for var I := 0 to x do
  begin
    for var k := 0 to y do
    begin
      if map[k][I].character <> nil then
        map[k][I].character.Free;
      if map[k][I].building <> nil then
        map[k][I].building.Free;
      map[k][I].Free;
    end;
  end;
  SetLength(map, 0);
end;

function GetCell(pos: vector2): TCellData;
begin
  if (pos.x < 0) or (pos.y < 0) or (pos.x > x) or (pos.y > y) then
    result := nil
  else
    result := map[pos.y, pos.x];
end;

function GetCellTypeByName(name: string): TCellType;
var
  id: integer;
begin
  for var I := 0 to Length(cells) - 1 do
    if cells[I].sprite = name then
      id := I;
  result := TCellType(cells[id].tp);
end;

function GetCellTypeById(id: char): TCellType;
begin
  for var I := 0 to Length(cells) - 1 do
    if cells[I].literal = id then
      result := TCellType(cells[I].tp);
end;

function GetCellById(xpos, ypos: integer; id: char): TCellData;
var
  x, y: extended;
begin
  y := cellSpaceY * ypos;
  x := cellSpaceX * xpos;

  if ypos mod 2 = 0 then
    x := cellSpaceX / 2 + x;
  result := TCellData.Create(x, y, cellSize);

  for var I := 0 to Length(cells) - 1 do
    if cells[I].literal = id then
    begin
      result.sprite := cells[I].sprite;
      result.cType := TCellType(cells[I].tp);
      result.attackBlocker := cells[I].attackBlocker;
    end;

  result.ReDraw();
end;

function LoadCells(): integer;
var
  Files: TStringDynArray;
  f: TextFile;
  line: string;
  correct: boolean;
  sprite: string;
  literal: char;
  tp: integer;
  code: integer;
  isBlocker: boolean;
begin
  SetLength(cells, 0);
  Files := TDirectory.GetFiles(ExtractFilePath(ParamStr(0)) +
    'Resourses\Cells\');
  for var FileName in Files do
  begin
    correct := true;

    AssignFile(f, FileName);
    Reset(f);
    readln(f, line);
    readln(f, sprite);

    readln(f, line);
    readln(f, line);
    literal := line[1];

    readln(f, line);
    readln(f, line);
    val(line, tp, code);

    readln(f, line);
    readln(f, line);
    isBlocker := line[1] = '+';

    if (code > 0) or (tp > 3) then
      correct := false;

    if correct then
    begin
      SetLength(cells, Length(cells) + 1);

      cells[Length(cells) - 1].sprite := sprite;
      cells[Length(cells) - 1].literal := literal;
      cells[Length(cells) - 1].tp := tp;
      cells[Length(cells) - 1].attackBlocker := isBlocker;
    end;
  end;
  result := Length(cells);
end;

procedure Init(mapName: string);
var
  I, len: integer;
  f: TextFile;
  line: string;
begin
  AssignFile(f, ExtractFilePath(ParamStr(0)) + 'Resourses\Maps\' + mapName
    + '.txt');
  Reset(f);
  readln(f, line);
  I := 0;
  while (not EOF(f)) do
  begin
    readln(f, line);

    SetLength(map, I + 1);
    SetLength(map[I], Length(line));
    y := I;
    x := Length(line) - 1;
    minMapY := -x * cellSpaceY div 2;
    minMapX := -y * cellSpaceY div 4;
    for var k := 0 to Length(line) - 1 do
    begin
      map[I][k] := GetCellById(k, I, line[k + 1]);
      map[I][k].decardPos.x := k;
      map[I][k].decardPos.y := I;
      map[I][k].cubePos := decardToCube(map[I][k].decardPos);
      map[I][k].img.Bitmap.LoadFromFile(ExtractFilePath(ParamStr(0)) +
        'Resourses\Sprites\' + map[I][k].sprite + '.png');
    end;

    inc(I);

  end;

  CloseFile(f);

  form2.BuildingsOrigin.BringToFront();
  form2.CharactersOrigin.BringToFront();
end;

function GetMapScale(): vector2;
begin
  result.x := x;
  result.y := y;
end;

procedure UnselectMap();
begin
  for var I := 0 to x do
  begin
    for var k := 0 to y do
    begin
      map[k][I].IsSelected := false;
      map[k][I].ReDraw();
    end;
  end;
end;

procedure SelectMap(cond: SelectionCondition; caster: TCellData);
begin
  for var I := 0 to x do
  begin
    for var k := 0 to y do
    begin
      map[k][I].IsSelected := cond(caster, map[k][I]);
      map[k][I].ReDraw();
    end;
  end;
end;

function GetMapList(): TStringDynArray;
var
  Files: TStringDynArray;
  f: TextFile;
  line: string;
  len, j: integer;
  correct, found: boolean;
begin
  SetLength(result, 0);
  Files := TDirectory.GetFiles(ExtractFilePath(ParamStr(0)) +
    'Resourses\Maps\');
  for var FileName in Files do
  begin
    correct := true;
    try
      begin
        AssignFile(f, FileName);
        Reset(f);
        readln(f, line);
        readln(f, line);
        len := Length(line);
        while (not EOF(f) and correct) do
        begin
          readln(f, line);
          if Length(line) <> len then
            correct := false
          else
          begin
            for var I := 1 to len do
            begin
              j := 0;
              found := false;
              while (not found) and (j < Length(cells)) do
              begin
                if (cells[j].literal = line[I]) then
                  found := true;
                j := j + 1;
              end;
              if not found then
                correct := false;
            end;
          end;

        end;
      end
    except
      correct := false;
    end;
    if correct then
    begin
      SetLength(result, Length(result) + 1);
      var
        lastDir: integer;
      for var k := 1 to Length(FileName) do
      begin
        if (FileName[k] = '/') or (FileName[k] = '\') then
          lastDir := k;
      end;
      result[Length(result) - 1] := Copy(FileName, lastDir + 1,
        Length(FileName) - lastDir - 4);
    end;
  end;
end;

function GetCellBetween(a, b: TCellData): TCellList;
begin
  result := GetCellBetween(a.cubePos, b.cubePos);
end;

function GetCellBetween(a, b: Vector3): TCellList;
var
  n: integer;
begin
  n := GetDistance(a, b);
  SetLength(result, n + 1);

  for var I := 0 to n do
    result[I] := GetCell(CubeToDecard(LerpCubePos(a, b, I * (1 / n))));
end;

end.
