unit CellManager;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  DataTypes;

var x : integer;
type
  SelectionCondition = function(caster, target: TCellData): boolean of object;

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

var
  y: integer;
  map: Array of Array of TCellData;

function GetCell(pos: vector2): TCellData;
begin
  if (pos.x < 0) or (pos.y < 0) or (pos.x > x) or (pos.y > y) then
    result := nil
  else
    result := map[pos.y, pos.x];
end;

const
  nameList: array [0 .. 4] of string = ('meadow', 'desert', 'forest',
    'hills', 'sea');

function GetCellTypeByName(name: string): TCellType;
var
  id: integer;
begin
  for var I := 0 to Length(nameList) - 1 do
    if nameList[I] = name then
      id := I;
  result := GetCellTypeById(char(Ord('0') + id));
end;

function GetCellTypeById(id: char): TCellType;
begin
  Case id of
    '0':
      begin
        result := cDefault;
      end;
    '1':
      begin
        result := cDefault;
      end;
    '2':
      begin
        result := cDifficult;
      end;
    '3':
      begin
        result := cBlocked;
      end;
    '4':
      begin
        result := cBlocked;
      end;
  End;
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

  Case id of
    '0':
      begin
        result.sprite := 'meadow';
      end;
    '1':
      begin
        result.sprite := 'desert';
      end;
    '2':
      begin
        result.sprite := 'forest';
      end;
    '3':
      begin
        result.sprite := 'hills';
      end;
    '4':
      begin
        result.sprite := 'sea';
      end;
  End;

  result.cType := GetCellTypeById(id);

  result.ReDraw();
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
    end;

    inc(I);

  end;

  CloseFile(f);
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
  len: integer;
  correct: boolean;
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
              if (line[I] < '0') or (line[I] > '4') then
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
      var lastDir : integer;
      for var k := 1 to Length(fileName) do
        begin
          if (fileName[k] = '/')  or (fileName[k] = '\') then
            lastDir := k;
        end;
      result[Length(result) - 1] := Copy(FileName, lastDir + 1, Length(fileName) - lastDir - 4);
    end;
  end;
end;

end.
