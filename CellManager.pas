unit CellManager;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects, DataTypes;

type
  SelectionCondition = function(caster, target: TCellData): boolean of object;
procedure Init(mapName : string);

function GetCell(pos : vector2) : TCellData;

function GetMapScale() : vector2;

procedure UnselectMap();
procedure SelectMap(cond : SelectionCondition; caster : TCellData);

implementation

uses Window, WinApi.Windows;

const
 cellSize = 150;
 cellSpaceX = 130;
 cellSpaceY = 110;

var
 x, y: integer;
 map : Array of Array of TCellData;

function GetCell(pos : vector2) : TCellData;
begin
  if (pos.x < 0) or (pos.y < 0) or (pos.x > x) or (pos.y > y) then
    result := nil
  else
    result := map[pos.y, pos.x];
end;

function GetCellById(xpos, ypos : integer; id : char) : TCellData;
var x, y : extended;
begin
{  X := cellSpacey * xpos;
  Y := cellSpacex * ypos;

    if xpos mod 2 = 0 then
        y := cellSpacex / 2 + y;
          Result := TCellData.Create(y, x, cellSize);}
  Y := cellSpacey * ypos;
  X := cellSpacex * xpos;

  if ypos mod 2 = 0 then
    X := cellSpacex / 2 + x;
  Result := TCellData.Create(x, y, cellSize);

  Case id of
  '0' : begin
          Result.sprite := 'meadow';
          Result.cType := cDefault;
        end;
  '1' : begin
          Result.sprite := 'desert';
          Result.cType := cDefault;
        end;
  '2' : begin
          Result.sprite := 'forest';
          Result.cType := cDifficult;
        end;
  '3' : begin
          Result.sprite := 'hills';
          Result.cType := cBlocked;
        end;
  '4' : begin
          Result.sprite := 'sea';
          Result.cType := cBlocked;
        end;
  End;
  result.ReDraw();
end;

procedure Init(mapName : string);
var i, len : integer;
 f : TextFile;
 line : string;
begin
  AssignFile(f, ExtractFilePath(ParamStr(0)) + 'Resourses\Maps\' + mapName + '.txt');
  Reset(f);
  i := 0;
  while (not EOF(f)) do
  begin
    readln(f, line);

    SetLength(map, i + 1);
    SetLength(map[i], Length(line));
    y := i;
    x := Length(line) - 1;

    for var k := 0 to Length(line) - 1 do
    begin
      map[i][k] := GetCellById(k,i,line[k+1]);
      map[i][k].decardPos.x := k;
      map[i][k].decardPos.y := i;
      map[i][k].cubePos := decardToCube(map[i][k].decardPos);
    end;

    inc(i);

  end;

  CloseFile(f);
end;

function GetMapScale() : vector2;
begin
  result.x := x;
  result.y := y;
end;

procedure UnselectMap();
begin
for var i := 0 to x do
  begin
    for var k := 0 to y do
    begin
      map[k][i].IsSelected := false;
      map[k][i].ReDraw();
    end;
  end;
end;

procedure SelectMap(cond : SelectionCondition; caster : TCellData);
begin
for var i := 0 to x do
  begin
    for var k := 0 to y do
    begin
      map[k][i].IsSelected := cond(caster, map[k][i]);
      map[k][i].ReDraw();
    end;
  end;
end;

end.
