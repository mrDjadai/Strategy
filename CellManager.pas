unit CellManager;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs;

Type
Vector3 = Record
  x, y, z : integer;
End;

Vector2 = Record
  x, y : integer;
End;

Cell = Record
  sprite : string;
  decardPos : Vector2;
  cubePos : Vector3;
End;

procedure Init();

implementation
var
 x, y : integer;
 map : Array of Array of Cell;

procedure Init();
begin

end;


end.
