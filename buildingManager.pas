unit buildingManager;

interface

uses DataTypes;

function TryBuild(cell : TCellData) : boolean;

implementation

uses PlayerManager, System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Objects, FMX.Layouts;

const buildingOffset : vector2 = (x : -10; y : -12);
procedure Build(cell : TCellData; ID : integer);
begin
var b : TBuilding;
    b := TBuilding.Create;
    b.OnBuild(cell, curPlayer);

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


    b.sprite := 'build';//тест

    cell.ReDraw();
end;

function TryBuild(cell : TCellData) : boolean;
begin
  result := false;

  if cell.cType <> cBlocked then
  begin
    Build(cell, 0);
    result := true;
  end;
end;
end.
