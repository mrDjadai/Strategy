unit CellManager;

interface

uses
  System.SysUtils, System.Types, System.Classes,
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.Objects,
  DataTypes, System.UITypes;

var
  x: integer;
  dangerCellDamage: integer;

type
  TMapEdge = (meNone, meLeft, meRight, meTop, meBottom, meTopLeft, meTopRight,
    meBottomLeft, meBottomRight);

  SelectionCondition = function(caster, target: TCellData): boolean of object;
  TCellList = Array of TCellData;

  TTransparencyMap = array of array of boolean;

  TTransparentHitImage = class(TImage)
  private
    FTransparencyMap: TTransparencyMap;
    FTransparencyThreshold: byte;

  protected
    function PointInObject(x, Y: Single): boolean; override;
  public
    procedure InitializeTransparencyMap;
    procedure SetTransparencyThreshold(Threshold: byte);
    constructor Create(AOwner: TComponent); override;
    procedure UpdateTransparencyMap;
  end;

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

function CheckMapEdges(): TMapEdge;

implementation

uses Window, WinApi.Windows, System.IOUtils;

const
  cellSize = 150;
  cellSpaceX = 129;
  cellSpaceY = 112;

type
  TCellInfo = record
    sprite: string;
    literal: char;
    tp: integer;
    attackBlocker: boolean;
  end;

var
  Y: integer;
  map: Array of Array of TCellData;
  cells: Array of TCellInfo;

function CheckMapEdges(): TMapEdge;
var
  mapLeft, mapRight, mapTop, mapBottom: Single;
  screenLeft, screenRight, screenTop, screenBottom: Single;
  firstCell, lastCell: TCellData;
  cellPos: TPointF;
  mapWidth, mapHeight: Single;
begin
  Result := meNone;
  if Length(map) = 0 then
    Exit;

  // Получаем границы экрана
  screenLeft := 0;
  screenTop := 0;
  screenRight := Form2.ClientWidth;
  screenBottom := Form2.ClientHeight;

  // Получаем первую и последнюю клетки карты
  firstCell := map[0][0];
  lastCell := map[Length(map)-1][Length(map[0])-1];

  // Получаем абсолютные позиции клеток относительно формы
  cellPos := firstCell.img.LocalToAbsolute(PointF(0, cellSize));
  mapLeft := cellPos.X - cellSpaceX / 2;
  mapTop := cellPos.Y;

  cellPos := lastCell.img.LocalToAbsolute(PointF(cellSize, 0));
  mapRight := cellPos.X + cellSpaceX / 2;
  mapBottom := cellPos.Y;

  // Вычисляем полный размер карты
  mapWidth := mapRight - mapLeft;
  mapHeight := mapBottom - mapTop;

  // Проверяем, помещается ли карта на экране
  if (mapWidth >= screenRight)then
  begin
    // Если карта не помещается - стандартная проверка границ
    if (mapLeft > screenLeft) and (mapTop > screenTop) then
      Result := meTopRight
    else if (mapRight < screenRight) and (mapTop > screenTop) then
      Result := meTopLeft
    else if (mapLeft > screenLeft) and (mapBottom < screenBottom) then
      Result := meBottomLeft
    else if (mapRight < screenRight) and (mapBottom < screenBottom) then
      Result := meBottomRight
    else if mapLeft > screenLeft then
      Result := meRight
    else if mapRight < screenRight then
      Result := meLeft
    else if mapTop > screenTop then
      Result := meTop
    else if mapBottom < screenBottom then
      Result := meBottom;
  end
  else
  begin
    // Если карта не помещается - стандартная проверка границ
    if (mapLeft < screenLeft) and (mapTop > screenTop) then
      Result := meTopLeft
    else if (mapRight > screenRight) and (mapTop > screenTop) then
      Result := meTopRight
    else if (mapLeft < screenLeft) and (mapBottom < screenBottom) then
      Result := meBottomLeft
    else if (mapRight > screenRight) and (mapBottom < screenBottom) then
      Result := meBottomRight
    else if mapLeft < screenLeft then
      Result := meLeft
    else if mapRight > screenRight then
      Result := meRight
    else if mapTop > screenTop then
      Result := meTop
    else if mapBottom < screenBottom then
      Result := meBottom;
  end;
end;


procedure DeleteMap();
begin
  for var I := 0 to x do
  begin
    for var k := 0 to Y do
    begin
      map[k][I].character.Free;
      map[k][I].building.Free;
      map[k][I].Free;
    end;
  end;
  SetLength(map, 0);
end;

function GetCell(pos: vector2): TCellData;
begin
  if (pos.x < 0) or (pos.Y < 0) or (pos.x > x) or (pos.Y > Y) then
    Result := nil
  else
    Result := map[pos.Y, pos.x];
end;

function GetCellTypeByName(name: string): TCellType;
var
  id: integer;
begin
  for var I := 0 to Length(cells) - 1 do
    if cells[I].sprite = name then
      id := I;
  Result := TCellType(cells[id].tp);
end;

function GetCellTypeById(id: char): TCellType;
begin
  for var I := 0 to Length(cells) - 1 do
    if cells[I].literal = id then
      Result := TCellType(cells[I].tp);
end;

function GetCellById(xpos, ypos: integer; id: char): TCellData;
var
  x, Y: extended;
begin
  Y := cellSpaceY * ypos;
  x := cellSpaceX * xpos;

  if ypos mod 2 = 0 then
    x := cellSpaceX / 2 + x;
  Result := TCellData.Create(x, Y, cellSize);

  for var I := 0 to Length(cells) - 1 do
    if cells[I].literal = id then
    begin
      Result.sprite := cells[I].sprite;
      Result.cType := TCellType(cells[I].tp);
      Result.attackBlocker := cells[I].attackBlocker;
    end;

  Result.ReDraw();
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
  Result := Length(cells);
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
    Y := I;
    x := Length(line) - 1;
    minMapY := -x * cellSpaceY div 2;
    minMapX := -Y * cellSpaceY div 4;
    for var k := 0 to Length(line) - 1 do
    begin
      map[I][k] := GetCellById(k, I, line[k + 1]);
      map[I][k].decardPos.x := k;
      map[I][k].decardPos.Y := I;
      map[I][k].cubePos := decardToCube(map[I][k].decardPos);
      map[I][k].img.Bitmap.LoadFromFile(ExtractFilePath(ParamStr(0)) +
        'Resourses\Sprites\' + map[I][k].sprite + '.png');

      TTransparentHitImage(map[I][k].img).InitializeTransparencyMap();
    end;

    inc(I);

  end;

  CloseFile(f);

  Form2.BuildingsOrigin.BringToFront();
  Form2.CharactersOrigin.BringToFront();
end;

function GetMapScale(): vector2;
begin
  Result.x := x;
  Result.Y := Y;
end;

procedure UnselectMap();
begin
  for var I := 0 to x do
  begin
    for var k := 0 to Y do
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
    for var k := 0 to Y do
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
  SetLength(Result, 0);
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
      SetLength(Result, Length(Result) + 1);
      var
        lastDir: integer;
      for var k := 1 to Length(FileName) do
      begin
        if (FileName[k] = '/') or (FileName[k] = '\') then
          lastDir := k;
      end;
      Result[Length(Result) - 1] := Copy(FileName, lastDir + 1,
        Length(FileName) - lastDir - 4);
    end;
  end;
end;

function GetCellBetween(a, b: TCellData): TCellList;
begin
  Result := GetCellBetween(a.cubePos, b.cubePos);
end;

function GetCellBetween(a, b: Vector3): TCellList;
var
  n: integer;
begin
  n := GetDistance(a, b);
  SetLength(Result, n + 1);

  for var I := 0 to n do
    Result[I] := GetCell(CubeToDecard(LerpCubePos(a, b, I * (1 / n))));
end;

constructor TTransparentHitImage.Create(AOwner: TComponent);
begin
  inherited;
  FTransparencyThreshold := 10;
end;

procedure TTransparentHitImage.SetTransparencyThreshold(Threshold: byte);
begin
  if FTransparencyThreshold <> Threshold then
  begin
    FTransparencyThreshold := Threshold;
    UpdateTransparencyMap();
  end;
end;

procedure TTransparentHitImage.InitializeTransparencyMap();
begin
  if (Bitmap <> nil) and not Bitmap.IsEmpty then
  begin
    SetLength(FTransparencyMap, Bitmap.Width, Bitmap.Height);
    UpdateTransparencyMap();
  end;
end;

procedure TTransparentHitImage.UpdateTransparencyMap();
var
  BitmapData: TBitmapData;
  x, Y: integer;
  PixelColor: TAlphaColor;
begin
  if (Bitmap <> nil) and not Bitmap.IsEmpty then
    if Bitmap.map(TMapAccess.Read, BitmapData) then
      try
        for Y := 0 to Bitmap.Height - 1 do
          for x := 0 to Bitmap.Width - 1 do
          begin
            PixelColor := BitmapData.GetPixel(x, Y);
            FTransparencyMap[x, Y] := TAlphaColorRec(PixelColor).a <
              FTransparencyThreshold;
          end;
      finally
        Bitmap.Unmap(BitmapData);
      end;
end;

function TTransparentHitImage.PointInObject(x, Y: Single): boolean;
var
  LocalPoint: TPointF;
  BitmapX, BitmapY: integer;
  PixelColor: TAlphaColor;
begin
  Result := inherited PointInObject(x, Y);

  if Result and (Bitmap <> nil) and not Bitmap.IsEmpty then
  begin
    LocalPoint := AbsoluteToLocal(TPointF.Create(x, Y));

    BitmapX := Trunc(LocalPoint.x * Bitmap.Width / Width);
    BitmapY := Trunc(LocalPoint.Y * Bitmap.Height / Height);

    if (BitmapX >= 0) and (BitmapX < Bitmap.Width) and (BitmapY >= 0) and
      (BitmapY < Bitmap.Height) then
    begin
      Result := not FTransparencyMap[BitmapX, BitmapY];
    end
    else
      Result := false;
  end;
end;

end.

  end.
