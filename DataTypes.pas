unit DataTypes;

interface
 uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects;

Type

Vector2 = Record
  x, y : integer;
End;

Vector3 = Record
  x, y, z : integer;
End;

TCellData = class;

TSkill = class
  protected
    function IsCorrectTarget(caster, target: TCellData) : boolean; virtual; abstract;
  public
    reloadTime : integer;
    timeAfterUse : integer;
    hasTarget : boolean;
    procedure Select(caster: TCellData);
    procedure Use(caster, target: TCellData); virtual; abstract;
end;

TCharacter = class
    private

    var healsBar : TImage;

    destructor OnDestroy();
    procedure SetHP(h : integer);
    function GetHP() : integer;

    public
      var
        owner : byte;
        name : string;
        sprite : string;
        pos : vector2;
        heals : integer;
        maxHp : integer;
        speed : integer;
        armor : integer;
        img : TImage;
        movePoints : integer;

        atack, skill1, skill2 : TSkill;

        procedure Init(form : TForm);
        procedure ResetMP();
        procedure BuyMP();
        procedure ReDraw();
        function IsSelected() : boolean;
        property HP: integer read GetHP write SetHP;
  end;

TCellType = (cBlocked, cDefault, cDifficult);


TCellData = class
  private
    procedure OnClick(sender : Tobject);
    procedure SetImage(im : TImage);
    function GetImage() : TImage;
    destructor OnDestroy();

    var
      img : TImage;

  public
    var
      sprite : string;
      decardPos : Vector2;
      cubePos : Vector3;
      cType : TCellType;
      character : TCharacter;

  procedure ReDraw();

  property Image: TImage read GetImage write SetImage;

End;


charList = ^charElem;
charElem = Record
      data : TCharacter;
      next : charList;
End;

  TPlayer = class
    private
      destructor Destroy();
      var
        characters : charList;
    public
      procedure AddCharacter(c : TCharacter);
      procedure Init();
      procedure OnRoundStart();
  end;

function decardToCube(pos : vector2) : Vector3;

function GetDistance(a, b : vector3) : integer;

implementation

uses Drawer, CharacterManager, PlayerManager, CellManager, CharacterDataVisualisator;

procedure TCellData.ReDraw();
var cBitmap : TBitMap;
begin
  img.Bitmap.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Resourses\Sprites\' + sprite + '.png');

  if character <> nil then
    character.ReDraw();
end;

procedure TCellData.OnClick(sender: TObject);
begin                          //тест
  WriteLn('clicked    ' + sprite + '  at decard  x=', decardPos.x, '  y=', decardPos.y, '/cube  x=', cubePos.x, '  y=', cubePos.y, '  z=', cubePos.z,'   has character  ', character <> nil);


  if character = nil then       // если режим предварительной расстановки
  begin
    if selectedCharacter.x = -1 then
      TryCreateCharacter(Self)
    else
      TryMoveCharacter(GetCell(selectedCharacter), Self);
  end
  else
  begin
    if character.owner = curPlayer then
    begin
      if (selectedCharacter.x = decardPos.x) and (selectedCharacter.y = decardPos.y) then
        UnselectCharacter()
      else
        SelectCharacter(decardPos);
    end;
  end;

end;

procedure TCellData.SetImage(im: TImage);
begin
  if img <> nil then
    img.OnClick := nil;
  img := im;
  img.OnClick := OnClick;
end;

function TCellData.GetImage: TImage;
begin
  Result := img;
end;


destructor TCharacter.OnDestroy();
begin
  img.Free;
  healsBar.Free;
  inherited Destroy;
end;


destructor TCellData.OnDestroy();
begin
  img.Free;
  character.Free;
  inherited Destroy;
end;


procedure TCharacter.ReDraw();
begin
  img.Bitmap.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Resourses\Sprites\' + sprite + IntToStr(owner) + '.png');
  if isSelected then
    DrawOutline(img, TAlphaColors.Yellow, 50);
end;


function decardToCube(pos : vector2) : Vector3;
begin
  result.x := pos.x - (pos.y + (pos.y and 1)) div 2;
  result.y := pos.y;
  result.z := -result.x - result.y;
end;

function GetDistance(a, b : vector3) : integer;
begin
  result := (Abs(a.x - b.x) + Abs(a.y - b.y) + Abs(a.z - b.z)) div 2;
end;


const
  healsBarScale : vector2 = (x: 50; y : 4);
  healsBarPos : vector2 = (x: 53; y : 110);


procedure TCharacter.Init(form : TForm);
begin
  healsBar := TImage.Create(form);
  healsBar.Parent := img;
  healsBar.Position.X := healsBarPos.X;
  healsBar.Position.Y := healsBarPos.Y;

  healsBar.Width := healsBarScale.x;
  healsBar.Height := healsBarScale.y;

  healsBar.Bitmap.Width := healsBarScale.x;
  healsBar.Bitmap.Height := healsBarScale.y;
  healsBar.Bitmap.Canvas.BeginScene();
  healsBar.Bitmap.Canvas.Fill.Color := TAlphaColors.Crimson;
  healsBar.Bitmap.Canvas.FillRect(TRectF.Create(0, 0, healsBarScale.x, healsBarScale.y), 0, 0, [], 1);
  healsBar.Bitmap.Canvas.EndScene();
end;


function TCharacter.GetHP() : integer;
begin
  result := heals;
end;

procedure TCharacter.SetHP(h : integer);
begin
  heals := h;

  healsBar.Bitmap.Width := healsBarScale.x;
  healsBar.Bitmap.Height := healsBarScale.y;
  healsBar.Bitmap.Canvas.BeginScene();
  healsBar.Bitmap.Canvas.Fill.Color := TAlphaColors.Brown;
  healsBar.Bitmap.Canvas.FillRect(TRectF.Create(0, 0, healsBarScale.x, healsBarScale.y), 0, 0, [], 1);
  healsBar.Bitmap.Canvas.Fill.Color := TAlphaColors.Crimson;
  healsBar.Bitmap.Canvas.FillRect(TRectF.Create(0, 0, Round(healsBarScale.x * heals / maxHP), healsBarScale.y), 0, 0, [], 1);

  healsBar.Bitmap.Canvas.EndScene();

end;

procedure TCharacter.ResetMP();
begin
  movePoints := 0;
end;

procedure TCharacter.BuyMP();
begin
  if GetActionCount() > 0 then
  begin
    Inc(movePoints, speed);
    SetActionCount(GetActionCount() - 1);
    CharacterDataVisualisator.ReDraw();
  end;
end;

procedure TPlayer.Init();
begin
  New(characters);
  characters^.data := nil;
  characters^.next := nil;
end;

procedure TPlayer.AddCharacter(c: TCharacter);
var cur : charList;
begin
  cur := characters;
  while cur.next <> nil do
    cur := cur^.next;
  New(cur^.next);
  cur := cur^.next;
  cur^.next := nil;
  cur^.data := c;
end;

procedure TPlayer.OnRoundStart();
var curChar : charList;
begin
  curChar := characters^.next;
  while curChar <> nil do
  begin
    curChar^.data.movePoints := 0;
    curChar := curChar^.next;
  end;
end;

function TCharacter.IsSelected() : boolean;
begin
  result := (selectedCharacter.x = pos.x) and (selectedCharacter.y = pos.y);
end;

destructor TPlayer.Destroy();
begin
  Dispose(characters);
end;

procedure TSkill.Select(caster: TCellData);
begin
  if GetActionCount() > 0 then
  begin
    if hasTarget then
    begin

    end
    else
    begin
      Use(caster, caster);
      SetActionCount(GetActionCount() - 1);
    end;
  end;
end;

end.
