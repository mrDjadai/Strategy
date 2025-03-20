unit DataTypes;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects;

Type

  Vector2 = Record
    x, y: integer;
  End;

  Vector3 = Record
    x, y, z: integer;
  End;

  TCellData = class;

  TSkill = class
  protected
    function IsCorrectTarget(caster, target: TCellData): boolean;
      virtual; abstract;
  public
    name : string;
    reloadTime: integer;
    timeAfterUse: integer;
    hasTarget: boolean;
    procedure Select(caster: TCellData);
    procedure Use(caster, target: TCellData); virtual; abstract;
    procedure Clone(other : TSkill); overload; virtual;
    Constructor Create(targetable : boolean); overload;
  end;

  TCharacter = class
  private
  var
    healsBar: TImage;

    procedure SetHP(h: integer);
    function GetHP(): integer;
    function RecalculateDamage(h : integer) : integer;

  public
  var
    owner: byte;
    name: string;
    sprite: string;
    pos: Vector2;
    heals: integer;
    maxHp: integer;
    speed: integer;
    armor: integer;
    img: TImage;
    movePoints: integer;

    atack, skill1, skill2: TSkill;

    destructor Destroy(); override;

    property HP: integer read GetHP write SetHP;
    procedure Init(form: TForm);
    procedure ResetMP();
    procedure BuyMP();
    procedure ReDraw();
    function IsSelected(): boolean;
    Constructor Create(other : TCharacter); overload;
    constructor Create; overload;
  end;

  TCellType = (cBlocked, cDefault, cDifficult);

  TCellData = class
  private
    procedure OnClick(sender: Tobject);
    procedure SetImage(im: TImage);
    function GetImage(): TImage;
    destructor Destroy(); override;

  var
    img : TImage;
    selector : TImage;

  public
  var
    isSelected : boolean;
    sprite: string;
    decardPos: Vector2;
    cubePos: Vector3;
    cType: TCellType;
    character: TCharacter;

    procedure ReDraw();

    property Image: TImage read GetImage write SetImage;

    Constructor Create(x, y : extended; size : integer);

  End;

  charList = ^charElem;

  charElem = Record
    data: TCharacter;
    next: charList;
  End;

  TPlayer = class
  private
    destructor Destroy();

  var
    characters: charList;
  public
    procedure AddCharacter(c: TCharacter);
    procedure RemoveCharacter(c: TCharacter);
    procedure Init();
    procedure OnRoundStart();
  end;

function decardToCube(pos: Vector2): Vector3;

function GetDistance(a, b: Vector3): integer;

implementation

uses Drawer, CharacterManager, PlayerManager, CellManager,
  CharacterDataVisualisator, Window;


Constructor TCellData.Create(x, y : extended; size : integer);
begin
    inherited Create;
    Image := TImage.Create(form2);

    Image.Parent := form2.Map;

    Image.Position.X := x;
    Image.Position.Y := y;

    Image.Width := size;
    Image.Height := size;
    Image.RotationAngle := 90;
    isSelected := false;

    selector := TImage.Create(form2);

    selector.Parent := image;

    selector.Position.X := 0;
    selector.Position.Y := 0;

    selector.Width := size;
    selector.Height := size;
    selector.Bitmap.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Resourses\Sprites\cellSelector.png');
    selector.HitTest := false;
    selector.Visible := false;
end;

procedure TCellData.ReDraw();
var
  cBitmap: TBitMap;
begin
  img.Bitmap.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Resourses\Sprites\' +
    sprite + '.png');
  selector.Visible := isSelected;
  if character <> nil then
    character.ReDraw();
end;

procedure TCellData.OnClick(sender: Tobject);
begin
  WriteLn('clicked    ' + sprite + '  at decard  x=', decardPos.x, '  y=',
    decardPos.y, '/cube  x=', cubePos.x, '  y=', cubePos.y, '  z=', cubePos.z,
    '   has character  ', character <> nil);

  if targetSelectionMode then
  begin
    if selectedSkill.IsCorrectTarget(SelectedCaster, self) then
    begin
      targetSelectionMode := false;
      selectedSkill.Use(SelectedCaster, self);
      selectedSkill := nil;
      Form2.SkipRound.Enabled := true;
      UnselectMap();
    end;
  end
  else
  if selectedSkill = nil then
  begin
    if character = nil then // если режим предварительной расстановки
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
        if (selectedCharacter.x = decardPos.x) and
          (selectedCharacter.y = decardPos.y) then
          UnselectCharacter()
        else
          SelectCharacter(decardPos);
      end;
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

destructor TCharacter.Destroy();
begin
  WriteLn('Kill ' + name);
  img.Free;
  inherited Destroy;
end;

destructor TCellData.Destroy();
begin
  img.Free;
  inherited Destroy;
end;

procedure TCharacter.ReDraw();
begin
  img.Bitmap.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Resourses\Sprites\' +
    sprite + IntToStr(owner) + '.png');
  if IsSelected then
    DrawOutline(img, TAlphaColors.Yellow, 50);
end;

function decardToCube(pos: Vector2): Vector3;
begin
  Result.x := pos.x - (pos.y + (pos.y and 1)) div 2;
  Result.y := pos.y;
  Result.z := -Result.x - Result.y;
end;

function GetDistance(a, b: Vector3): integer;
begin
  Result := (Abs(a.x - b.x) + Abs(a.y - b.y) + Abs(a.z - b.z)) div 2;
end;

const
  healsBarScale: Vector2 = (x: 50; y: 4);
  healsBarPos: Vector2 = (x: 53; y: 110);

procedure TCharacter.Init(form: TForm);
begin
  healsBar := TImage.Create(form);
  healsBar.Parent := img;
  healsBar.Position.x := healsBarPos.x;
  healsBar.Position.y := healsBarPos.y;

  healsBar.Width := healsBarScale.x;
  healsBar.Height := healsBarScale.y;

  healsBar.Bitmap.Width := healsBarScale.x;
  healsBar.Bitmap.Height := healsBarScale.y;
  healsBar.Bitmap.Canvas.BeginScene();
  healsBar.Bitmap.Canvas.Fill.Color := TAlphaColors.Crimson;
  healsBar.Bitmap.Canvas.FillRect(TRectF.Create(0, 0, healsBarScale.x,
  healsBarScale.y), 0, 0, [], 1);
  healsBar.Bitmap.Canvas.EndScene();
end;

function TCharacter.GetHP(): integer;
begin
  Result := heals;
end;

function TCharacter.RecalculateDamage(h: Integer): Integer;
begin
  result := h - armor;
  if result < 0 then
    result := 0;
end;

procedure TCharacter.SetHP(h: integer);
begin
  if h < heals then          //урон
  begin
    heals := heals - RecalculateDamage(heals - h);
  end
  else
  begin                   //лечение
    if h > maxHp then
      heals := maxHp
    else
      heals := h;
  end;

  healsBar.Bitmap.Width := healsBarScale.x;
  healsBar.Bitmap.Height := healsBarScale.y;
  healsBar.Bitmap.Canvas.BeginScene();
  healsBar.Bitmap.Canvas.Fill.Color := TAlphaColors.Brown;
  healsBar.Bitmap.Canvas.FillRect(TRectF.Create(0, 0, healsBarScale.x,
    healsBarScale.y), 0, 0, [], 1);
  healsBar.Bitmap.Canvas.Fill.Color := TAlphaColors.Crimson;
  healsBar.Bitmap.Canvas.FillRect(TRectF.Create(0, 0,
    Round(healsBarScale.x * heals / maxHp), healsBarScale.y), 0, 0, [], 1);

  healsBar.Bitmap.Canvas.EndScene();

  if heals <= 0 then
  begin
    players[owner].RemoveCharacter(Self);
    GetCell(pos).character := nil;
    Self.Free;
  end;
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
var
  cur: charList;
begin
  cur := characters;
  while cur.next <> nil do
    cur := cur^.next;
  New(cur^.next);
  cur := cur^.next;
  cur^.next := nil;
  cur^.data := c;
end;

procedure TPlayer.RemoveCharacter(c: TCharacter);
var
  cur, temp: charList;
begin
  cur := characters;
  while cur.next^.data <> c do
    cur := cur^.next;
  temp := cur^.next;
  cur^.next := cur^.next^.next;
  Dispose(temp);
end;

procedure TPlayer.OnRoundStart();
var
  curChar: charList;
begin
  curChar := characters^.next;
  while curChar <> nil do
  begin
    with curChar^.data do
    begin
      movePoints := 0;
      Inc(atack.timeAfterUse);
      Inc(skill1.timeAfterUse);
      Inc(skill2.timeAfterUse);
    end;
    curChar := curChar^.next;
  end;
end;

function TCharacter.IsSelected(): boolean;
begin
  Result := (selectedCharacter.x = pos.x) and (selectedCharacter.y = pos.y);
end;

destructor TPlayer.Destroy();
begin
  Dispose(characters);
end;

procedure TSkill.Select(caster: TCellData);
begin
  timeAfterUse := 0;
  CharacterDataVisualisator.ReDraw();
  if GetActionCount() > 0 then
  begin
    SetActionCount(GetActionCount() - 1);
    if hasTarget then
    begin
      targetSelectionMode := True;
      selectedSkill :=self;
      selectedCaster := caster;
      Form2.SkipRound.Enabled := false;
      SelectMap(self.IsCorrectTarget, caster);
    end
    else
    begin
      Use(caster, caster);
    end;
  end;
end;

Constructor TSkill.Create(targetable : boolean);
begin
  hasTarget := targetable;
end;

Procedure TSkill.Clone(other : TSkill);
begin
    inherited Create;
    name := other.name;
    reloadTime:= other.reloadTime;
    hasTarget := other.hasTarget;
end;

Constructor TCharacter.Create(other : TCharacter);
begin
    inherited Create;
    sprite := other.sprite;
    name := other.name;
    maxHp := other.maxHp;
    speed := other.speed;
    armor := other.armor;

    movePoints := 0;

    atack := TSkill.Create();
    skill1 := TSkill.Create();
    skill2 := TSkill.Create();
    atack.Clone(other.atack);
    skill1.Clone(other.skill1);
    skill2.Clone(other.skill2);
end;

Constructor TCharacter.Create();
begin
    inherited Create;
end;
end.
