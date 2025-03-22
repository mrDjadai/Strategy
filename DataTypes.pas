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

  Action = procedure of object;

  THealsContainer = class
  public
    healsBar: TImage;
    heals: integer;
    maxHp: integer;
    onDie : action;
    imageScale : vector2;
    procedure SetHP(h: integer);
    function GetHP(): integer;
    procedure Init(img : TImage; mHp : integer; scale, position : vector2; dieAction : Action);
  end;

  TCellData = class;

  TSkill = class
  protected
    function IsCorrectTarget(caster, target: TCellData): boolean;
      virtual; abstract;
  public
    name: string;
    reloadTime: integer;
    timeAfterUse: integer;
    hasTarget: boolean;
    procedure Select(caster: TCellData);
    procedure Use(caster, target: TCellData); virtual; abstract;
    Constructor Create(targetable: boolean); overload;
  end;

  TBuilding = class
  private
      destructor Destroy(); override;
  public
    cost : integer;
    name : string;
    maxHp : integer;
    healsBar : THealsContainer;
    cell: TCellData;
    owner: integer;
    pos: Vector2;
    img: TImage;
    sprite: string;
    procedure OnBuild(c: TCellData; ow: integer); virtual;
    procedure OnDemolish(); virtual;
    procedure OnEnter(); virtual;
    procedure OnStay(); virtual;
    procedure OnExit(); virtual;
    procedure ReDraw();
  end;

  TCharacter = class
  private
  var
    healsBar : THealsContainer;

    procedure Die();
    procedure SetHP(h: integer);
    function GetHP(): integer;
    function RecalculateDamage(h: integer): integer;

  public
  var
    maxHp: integer;
    owner: byte;
    name: string;
    sprite: string;
    pos: Vector2;
    speed: integer;
    armor: integer;
    img: TImage;
    movePoints: integer;

    atack, skill1, skill2: TSkill;

    destructor Destroy(); override;

    property HP: integer read GetHP write SetHP;
    procedure Init(form: TForm; maxHP : Integer);
    procedure ResetMP();
    procedure BuyMP();
    procedure ReDraw();
    function IsSelected(): boolean;

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
    img: TImage;
    selector: TImage;

  public
  var
    IsSelected: boolean;
    sprite: string;
    decardPos: Vector2;
    cubePos: Vector3;
    cType: TCellType;
    character: TCharacter;
    building: TBuilding;

    procedure ReDraw();
    procedure OnEnter();
    procedure OnStay();
    procedure OnExit();
    procedure AtackCell(damage : integer);

    property Image: TImage read GetImage write SetImage;

    Constructor Create(x, y: extended; size: integer);

  End;

  charList = ^charElem;

  charElem = Record
    data: TCharacter;
    next: charList;
  End;

  TPlayer = class
  private
  var
    playerMoney : integer;
    procedure SetMoney(m : integer);
    function GetMoney() : integer;
    destructor Destroy();

  var
    characters: charList;
  public
  var
    property Money: integer read GetMoney write SetMoney;
    procedure AddCharacter(c: TCharacter);
    procedure RemoveCharacter(c: TCharacter);
    procedure Init();
    procedure OnRoundStart();
  end;

function decardToCube(pos: Vector2): Vector3;

function GetDistance(a, b: Vector3): integer;

implementation

uses Drawer, CharacterManager, PlayerManager, CellManager,
  CharacterDataVisualisator, Window, BuildingManager;

Constructor TCellData.Create(x, y: extended; size: integer);
begin
  inherited Create;
  Image := TImage.Create(form2);

  Image.Parent := form2.Map;

  Image.Position.x := x;
  Image.Position.y := y;

  Image.Width := size;
  Image.Height := size;
  Image.RotationAngle := 90;
  IsSelected := false;

  selector := TImage.Create(form2);

  selector.Parent := Image;

  selector.Position.x := 0;
  selector.Position.y := 0;

  selector.Width := size;
  selector.Height := size;
  selector.Bitmap.LoadFromFile(ExtractFilePath(ParamStr(0)) +
    'Resourses\Sprites\cellSelector.png');
  selector.HitTest := false;
  selector.Visible := false;
end;

procedure TCellData.ReDraw();
var
  cBitmap: TBitMap;
begin
  img.Bitmap.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Resourses\Sprites\' +
    sprite + '.png');
  selector.Visible := IsSelected;
  if character <> nil then
    character.ReDraw();

  if building <> nil then
    building.ReDraw();
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
      form2.SkipRound.Enabled := true;
      UnselectMap();
    end;
  end
  else if selectedSkill = nil then
  begin
    if character = nil then // если режим предварительной расстановки
    begin
      if selectedCharacter.x = -1 then
      begin
        TryBuild(Self);
        TryCreateCharacter(Self)
      end
      else
        TryMoveCharacter(GetCell(selectedCharacter), self);
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

destructor TBuilding.Destroy();
begin
  WriteLn('Destroy ' + name);
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
  charHealsBarScale: Vector2 = (x: 50; y: 4);
  charHealsBarPos: Vector2 = (x: 53; y: 110);

procedure THealsContainer.Init(img : TImage; mHp : integer; scale, position : vector2; dieAction : Action);
begin
  healsBar := TImage.Create(form2);
  healsBar.Parent := img;
  healsBar.Position.x := position.x;
  healsBar.Position.y := position.y;

  healsBar.Width := scale.x;
  healsBar.Height := scale.y;

  healsBar.Bitmap.Width := scale.x;
  healsBar.Bitmap.Height := scale.y;
  healsBar.Bitmap.Canvas.BeginScene();
  healsBar.Bitmap.Canvas.Fill.Color := TAlphaColors.Crimson;
  healsBar.Bitmap.Canvas.FillRect(TRectF.Create(0, 0, scale.x,
    scale.y), 0, 0, [], 1);
  healsBar.Bitmap.Canvas.EndScene();
  healsBar.BringToFront;
  maxHp := mHP;
  heals := mHP;
  imageScale := scale;
  onDie := dieAction;
end;

procedure TCharacter.Init(form: TForm; maxHP : Integer);
begin
  healsBar := THealsContainer.Create;
  healsBar.Init(img, maxHP, charHealsBarScale, charHealsBarPos, self.Die);
end;

function TCharacter.GetHP(): integer;
begin
  Result := healsBar.GetHP();
end;

function THealsContainer.GetHP(): integer;
begin
  Result := heals;
end;

function TCharacter.RecalculateDamage(h: integer): integer;
begin
  Result := h - armor;
  if Result < 0 then
    Result := 0;
end;

procedure TCharacter.SetHP(h: integer);
begin
  if h < healsBar.GetHP() then // урон
  begin
    h := healsBar.GetHP() - RecalculateDamage(healsBar.GetHP() - h);
  end;
  healsBar.SetHP(h);
end;

procedure THealsContainer.SetHP(h: integer);
begin
  if h > maxHp then
    heals := maxHp
  else
    heals := h;

  healsBar.Bitmap.Width := imageScale.x;
  healsBar.Bitmap.Height := imageScale.y;

  healsBar.Bitmap.Canvas.BeginScene();

  healsBar.Bitmap.Canvas.Fill.Color := TAlphaColors.Brown;
  healsBar.Bitmap.Canvas.FillRect(TRectF.Create(0, 0, imageScale.x,
    imageScale.y), 0, 0, [], 1);
  healsBar.Bitmap.Canvas.Fill.Color := TAlphaColors.Crimson;
  healsBar.Bitmap.Canvas.FillRect(TRectF.Create(0, 0,
    Round(imageScale.x * (heals / maxHp)), imageScale.y), 0, 0, [], 1);

  healsBar.Bitmap.Canvas.EndScene();

  if heals <= 0 then
  begin
    OnDie();
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
      GetCell(pos).OnStay();
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
      targetSelectionMode := true;
      selectedSkill := self;
      SelectedCaster := caster;
      form2.SkipRound.Enabled := false;
      SelectMap(self.IsCorrectTarget, caster);
    end
    else
    begin
      Use(caster, caster);
    end;
  end;
end;

Constructor TSkill.Create(targetable: boolean);
begin
  hasTarget := targetable;
end;

Constructor TCharacter.Create();
begin
  inherited Create;
end;

const
  buildingHealsBarScale: Vector2 = (x: 60; y: 10);
  buildingHealsBarPos: Vector2 = (x: 45; y: 40);

procedure TBuilding.OnBuild(c: TCellData; ow: integer);
begin
  cell := c;
  cell.building := self;
  owner := ow;

  healsBar := THealsContainer.Create;
  healsBar.Init(img, maxHP, buildingHealsBarScale, buildingHealsBarPos, OnDemolish);
  healsBar.SetHP(maxHp);
end;

procedure TBuilding.OnDemolish();
begin
  if curPlayer <> owner then
    currentPlayer.Money := currentPlayer.Money + cost;
  GetCell(pos).building := nil;
  self.Free;
end;

procedure TBuilding.OnEnter();
begin
end;

procedure TBuilding.OnExit();
begin
end;

procedure TBuilding.OnStay();
begin
end;

procedure TCellData.OnEnter();
begin
  if building <> nil then
    building.OnEnter();
end;

procedure TCellData.OnExit();
begin
  if building <> nil then
    building.OnExit();
end;

procedure TCellData.OnStay();
begin
  if building <> nil then
    building.OnStay();
end;

procedure TBuilding.ReDraw();
begin
  img.Bitmap.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Resourses\Sprites\' +
    sprite + IntToStr(owner) + '.png');
end;

procedure TCharacter.Die();
begin
  players[owner].RemoveCharacter(self);
  GetCell(pos).character := nil;
  self.Free;
end;

procedure TCellData.AtackCell(damage : integer);
begin
  if character <> nil then
  begin
    character.HP := character.HP - damage;
  end
  else
  begin
    building.HealsBar.SetHP(building.HealsBar.GetHP() - damage);
  end;
end;

procedure TPlayer.SetMoney(m : integer);
begin
  playerMoney := m;
  form2.MoneyText.Text := IntToStr(m);
end;

function TPlayer.GetMoney() : integer;
begin
  result := playerMoney;
end;

end.
