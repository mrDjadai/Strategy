unit DataTypes;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Objects,
  FMX.Media, System.Generics.Collections;

const
  DicesTypesCount = 5;

Type
  
  TCount = Array of integer;

  DicesCount = array [0 .. (DicesTypesCount - 1)] of integer;

  skillData = array [0 .. 7] of string;

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
    onDie: Action;
    imageScale: Vector2;
    procedure SetHP(h: integer);
    procedure Init(img: TImage; mHp: integer; scale, position: Vector2;
      dieAction: Action);
  end;

  TCellData = class;

  TSkill = class
  protected
    function IsCorrectTarget(caster, target: TCellData): boolean;
      virtual; abstract;
  public
    audioSource: TMediaPlayer;
    name: string;
    reloadTime: integer;
    timeAfterUse: integer;
    hasTarget: boolean;
    procedure Select(caster: TCellData);
    procedure Use(caster, target: TCellData); virtual; abstract;
    function GetToolTip(): string; virtual; abstract;
    Constructor Create(targetable: boolean); overload;
    procedure PlaySound();
  end;

  TBuilding = class
  private
    destructor Destroy(); override;
  public
    cost: integer;
    name: string;
    maxHp: integer;
    healsBar: THealsContainer;
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

    class function CanBeBuilded(cell : TCellData) : boolean; virtual;
  end;

  TAnimationQueueItem = record
    Source: TCellData;
    Dest: TCellData;
  end;

  TAnimationQueue = TQueue<TAnimationQueueItem>;

  TCharacter = class
  private
  var
    healsBar: THealsContainer;

    procedure Die();
    procedure SetHP(h: integer);
    function GetHP(): integer;
    function RecalculateDamage(h: integer): integer;

  public
  var
    id: integer;
    maxHp: integer;
    owner: byte;
    cost: integer;
    name: string;
    sprite: string;
    pos: Vector2;
    speed: integer;
    armor: integer;
    img: TImage;
    movePoints: integer;
    bonusDices: DicesCount;
    curseRounds: integer;
    animationQueue : TAnimationQueue;
    isAnimating : boolean;

    atack, skill1, skill2: TSkill;

    destructor Destroy(); override;

    property HP: integer read GetHP write SetHP;
    procedure Init(form: TForm; maxHp: integer);
    procedure BuyMP();
    procedure ReDraw();
    function IsSelected(): boolean;
  end;

  TCellType = (cBlocked, cDefault, cDifficult, cDanger);

  TCellData = class
  private
    procedure OnClick(sender: Tobject);
    destructor Destroy(); override;

  var

    selector: TImage;

  public
  var
    img: TImage;
    IsSelected: boolean;
    sprite: string;
    decardPos: Vector2;
    cubePos: Vector3;
    cType: TCellType;
    character: TCharacter;
    building: TBuilding;
    attackBlocker: boolean;

    procedure ReDraw();
    procedure OnEnter();
    procedure OnStay();
    procedure OnExit();
    procedure AtackCell(damage: integer);

    property Image: TImage read img;

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
    playerMoney: integer;
    procedure SetMoney(m: integer);
    destructor Destroy();

  public
  var
    portalCell: TCellData;
    characters: charList;
    boughtCharacters: TCount;
    BuildingsCount: TCount;
    property Money: integer read playermoney write SetMoney;
    procedure AddCharacter(c: TCharacter);
    procedure RemoveCharacter(c: TCharacter);
    procedure Init();
    procedure OnRoundStart();
    function CanPlace(): boolean;
    class function CorrectHalf(caster, target: TCellData) : boolean;
  end;

function decardToCube(pos: Vector2): Vector3;
function cubeToDecard(pos: Vector3): Vector2;
function LerpCubePos(a, b: Vector3; t: real): Vector3;
function GetDistance(a, b: Vector3): integer;

function SumDices(a, b: DicesCount): DicesCount;
function SubDices(a, b: DicesCount): DicesCount;
function LoadDices(s: string): DicesCount;

implementation

uses Drawer, CharacterManager, PlayerManager, CellManager,
  CharacterDataVisualisator, Window, BuildingManager;

class function TPlayer.CorrectHalf(caster, target: TCellData) : boolean;
begin
      result := ((curPlayer = 0) and (target.decardPos.x <= (x - 1) div 2)) or
      ((curPlayer = 1) and (target.decardPos.x > (x) div 2))
end;

Constructor TCellData.Create(x, y: extended; size: integer);
begin
  inherited Create;
  img := TTransparentHitImage.Create(form2);
  img.OnClick := OnClick;
  Image.Parent := form2.Map;

  Image.position.x := x;
  Image.position.y := y;

  Image.Width := size;
  Image.Height := size;
  Image.RotationAngle := 90;
  IsSelected := false;

  selector := TImage.Create(form2);

  selector.Parent := Image;

  selector.position.x := 0;
  selector.position.y := 0;

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
  selector.Visible := IsSelected;
end;

procedure TCellData.OnClick(sender: Tobject);
begin

  if useConsole then
    WriteLn('clicked    ' + sprite + '  at decard  x=', decardPos.x, '  y=',
      decardPos.y, '/cube  x=', cubePos.x, '  y=', cubePos.y, '  z=', cubePos.z,
      '   has character  ', character <> nil);

  if prepareMode then
  begin
    if ((curPlayer = 0) and (decardPos.x <= (x - 1) div 2)) or
      ((curPlayer = 1) and (decardPos.x > (x) div 2)) then
    begin
      if placableBuildingId > -1 then
        TryBuild(Self);
      if placableCharacterId > -1 then
        TryCreateCharacter(Self)
    end
    else
    begin
      form2.IncorrectPlayer.CurrentTime := 0;
      form2.IncorrectPlayer.Play();
    end;
  end
  else
  begin
    if targetSelectionMode then
    begin
      if selectedSkill.IsCorrectTarget(SelectedCaster, Self) then
      begin
        targetSelectionMode := false;
        selectedSkill.PlaySound();
        selectedSkill.Use(SelectedCaster, Self);
        selectedSkill := nil;
        form2.SkipRound.Enabled := true;
        UnselectMap();
      end
      else
      begin
        form2.IncorrectPlayer.CurrentTime := 0;
        form2.IncorrectPlayer.Play();
      end;
    end
    else if selectedSkill = nil then
    begin
      if character = nil then
      begin
        if selectedCharacter.x <> -1 then
        begin
          TryMoveCharacter(GetCell(selectedCharacter), Self);
        end
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
          form2.SelectPlayer.CurrentTime := 0;
          form2.SelectPlayer.Play();
        end
        else
        begin
          form2.IncorrectPlayer.CurrentTime := 0;
          form2.IncorrectPlayer.Play();
        end;
      end;
    end;
  end;
end;

destructor TCharacter.Destroy();
begin
  if useConsole then
    WriteLn('Kill ' + name);

  atack.Free;
  skill1.Free;
  skill2.Free;

  img.Free;
  animationQueue.Free;
  inherited Destroy;
end;

destructor TBuilding.Destroy();
begin
  if useConsole then
    WriteLn('Destroy ' + name);
  img.Free;
  inherited Destroy;
end;

destructor TCellData.Destroy();
begin
  img.Free;
  inherited Destroy;
end;

const outlineWidth = 15;
procedure TCharacter.ReDraw();
begin
  img.Bitmap.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Resourses\Sprites\' +
    sprite + IntToStr(owner) + '.png');
  if IsSelected then
    DrawOutline(img, TAlphaColors.Yellow, outlineWidth)
  else if curseRounds > 0 then
    DrawOutline(img, TAlphaColors.Purple, outlineWidth);
end;

function decardToCube(pos: Vector2): Vector3;
begin
  Result.x := pos.x - (pos.y + (pos.y and 1)) div 2;
  Result.y := pos.y;
  Result.z := -Result.x - Result.y;
end;

function cubeToDecard(pos: Vector3): Vector2;
begin
  Result.x := pos.x + (pos.y + (pos.y and 1)) div 2;
  Result.y := pos.y;
end;

function GetDistance(a, b: Vector3): integer;
begin
  Result := (Abs(a.x - b.x) + Abs(a.y - b.y) + Abs(a.z - b.z)) div 2;
end;

const
  charHealsBarScale: Vector2 = (x: 50; y: 4);
  charHealsBarPos: Vector2 = (x: 53; y: 110);

procedure THealsContainer.Init(img: TImage; mHp: integer;
  scale, position: Vector2; dieAction: Action);
begin
  healsBar := TImage.Create(form2);
  healsBar.Parent := img;
  healsBar.position.x := position.x;
  healsBar.position.y := position.y;


  healsBar.Bitmap.Width := scale.x;
  healsBar.Bitmap.Height := scale.y;
  healsBar.Bitmap.Canvas.BeginScene();
  healsBar.Bitmap.Canvas.Fill.Color := TAlphaColors.Crimson;
  healsBar.Bitmap.Canvas.FillRect(TRectF.Create(0, 0, scale.x, scale.y), 0,
    0, [], 1);
  healsBar.Bitmap.Canvas.EndScene();
  healsBar.BringToFront;
  maxHp := mHp;
  heals := mHp;

  onDie := dieAction;
end;

procedure TCharacter.Init(form: TForm; maxHp: integer);
begin
  healsBar := THealsContainer.Create;
  healsBar.Init(img, maxHp, charHealsBarScale, charHealsBarPos, Self.Die);
  for var i := 0 to Length(bonusDices) - 1 do
    bonusDices[i] := 0;

  isAnimating := false;
  animationQueue:= TAnimationQueue.Create;
end;

function TCharacter.GetHP(): integer;
begin
  Result := healsBar.heals;
end;

function TCharacter.RecalculateDamage(h: integer): integer;
begin
  Result := h - armor;
  if Result < 0 then
    Result := 0;
end;

procedure TCharacter.SetHP(h: integer);
begin
  if h < healsBar.heals then // урон
  begin
    h := healsBar.heals - RecalculateDamage(healsBar.heals - h);
  end;
  healsBar.SetHP(h);
end;

procedure THealsContainer.SetHP(h: integer);
begin
  if h > maxHp then
    heals := maxHp
  else
    heals := h;

  healsBar.Bitmap.Canvas.BeginScene();

  healsBar.Bitmap.Canvas.Fill.Color := TAlphaColors.Brown;
  healsBar.Bitmap.Canvas.FillRect(TRectF.Create(0, 0, healsBar.Bitmap.Width,
    healsBar.Bitmap.Height), 0, 0, [], 1);
  healsBar.Bitmap.Canvas.Fill.Color := TAlphaColors.Crimson;
  healsBar.Bitmap.Canvas.FillRect(TRectF.Create(0, 0,
    healsBar.Bitmap.Width * (heals / maxHp) / healsBar.Scene.GetSceneScale, healsBar.Bitmap.Height), 0, 0, [], 1);

  healsBar.Bitmap.Canvas.EndScene();


  if heals <= 0 then
  begin
    onDie();
    Self.Free;
  end;
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
  BuildingsCount := defaulBuildingsCount;

  SetLength(BuildingsCount, Length(defaulBuildingsCount));
  for var i := 0 to Length(BuildingsCount) - 1 do
    BuildingsCount[i] := defaulBuildingsCount[i];
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

  if characters^.next = nil then
    Lose(Self);
end;

procedure TPlayer.OnRoundStart();
var
  curChar, nextChar: charList;
begin
  curChar := characters^.next;
  while curChar <> nil do
  begin
    with curChar^.data do
    begin
      nextChar := curChar^.next;
      movePoints := 0;
      Inc(atack.timeAfterUse);
      Inc(skill1.timeAfterUse);
      Inc(skill2.timeAfterUse);
      GetCell(pos).OnStay();

      if curseRounds > 0 then
      begin
        Dec(curseRounds);
        ReDraw();
      end;
    end;

    curChar := nextChar;
  end;
end;

function TCharacter.IsSelected(): boolean;
begin
  Result := (selectedCharacter.x = pos.x) and (selectedCharacter.y = pos.y);
end;

destructor TPlayer.Destroy();
var
  current, temp: charList;
begin
 current := characters;

  while current <> nil do
  begin
    temp := current;
    current := current^.next;
    Dispose(temp);
  end;
end;

procedure TSkill.Select(caster: TCellData);
begin
  if GetActionCount() > 0 then
  begin
    timeAfterUse := 0;
    CharacterDataVisualisator.ReDraw();
    SetActionCount(GetActionCount() - 1);
    if hasTarget then
    begin
      targetSelectionMode := true;
      selectedSkill := Self;
      SelectedCaster := caster;
      form2.SkipRound.Enabled := false;
      SelectMap(Self.IsCorrectTarget, caster);
    end
    else
    begin
      PlaySound();
      Use(caster, caster);
    end;
  end;
end;

Constructor TSkill.Create(targetable: boolean);
begin
  hasTarget := targetable;
end;

const
  buildingHealsBarScale: Vector2 = (x: 60; y: 10);
  buildingHealsBarPos: Vector2 = (x: 45; y: 40);

procedure TBuilding.OnBuild(c: TCellData; ow: integer);
begin
  cell := c;
  cell.building := Self;
  owner := ow;
  form2.BuildingPlacePlayer.CurrentTime := 0;
  form2.BuildingPlacePlayer.Play();
  healsBar := THealsContainer.Create;
  healsBar.Init(img, maxHp, buildingHealsBarScale, buildingHealsBarPos,
    OnDemolish);
  healsBar.SetHP(maxHp);
end;

procedure TBuilding.OnDemolish();
begin
  form2.DemolishPlayer.CurrentTime := 0;
  form2.DemolishPlayer.Play();
  if curPlayer <> owner then
    currentPlayer.Money := currentPlayer.Money + cost;
  GetCell(pos).building := nil;
  Self.Free;
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
  if (cType = cDanger) and (character <> nil) then
    character.HP := character.HP - dangerCellDamage;
end;

procedure TBuilding.ReDraw();
begin
  img.Bitmap.LoadFromFile(ExtractFilePath(ParamStr(0)) + 'Resourses\Sprites\' +
    sprite + IntToStr(owner) + '.png');
end;

procedure TCharacter.Die();
begin
  players[owner].RemoveCharacter(Self);
  GetCell(pos).character := nil;
  form2.DiePlayer.CurrentTime := 0;
  form2.DiePlayer.Play();
  if curPlayer <> owner then
    currentPlayer.SetMoney(currentPlayer.Money + Round( addedMoneyMultiplier * cost));
  Self.Free;
end;

procedure TCellData.AtackCell(damage: integer);
begin
  if character <> nil then
  begin
    if character.curseRounds > 0 then
      damage := Round(damage * curseDamageMultiplier);
    character.HP := character.HP - damage;
  end
  else
  begin
    if building <> nil then
      building.healsBar.SetHP(building.healsBar.heals - damage);
  end;
end;

procedure TPlayer.SetMoney(m: integer);
begin
  playerMoney := m;
  form2.MoneyText.Text := IntToStr(m);
end;

function TPlayer.CanPlace(): boolean;
begin
  Result := false;

  for var a in boughtCharacters do
    if a > 0 then
      Result := true;

  if Result = false then
    for var a in BuildingsCount do
      if a > 0 then
        Result := true;
end;

function SumDices(a, b: DicesCount): DicesCount;
begin
  for var i := 0 to Length(a) - 1 do
    Result[i] := a[i] + b[i];
end;

function SubDices(a, b: DicesCount): DicesCount;
begin
  for var i := 0 to Length(a) - 1 do
    Result[i] := a[i] - b[i];
end;

function LoadDices(s: string): DicesCount;
begin

  for var i := 0 to Length(Result) - 1 do
    Result[i] := 0;
  for var i := 1 to Length(s) do
    Result[i - 1] := Ord(s[i]) - Ord('0');
end;

procedure TSkill.PlaySound();
begin
  audioSource.Volume := 1;
  audioSource.CurrentTime := 0;
  audioSource.Play();
end;

function LerpCubePos(a, b: Vector3; t: real): Vector3;
begin
  Result.x := a.x + Round(t * (b.x - a.x));
  Result.y := a.y + Round(t * (b.y - a.y));
  Result.z := a.z + Round(t * (b.z - a.z));
end;

class function TBuilding.CanBeBuilded(cell : TCellData) : boolean;
begin
  result := true;
end;

end.
