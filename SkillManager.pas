unit SkillManager;

interface

uses DataTypes, DiceManager;

Type

  SplashAttack = class(TSkill)
  private
    function IsCorrectTarget(caster, target: TCellData): boolean; override;
  public
  var
    radius: integer;
    friendlyFire: boolean;
    damage: DicesCount;
    procedure Use(caster, target: TCellData); override;
  end;

  TargetAttack = class(TSkill)
  private
    function IsCorrectTarget(caster, target: TCellData): boolean; override;
  public
  var
    radius: integer;
    damage: DicesCount;
    procedure Use(caster, target: TCellData); override;
  end;

  SplashHeal = class(TSkill)
  private
    function IsCorrectTarget(caster, target: TCellData): boolean; override;
  public
  var
    radius: integer;
    heals: DicesCount;
    procedure Use(caster, target: TCellData); override;
  end;

  HonorExecution = class(TSkill)
  private
    function IsCorrectTarget(caster, target: TCellData): boolean; override;
  public
  var
    radius: integer;
    damage: DicesCount;
    bonus: DicesCount;
    procedure Use(caster, target: TCellData); override;
  end;

  Teleport = class(TSkill)
  private
    function IsCorrectTarget(caster, target: TCellData): boolean; override;
  public
  var
    radius: integer;
    procedure Use(caster, target: TCellData); override;
  end;

  Shield = class(TSkill)
  private
    function IsCorrectTarget(caster, target: TCellData): boolean; override;
  public
  var
    deltaSpeed : integer;
    deltaArmor : integer;
    deltaDamage : dicesCount;
    armoredSprite : string;

    baseSprite : string;
    isActive : boolean;

    procedure Use(caster, target: TCellData); override;
  end;

  BuildingPlacer = class(TSkill)
  private
    function IsCorrectTarget(caster, target: TCellData): boolean; override;
  public
  var
    radius: integer;
    buildingId : integer;
    procedure Use(caster, target: TCellData); override;
  end;

  FireBall = class(TSkill)
  private
    function IsCorrectTarget(caster, target: TCellData): boolean; override;
  public
  var
    radius : integer;
    damage : DicesCount;
    neigbourDamage : Integer;
    procedure Use(caster, target: TCellData); override;
  end;

implementation

uses CellManager, PlayerManager, CharacterManager, CharacterDataVisualisator, buildingManager;

function SplashAttack.IsCorrectTarget(caster: TCellData;
  target: TCellData): boolean;
var
  dist: integer;
begin
  dist := GetDistance(caster.cubePos, target.cubePos);
  result := (dist > 0) and (dist <= radius);
end;

procedure SplashAttack.Use(caster, target: TCellData);
var
  cur: vector2;
  curDamage: integer;
  curChar: TCharacter;
begin
  curDamage := DropDices(SumDices(damage, caster.character.bonusDices));
  for var i := 0 to GetMapScale().x do
  begin
    for var k := 0 to GetMapScale().y do
    begin
      cur.x := i;
      cur.y := k;
      if GetCell(cur) <> nil then
      begin
        curChar := GetCell(cur).character;
        if IsCorrectTarget(caster, GetCell(cur)) and
          (((curChar <> nil) and (friendlyFire or (GetCell(cur).character.owner
          <> curPlayer))) or ((curChar = nil) and (GetCell(cur).building <>
          nil))) then
        begin
          GetCell(cur).AtackCell(curDamage);
        end;
      end;
    end;
  end;
end;

function TargetAttack.IsCorrectTarget(caster: TCellData;
  target: TCellData): boolean;
var
  dist: integer;
begin
  dist := GetDistance(caster.cubePos, target.cubePos);
  result := (dist > 0) and (dist <= radius) and
    (((target.character <> nil) and (target.character.owner <> curPlayer) or
    ((target.character = nil) and (target.building <> nil) and
    (target.building.owner <> curPlayer))));
end;

procedure TargetAttack.Use(caster, target: TCellData);
var
  curDamage: integer;
begin
  curDamage := DropDices(SumDices(damage, caster.character.bonusDices));
  target.AtackCell(curDamage);
end;

function SplashHeal.IsCorrectTarget(caster: TCellData;
  target: TCellData): boolean;
var
  dist: integer;
begin
  dist := GetDistance(caster.cubePos, target.cubePos);
  result := (dist <= radius) and (target.character.owner = curPlayer);
end;

procedure SplashHeal.Use(caster, target: TCellData);
var
  cur: vector2;
  curHeal: integer;
  curChar: TCharacter;
begin
  curHeal := DropDices(heals);
  for var i := 0 to GetMapScale().x do
  begin
    for var k := 0 to GetMapScale().y do
    begin
      cur.x := i;
      cur.y := k;
      if GetCell(cur) <> nil then
      begin
        curChar := GetCell(cur).character;
        if (curChar <> nil) and IsCorrectTarget(caster, GetCell(cur)) then
        begin
          curChar.HP := curChar.HP + curHeal;
        end;
      end;
    end;
  end;
end;

function HonorExecution.IsCorrectTarget(caster: TCellData;
  target: TCellData): boolean;
var
  dist: integer;
begin
  dist := GetDistance(caster.cubePos, target.cubePos);
  result := (dist > 0) and (dist <= radius) and
    (target.character <> nil) and (target.character.owner <> curPlayer);
end;

procedure HonorExecution.Use(caster, target: TCellData);
var
  curDamage: integer;
  cur: charList;
begin
  curDamage := DropDices(SumDices(damage, caster.character.bonusDices));
  target.AtackCell(curDamage);

  if target.character = nil then
  begin
    cur := currentPlayer.characters;
    var a : charelem;
    a := cur^;
    while cur^.next <> nil do
    begin
      cur := cur^.next;
      cur^.data.bonusDices := SumDices(cur^.data.bonusDices, bonus);
    end;
  end;
end;

function Teleport.IsCorrectTarget(caster: TCellData;
  target: TCellData): boolean;
var
  dist: integer;
begin
  dist := GetDistance(caster.cubePos, target.cubePos);
  result := (dist > 0) and (target.cType <> cBlocked) and (dist <= radius) and (target.character = nil)
  and ((target.building = nil) or (target.building.owner = caster.character.owner));
end;

procedure Teleport.Use(caster, target: TCellData);
begin
  MoveCharacter(caster, target);
end;

function Shield.IsCorrectTarget(caster: TCellData;
  target: TCellData): boolean;
begin
  result := true;
end;

procedure Shield.Use(caster, target: TCellData);
begin
  if not isActive then
  begin
    Inc(caster.character.armor, deltaArmor);
    Inc(caster.character.speed, deltaSpeed);
    caster.character.bonusDices := SumDices(caster.character.bonusDices, deltaDamage);
    caster.character.movePoints := 0;

    baseSprite := caster.character.sprite;
    caster.character.sprite := armoredSprite;
  end
  else
  begin
    Dec(caster.character.armor, deltaArmor);
    Dec(caster.character.speed, deltaSpeed);
    caster.character.bonusDices := SubDices(caster.character.bonusDices, deltaDamage);

    caster.character.sprite := baseSprite;
  end;

    isActive := not isActive;

    caster.character.ReDraw();
    CharacterDataVisualisator.ReDraw();
end;

function BuildingPlacer.IsCorrectTarget(caster: TCellData;
  target: TCellData): boolean;
var
  dist: integer;
begin
  dist := GetDistance(caster.cubePos, target.cubePos);
  result := (dist > 0) and (target.cType <> cBlocked) and (dist <= radius) and (target.building = nil)
  and ((target.building = nil) or (target.building.owner = caster.character.owner));
end;

procedure BuildingPlacer.Use(caster, target: TCellData);
begin
  TryBuild(target, buildingId);
end;

function FireBall.IsCorrectTarget(caster: TCellData;
  target: TCellData): boolean;
var
  dist: integer;
begin
  dist := GetDistance(caster.cubePos, target.cubePos);
  result := (dist > 0) and (dist <= radius) and
    (((target.character <> nil) and (target.character.owner <> curPlayer) or
    ((target.character = nil) and (target.building <> nil) and
    (target.building.owner <> curPlayer))));
end;

const  neignourPositions : array[0..7] of Vector2 =(
  (x : 1; y : 0),
  (x : 1; y : 1),
  (x : 0; y : 1),
  (x : -1; y : 1),
  (x : -1; y : 0),
  (x : -1; y : -1),
  (x : 0; y : -1),
  (x : 1; y : -1));

procedure FireBall.Use(caster, target: TCellData);
var
  cell : TCellData;
  curPos : vector2;
  curDamage: integer;
  nDamage : integer;
begin
  curDamage := DropDices(SumDices(damage, caster.character.bonusDices));
  nDamage := curDamage div neigbourDamage;

  target.AtackCell(curDamage);
  for var pos in neignourPositions do
  begin
    curPos.x := target.decardPos.x + pos.x;
    curPos.y := target.decardPos.y + pos.y;
    cell := GetCell(curPos);
    if cell <> nil then
      cell.AtackCell(nDamage);
  end;
end;

end.
