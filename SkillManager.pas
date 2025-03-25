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

implementation

uses CellManager, PlayerManager, CharacterManager;

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

end.
