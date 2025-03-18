unit SkillManager;

interface

uses DataTypes, DiceManager;

Type

SplashAttack = class(TSkill)
  private
    constructor OnCreate();
    function IsCorrectTarget(caster, target: TCellData) : boolean; override;
  public
    var
      radius : integer;
      friendlyFire : boolean;
      damage : DicesCount;
    procedure Use(caster, target: TCellData); override;
end;

implementation

uses CellManager, PlayerManager;

constructor SplashAttack.OnCreate;
begin
  hasTarget := false;
end;

function SplashAttack.IsCorrectTarget(caster: TCellData; target: TCellData): Boolean;
var dist : integer;
begin
  dist := GetDistance(caster.cubePos, target.cubePos);
  result := (dist > 0) and (dist <= radius);
end;

procedure SplashAttack.Use(caster, target: TCellData);
var cur : vector2;
  curDamage : integer;
  curChar : TCharacter;
begin
  curDamage:= DropDices(damage);
  for var i := 0 to GetMapScale().x do
    begin
      for var k := 0 to GetMapScale().y do
      begin
        cur.x := i;
        cur.y := k;
        if GetCell(cur) <> nil then
        begin
          curChar := GetCell(cur).character;
          if (curChar <> nil) and  IsCorrectTarget(caster, GetCell(cur))
          and (friendlyFire or (GetCell(cur).character.owner <> curPlayer)) then
          begin
            curChar.HP := curChar.HP - curDamage;
          end;
        end;
      end;
    end;
  end;
end.
