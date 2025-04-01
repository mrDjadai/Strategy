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
    isBlockable: boolean;
    procedure Use(caster, target: TCellData); override;
    function GetToolTip(): string; override;
  end;

  TargetAttack = class(TSkill)
  private
    function IsCorrectTarget(caster, target: TCellData): boolean; override;
  public
  var
    radius: integer;
    damage: DicesCount;
    isBlockable: boolean;
    procedure Use(caster, target: TCellData); override;
    function GetToolTip(): string; override;
  end;

  SplashHeal = class(TSkill)
  private
    function IsCorrectTarget(caster, target: TCellData): boolean; override;
  public
  var
    radius: integer;
    heals: DicesCount;
    isBlockable: boolean;
    procedure Use(caster, target: TCellData); override;
    function GetToolTip(): string; override;
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
    function GetToolTip(): string; override;
  end;

  Teleport = class(TSkill)
  private
    function IsCorrectTarget(caster, target: TCellData): boolean; override;
  public
  var
    radius: integer;
    procedure Use(caster, target: TCellData); override;
    function GetToolTip(): string; override;
  end;

  Shield = class(TSkill)
  private
    function IsCorrectTarget(caster, target: TCellData): boolean; override;
  public
  var
    deltaSpeed: integer;
    deltaArmor: integer;
    deltaDamage: DicesCount;
    armoredSprite: string;

    baseSprite: string;
    isActive: boolean;

    procedure Use(caster, target: TCellData); override;
    function GetToolTip(): string; override;
  end;

  BuildingPlacer = class(TSkill)
  private
    function IsCorrectTarget(caster, target: TCellData): boolean; override;
  public
  var
    radius: integer;
    buildingId: integer;
    procedure Use(caster, target: TCellData); override;
    function GetToolTip(): string; override;
  end;

  FireBall = class(TSkill)
  private
    function IsCorrectTarget(caster, target: TCellData): boolean; override;
  public
  var
    radius: integer;
    damage: DicesCount;
    neigbourDamage: integer;
    procedure Use(caster, target: TCellData); override;
    function GetToolTip(): string; override;
  end;

  Curse = class(TSkill)
  private
    function IsCorrectTarget(caster, target: TCellData): boolean; override;
  public
  var
    radius: integer;
    duration: integer;
    procedure Use(caster, target: TCellData); override;
    function GetToolTip(): string; override;
  end;

implementation

uses CellManager, PlayerManager, CharacterManager, CharacterDataVisualisator,
  buildingManager, System.SysUtils;

function IsPointVisible(a, b: TCellData): boolean;
begin
  result := true;

  for var cell in GetCellBetween(a, b) do
    if cell.attackBlocker then
      result := false;
end;

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
          <> curPlayer))) or ((curChar = nil) and (GetCell(cur).building <> nil)
          )) and ((isBlockable = false) or IsPointVisible(caster, target)) then
        begin
          GetCell(cur).AtackCell(curDamage);
        end;
      end;
    end;
  end;
  CharacterDataVisualisator.ReDraw();
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
    (target.building.owner <> curPlayer)))) and
    ((isBlockable = false) or IsPointVisible(caster, target));
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
        if (curChar <> nil) and IsCorrectTarget(caster, GetCell(cur)) and
          ((isBlockable = false) or IsPointVisible(caster, target)) then
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
  result := (dist > 0) and (dist <= radius) and (target.character <> nil) and
    (target.character.owner <> curPlayer);
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
    var
      a: charelem;
    a := cur^;
    while cur^.next <> nil do
    begin
      cur := cur^.next;
      cur^.data.bonusDices := SumDices(cur^.data.bonusDices, bonus);
    end;
  end;
  CharacterDataVisualisator.ReDraw();
end;

function Teleport.IsCorrectTarget(caster: TCellData; target: TCellData)
  : boolean;
var
  dist: integer;
begin
  dist := GetDistance(caster.cubePos, target.cubePos);
  result := (dist > 0) and (target.cType <> cBlocked) and (dist <= radius) and
    (target.character = nil) and
    ((target.building = nil) or
    (target.building.owner = caster.character.owner));
end;

procedure Teleport.Use(caster, target: TCellData);
begin
  MoveCharacter(caster, target);
end;

function Shield.IsCorrectTarget(caster: TCellData; target: TCellData): boolean;
begin
  result := true;
end;

procedure Shield.Use(caster, target: TCellData);
begin
  if not isActive then
  begin
    Inc(caster.character.armor, deltaArmor);
    Inc(caster.character.speed, deltaSpeed);
    caster.character.bonusDices := SumDices(caster.character.bonusDices,
      deltaDamage);
    caster.character.movePoints := 0;

    baseSprite := caster.character.sprite;
    caster.character.sprite := armoredSprite;
  end
  else
  begin
    Dec(caster.character.armor, deltaArmor);
    Dec(caster.character.speed, deltaSpeed);
    caster.character.bonusDices := SubDices(caster.character.bonusDices,
      deltaDamage);

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
  result := (dist > 0) and (target.cType <> cBlocked) and (dist <= radius) and
    (target.building = nil) and
    ((target.building = nil) or
    (target.building.owner = caster.character.owner));
end;

procedure BuildingPlacer.Use(caster, target: TCellData);
begin
  TryBuild(target, buildingId);
end;

function FireBall.IsCorrectTarget(caster: TCellData; target: TCellData)
  : boolean;
var
  dist: integer;
begin
  dist := GetDistance(caster.cubePos, target.cubePos);
  result := (dist > 0) and (dist <= radius) and
    (((target.character <> nil) and (target.character.owner <> curPlayer) or
    ((target.character = nil) and (target.building <> nil) and
    (target.building.owner <> curPlayer))));
end;

const
  neignourPositions: array [0 .. 7] of vector2 = ((x: 1; y: 0), (x: 1; y: 1),
    (x: 0; y: 1), (x: - 1; y: 1), (x: - 1; y: 0), (x: - 1; y: - 1), (x: 0;
    y: - 1), (x: 1; y: - 1));

procedure FireBall.Use(caster, target: TCellData);
var
  cell: TCellData;
  curPos: vector2;
  curDamage: integer;
  nDamage: integer;
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

function Curse.IsCorrectTarget(caster: TCellData; target: TCellData): boolean;
var
  dist: integer;
begin
  dist := GetDistance(caster.cubePos, target.cubePos);
  result := (dist <= radius) and (target.character <> nil) and
    (target.character.owner <> curPlayer);
end;

procedure Curse.Use(caster, target: TCellData);
begin
  Inc(target.character.curseRounds, duration);
  target.character.ReDraw();
end;

function SplashAttack.GetToolTip(): string;
begin
  if friendlyFire then
  begin
    if radius = 1 then
      result := 'Наносит урон ' + GetCubeText(damage) + ' всем в радиусе ' +
        IntToStr(radius) + ' клетки'
    else
      result := 'Наносит урон ' + GetCubeText(damage) + ' всем в радиусе ' +
        IntToStr(radius) + ' клеток';
  end
  else
  begin
    begin
      if radius = 1 then
        result := 'Наносит урон ' + GetCubeText(damage) +
          ' всем противникам в радиусе ' + IntToStr(radius) + ' клетки'
      else
        result := 'Наносит урон ' + GetCubeText(damage) +
          ' всем противникам в радиусе ' + IntToStr(radius) + ' клеток';
    end;
  end;
  if isBlockable then
    result := result + '. Блокируется укрытиями';
end;

function TargetAttack.GetToolTip(): string;
begin
  if radius = 1 then
    result := 'Наносит урон ' + GetCubeText(damage) +
      ' выбранному противнику в радиусе ' + IntToStr(radius) + ' клетки'
  else
    result := 'Наносит урон ' + GetCubeText(damage) +
      ' выбранному противнику в радиусе ' + IntToStr(radius) + ' клеток';
      
  if isBlockable = false then
    result := result + '. Атакует навесом';
end;

function SplashHeal.GetToolTip(): string;
begin
  if radius = 1 then
    result := 'Восстанавливает здоровье ' + GetCubeText(heals) +
      ' всем союзникам в радиусе ' + IntToStr(radius) + ' клетки'
  else
    result := 'Восстанавливает здоровье ' + GetCubeText(heals) +
      ' всем союзникам в радиусе ' + IntToStr(radius) + ' клеток';
      
  if isBlockable then
    result := result + '. Блокируется укрытиями';
end;

function HonorExecution.GetToolTip(): string;
begin
  if radius = 1 then
    result := 'Наносит урон ' + GetCubeText(damage) +
      ' выбранному противнику в радиусе ' + IntToStr(radius) +
      ' клетки. Если противник умрёт то все союзники получат бонус к атаке' +
      GetCubeText(bonus)
  else
    result := 'Наносит урон ' + GetCubeText(damage) +
      ' выбранному противнику в радиусе ' + IntToStr(radius) +
      ' клеток. Если противник умрёт то все союзники получат бонус к атаке' +
      GetCubeText(bonus);
end;

function Teleport.GetToolTip(): string;
begin
  if radius = 1 then
    result := 'Вы перемещаетесь в любую позицию в радиусе ' + IntToStr(radius) +
      ' клетки'
  else
    result := 'Вы перемещаетесь в любую позицию в радиусе ' + IntToStr(radius) +
      ' клеток'
end;

function Shield.GetToolTip(): string;
begin
  if isActive then
    result := 'Понижает защиту на ' + IntToStr(deltaArmor) +
      '. Повышает скорость на ' + IntToStr(-deltaSpeed) + '. Понижает урон на '
      + GetCubeText(deltaDamage)
  else
    result := 'Повышает защиту на ' + IntToStr(deltaArmor) +
      '. Понижает скорость на ' + IntToStr(-deltaSpeed) + '. Повышает урон на '
      + GetCubeText(deltaDamage);
end;

function FireBall.GetToolTip(): string;
begin
  if radius = 1 then
    result := 'Наносит урон ' + GetCubeText(damage) +
      ' выбранному противнику в радиусе ' + IntToStr(radius) +
      ' клетки. И всем его соседям в ' + IntToStr(neigbourDamage) +
      ' раза меньше урона'
  else
    result := 'Наносит урон ' + GetCubeText(damage) +
      ' выбранному противнику в радиусе ' + IntToStr(radius) +
      ' клеток. И всем его соседям в ' + IntToStr(neigbourDamage) +
      ' раза меньше урона'
end;

function BuildingPlacer.GetToolTip(): string;
begin
  if radius = 1 then
    result := 'Ставит ' + GetBuildingName(buildingId) +
      ' на выбранную клетку в радиусе ' + IntToStr(radius) + ' клетки'
  else
    result := 'Ставит ' + GetBuildingName(buildingId) +
      ' на выбранную клетку в радиусе ' + IntToStr(radius) + ' клеток'
end;

function Curse.GetToolTip(): string;
begin
  if radius = 1 then
    result := 'Повышает входящий урон выбранному противнику в радиусе ' +
      IntToStr(radius) + ' клетки в ' + FloatToStr(curseDamageMultiplier) +
      ' раза на ' + IntToStr(duration) + ' раунда'
  else
    result := 'Повышает входящий урон выбранному противнику в радиусе ' +
      IntToStr(radius) + ' клеток в ' + FloatToStr(curseDamageMultiplier) +
      ' раза на ' + IntToStr(duration) + ' раунда'
end;

end.
