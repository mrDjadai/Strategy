unit CharacterManager;

interface

uses DataTypes, FMX.Objects, System.Classes, FMX.Controls, FMX.Ani,
  System.Types;

procedure SelectCharacter(pos: Vector2);

procedure UnselectCharacter();

procedure TryMoveCharacter(source, dest: TCellData);

function TryCreateCharacter(cell: TCellData): boolean;

function IsCorrectDest(character: TCharacter; dest: TCellData)
  : boolean; overload;

function IsCorrectDest(src, dest: TCellData): boolean; overload;

procedure Init();

function GetCharList(): TStringDynArray;

procedure MoveCharacter(source, dest: TCellData);

implementation

uses PlayerManager, CellManager, System.SysUtils, Window,
  CharacterDataVisualisator, System.IOUtils, SkillManager, DiceManager;

const
  movingDuration = 0.5;

var
  charTypes: TStringDynArray;

type
  skillData = array [0 .. 6] of string;

function LoadSkill(data: skillData): TSkill;
var
  line: string;
  id: integer;
begin
  case StrToInt(data[0]) of
    0:
      begin
        var
          splash: SplashAttack;
        splash := SplashAttack.Create;
        splash.radius := StrToInt(data[3]);
        var
          damage: dicesCount;
        damage := LoadDices(data[4]);
        splash.damage := damage;
        splash.friendlyFire := data[5] = '+';
        result := splash;
        result.hasTarget := false;
      end;
    1:
      begin
        var
          target: TargetAttack;
        target := TargetAttack.Create;
        target.radius := StrToInt(data[3]);
        var
          damage: dicesCount;
        damage := LoadDices(data[4]);
        target.damage := damage;
        result := target;
        result.hasTarget := true;
      end;
    2:
      begin
        var
          heal: SplashHeal;
        heal := SplashHeal.Create;
        heal.radius := StrToInt(data[3]);
        var
        damage := LoadDices(data[4]);
        heal.heals := damage;
        result := heal;
        result.hasTarget := false;
      end;
    3:
      begin
        var
          ex: HonorExecution;
        ex := HonorExecution.Create;
        ex.radius := StrToInt(data[3]);
        var
          damage: dicesCount;
        damage := LoadDices(data[4]);
        ex.bonus := damage;

        damage := LoadDices(data[5]);
        ex.damage := damage;

        result := ex;
        result.hasTarget := true;
      end;
    4:
      begin
        var
          tp: Teleport;
        tp := Teleport.Create;
        tp.radius := StrToInt(data[3]);
        result := tp;
        result.hasTarget := true;
      end;
    5:
      begin
        var
          sh: Shield;
        sh := Shield.Create;
        sh.deltaArmor := StrToInt(data[3]);
        sh.deltaSpeed := StrToInt(data[4]);
        sh.deltaDamage := LoadDices(data[5]);
        sh.armoredSprite := data[6];
        sh.isActive := false;

        result := sh;
        result.hasTarget := false;
      end;
  end;

  result.name := data[1];
  result.reloadTime := StrToInt(data[2]);
end;

function LoadCharacter(FileName: string): TCharacter;
var
  line: string;
  f: TextFile;
  data: skillData;
begin
  result := TCharacter.Create;

  AssignFile(f, FileName);
  Reset(f);
  readln(f, line);
  result.name := line;

  readln(f, line);
  result.sprite := line;

  readln(f, line);
  result.cost := StrToInt(line);

  readln(f, line);
  result.maxHp := StrToInt(line);

  readln(f, line);
  result.speed := StrToInt(line);

  readln(f, line);
  result.armor := StrToInt(line);

  for var i := 0 to High(skillData) do
    readln(f, data[i]);
  result.atack := LoadSkill(data);
  for var i := 0 to High(skillData) do
    readln(f, data[i]);
  result.skill1 := LoadSkill(data);
  for var i := 0 to High(skillData) do
    readln(f, data[i]);
  result.skill2 := LoadSkill(data);

  CloseFile(f);
end;

procedure Init();
var
  Files: TStringDynArray;
begin
  SetLength(charTypes, 0);
  Files := TDirectory.GetFiles(ExtractFilePath(ParamStr(0)) +
    'Resourses\Characters\');
  for var FileName in Files do
  begin
    if true then // Добавить валидацию
    begin
      SetLength(charTypes, Length(charTypes) + 1);
      charTypes[Length(charTypes) - 1] := FileName;
    end;
  end;
end;

procedure SelectCharacter(pos: Vector2);
var
  last: Vector2;
begin
  last := selectedCharacter;

  selectedCharacter := pos;
  SetCaharcter(GetCell(pos).character);
  GetCell(pos).character.ReDraw();

  if last.x <> -1 then
  begin
    GetCell(last).character.ReDraw();
  end;
end;

procedure UnselectCharacter();
var
  last: Vector2;
begin
  if selectedCharacter.x <> -1 then
  begin
    last := selectedCharacter;
    selectedCharacter.x := -1;
    selectedCharacter.y := -1;
    GetCell(last).character.ReDraw();
    SetCaharcter(nil);
  end;
end;

procedure AnimationFinished(Sender: TObject);
begin
  if Sender is TFloatAnimation then
    TFloatAnimation(Sender).Free;
end;

procedure MoveCharacter(source, dest: TCellData);
var
  AnimX, AnimY: TFloatAnimation;
  Image: TImage;
begin
  Image := source.character.img;
  selectedCharacter := dest.decardPos;

  dest.character := source.character;
  dest.character.pos := dest.decardPos;

  source.OnExit();
  dest.OnEnter();
  source.character := nil;

  // Анимация

  AnimX := TFloatAnimation.Create(Image);
  AnimX.Parent := Image;
  AnimX.PropertyName := 'Position.X';
  AnimX.StartValue := source.Image.Position.x;
  AnimX.StopValue := dest.Image.Position.x;
  AnimX.Duration := movingDuration;
  AnimX.Start;

  AnimY := TFloatAnimation.Create(Image);
  AnimY.Parent := Image;
  AnimY.PropertyName := 'Position.Y';
  AnimY.StartValue := source.Image.Position.y;
  AnimY.StopValue := dest.Image.Position.y;
  AnimY.Duration := movingDuration;
  AnimY.Start;

  AnimX.OnFinish := Form2.DeleteAnimation;
  AnimY.OnFinish := Form2.DeleteAnimation;
end;

procedure TryMoveCharacter(source, dest: TCellData);
begin
  if IsCorrectDest(source, dest) then
  begin
    MoveCharacter(source, dest);
    if dest.cType = cDifficult then
      dest.character.movePoints := dest.character.movePoints - 2
    else
      dest.character.movePoints := dest.character.movePoints - 1;
  end;
  ReDraw();
end;

const
  characterOffset: Vector2 = (x: - 14; y: 0);

procedure CreateCharacter(cell: TCellData; charID: integer);
begin
  var
    c: TCharacter;
  c := LoadCharacter(charTypes[charID]);
  c.owner := curPlayer;

  c.pos := cell.decardPos;
  cell.character := c;

  var
    myImage: TImage;
  myImage := TImage.Create(TComponent(cell.Image).owner);
  myImage.Parent := TControl(cell.Image).Parent;

  myImage.Position.x := cell.Image.Position.x + characterOffset.x;
  myImage.Position.y := cell.Image.Position.y + characterOffset.y;
  myImage.Height := cell.Image.Height;
  myImage.Width := cell.Image.Width;
  myImage.BringToFront();

  myImage.HitTest := false;
  c.img := myImage;

  c.Init(Form2, c.maxHp);
  cell.ReDraw();

  c.hp := c.maxHp;

  currentPlayer.AddCharacter(c);
end;

function TryCreateCharacter(cell: TCellData): boolean;
begin
  result := false;

  if (cell.cType <> cBlocked) and (cell.character = nil) then
  begin
    CreateCharacter(cell, placableCharacterId);
    placableCharacterId := -1;
    result := true;

    TryEndPrepare();
  end;
end;

function IsCorrectDest(character: TCharacter; dest: TCellData)
  : boolean; overload;
begin
  if (GetDistance(decardToCube(character.pos), dest.cubePos) = 1) and
    (dest.character = nil) then
    case dest.cType of
      cBlocked:
        result := false;
      cDefault:
        result := character.movePoints > 0;
      cDifficult:
        result := character.movePoints > 1;
    end
  else
    result := false;
end;

function IsCorrectDest(src, dest: TCellData): boolean; overload;
begin
  if (GetDistance(src.cubePos, dest.cubePos) = 1) then
    case dest.cType of
      cBlocked:
        result := false;
      cDefault:
        result := src.character.movePoints > 0;
      cDifficult:
        result := src.character.movePoints > 1;
    end
  else
    result := false;
end;

function GetCharList(): TStringDynArray;
begin
  result := charTypes;
end;

end.
