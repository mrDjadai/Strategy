unit CharacterManager;

interface

uses DataTypes, FMX.Objects, System.Classes, FMX.Controls, FMX.Ani,
  System.Types;

  type TCharacterAnimation = class(TFloatAnimation)
    public c : TCharacter;
    removeFromQueue : boolean;
  end;

procedure SelectCharacter(pos: Vector2);

procedure UnselectCharacter();

procedure AnimateMoving(c: TCharacter);

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
  CharacterDataVisualisator, System.IOUtils, SkillManager, DiceManager,
  FMX.Media;

const
  movingDuration = 0.5;

type
  TCharType = record
    name: string;
    attackPlayer, skill1Player, skill2Player: TMediaPlayer;
  end;

  TSkillAudioNames = array [0 .. 2] of string;

var
  charTypes: Array of TCharType;

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
        splash.radius := StrToInt(data[4]);
        var
          damage: dicesCount;
        damage := LoadDices(data[5]);
        splash.damage := damage;
        splash.friendlyFire := data[6] = '+';
        splash.isBlockable := data[7] = '+';
        result := splash;
        result.hasTarget := false;
      end;
    1:
      begin
        var
          target: TargetAttack;
        target := TargetAttack.Create;
        target.radius := StrToInt(data[4]);
        var
          damage: dicesCount;
        damage := LoadDices(data[5]);
        target.isBlockable := data[6] = '+';
        target.damage := damage;
        result := target;
        result.hasTarget := true;
      end;
    2:
      begin
        var
          heal: SplashHeal;
        heal := SplashHeal.Create;
        heal.radius := StrToInt(data[4]);
        var
        damage := LoadDices(data[5]);
        heal.isBlockable := data[6] = '+';
        heal.heals := damage;
        result := heal;
        result.hasTarget := false;
      end;
    3:
      begin
        var
          ex: HonorExecution;
        ex := HonorExecution.Create;
        ex.radius := StrToInt(data[4]);
        var
          damage: dicesCount;
        damage := LoadDices(data[5]);
        ex.bonus := damage;

        damage := LoadDices(data[6]);
        ex.damage := damage;

        result := ex;
        result.hasTarget := true;
      end;
    4:
      begin
        var
          tp: Teleport;
        tp := Teleport.Create;
        tp.radius := StrToInt(data[4]);
        result := tp;
        result.hasTarget := true;
      end;
    5:
      begin
        var
          sh: Shield;
        sh := Shield.Create;
        sh.deltaArmor := StrToInt(data[4]);
        sh.deltaSpeed := StrToInt(data[5]);
        sh.deltaDamage := LoadDices(data[6]);
        sh.armoredSprite := data[7];
        sh.isActive := false;

        result := sh;
        result.hasTarget := false;
      end;
    6:
      begin
        var
          bd: BuildingPlacer;
        bd := BuildingPlacer.Create;
        bd.radius := StrToInt(data[4]);
        bd.buildingId := StrToInt(data[5]);
        result := bd;
        result.hasTarget := true;
      end;
    7:
      begin
        var
          fb: FireBall;
        fb := FireBall.Create;
        fb.radius := StrToInt(data[4]);
        fb.neigbourDamage := StrToInt(data[5]);
        fb.damage := LoadDices(data[6]);
        result := fb;
        result.hasTarget := true;
      end;
    8:
      begin
        var
          c: Curse;
        c := Curse.Create;
        c.radius := StrToInt(data[4]);
        c.duration := StrToInt(data[5]);
        result := c;
        result.hasTarget := true;
      end;
  end;

  result.name := data[1];
  result.reloadTime := StrToInt(data[3]);
end;

function GetSkillAudioNames(FileName: string): TSkillAudioNames;
var
  f: TextFile;
  line: string;
begin
  AssignFile(f, FileName);
  Reset(f);
  for var c := 0 to 17 do
    readln(f, line);
  for var i := 0 to 2 do
  begin
    readln(f, line);
    readln(f, line);
    readln(f, result[i]);
    for var k := 0 to 13 do
      readln(f, line);
  end;
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
  readln(f, line);
  result.name := line;

  readln(f, line);    //  hint
  readln(f, line);

  readln(f, line);
  readln(f, line);
  result.sprite := line;

  readln(f, line);
  readln(f, line);
  result.cost := StrToInt(line);

  readln(f, line);
  readln(f, line);
  result.maxHp := StrToInt(line);

  readln(f, line);
  readln(f, line);
  result.speed := StrToInt(line);

  readln(f, line);
  readln(f, line);
  result.armor := StrToInt(line);

  readln(f, line);
  for var i := 0 to High(skillData) do
  begin
    readln(f, data[i]);
    readln(f, data[i]);
  end;
  result.atack := LoadSkill(data);

  readln(f, line);
  for var i := 0 to High(skillData) do
  begin
    readln(f, data[i]);
    readln(f, data[i]);
  end;
  result.skill1 := LoadSkill(data);

  readln(f, line);
  for var i := 0 to High(skillData) do
  begin
    readln(f, data[i]);
    readln(f, data[i]);
  end;
  result.skill2 := LoadSkill(data);

  CloseFile(f);

  for var i := Low(charTypes) to High(charTypes) do
    if charTypes[i].name = FileName then
      result.id := i;

  result.atack.audioSource := charTypes[result.id].attackPlayer;
  result.skill1.audioSource := charTypes[result.id].skill1Player;
  result.skill2.audioSource := charTypes[result.id].skill2Player;
end;

function IsValidSkill(data: skillData): boolean;
var
  int, int1, int2, id, code, code1, code2: integer;
begin
  result := true;

  Val(data[0], id, code);

  if not FileExists(ExtractFilePath(ParamStr(0)) + 'Resourses\Audio\Skills\' +
    data[2] + '.wav') then
    code := 1;
  Val(data[3], int, code1);

  if (code = 0) and (code1 = 0) and (id >= 0) and (id <= 8) then
  begin
    case id of
      0:
        begin
          Val(data[5], int1, code);
          Val(data[4], int, code1);
          result := (code = 0) and (code1 = 0) and (int > 0) and (int1 >= 0) and
            ((data[6] = '+') or (data[6] = '-')) and
            ((data[7] = '+') or (data[7] = '-'));
        end;
      1, 2:
        begin
          Val(data[5], int1, code);
          Val(data[4], int, code1);
          result := (code = 0) and (code1 = 0) and (int > 0) and (int1 >= 0) and
            ((data[6] = '+') or (data[6] = '-'));
        end;
      3:
        begin
          Val(data[5], int, code);
          Val(data[4], int1, code1);
          Val(data[6], int2, code2);
          result := (code = 0) and (code1 = 0) and (code2 = 0) and (int > 0) and
            (int1 > 0) and (int2 > 0);
        end;
      4:
        begin
          Val(data[4], int, code);
          result := (code = 0) and (int > 0);
        end;
      5:
        begin
          Val(data[4], int, code);
          Val(data[5], int, code1);
          Val(data[6], int1, code2);
          result := (code = 0) and (code1 = 0) and (code2 = 0) and (int1 > 0)
            and FileExists(ExtractFilePath(ParamStr(0)) + 'Resourses\Sprites\' +
            data[7] + '0.png') and
            FileExists(ExtractFilePath(ParamStr(0)) + 'Resourses\Sprites\' +
            data[7] + '1.png');
        end;
      6:
        begin
          Val(data[5], int, code);
          Val(data[4], int1, code1);
          result := (code = 0) and (code1 = 0) and (int > 0) and (int1 >= 0);
        end;
      7:
        begin
          Val(data[6], int, code);
          Val(data[5], int1, code1);
          Val(data[4], int2, code2);
          result := (code = 0) and (code1 = 0) and (int > 0) and (int1 > 0) and
            (int2 > 0);
        end;
      8:
        begin
          Val(data[5], int1, code1);
          Val(data[4], int2, code2);
          result := (code = 0) and (code1 = 0) and (int1 > 0) and (int2 > 0);
        end;
    end;
  end
  else
    result := false;
end;

function IsValidChar(FileName: string): boolean;
var
  line: string;
  f: TextFile;
  data: skillData;
  code, int: integer;
begin
  result := true;

  AssignFile(f, FileName);
  Reset(f);

  readln(f, line);
  readln(f, line);

  readln(f, line);
  readln(f, line);

  readln(f, line);
  readln(f, line);

  if not FileExists(ExtractFilePath(ParamStr(0)) + 'Resourses\Sprites\' + line +
    '0.png') then
    result := false;
  if not FileExists(ExtractFilePath(ParamStr(0)) + 'Resourses\Sprites\' + line +
    '1.png') then
    result := false;

  readln(f, line);
  readln(f, line);

  Val(line, int, code);
  if code > 0 then
    result := false;

  readln(f, line);
  readln(f, line);

  Val(line, int, code);
  if code > 0 then
    result := false;

  readln(f, line);
  readln(f, line);

  Val(line, int, code);
  if code > 0 then
    result := false;

  readln(f, line);
  readln(f, line);

  Val(line, int, code);
  if code > 0 then
    result := false;

  readln(f, line);
  for var i := 0 to High(skillData) do
  begin
    readln(f, data[i]);
    readln(f, data[i]);
  end;

  if not IsValidSkill(data) then
    result := false;

  readln(f, line);
  for var i := 0 to High(skillData) do
  begin
    readln(f, data[i]);
    readln(f, data[i]);
  end;

  if not IsValidSkill(data) then
    result := false;

  readln(f, line);
  for var i := 0 to High(skillData) do
  begin
    readln(f, data[i]);
    readln(f, data[i]);
  end;

  if not IsValidSkill(data) then
    result := false;

  CloseFile(f);
end;

const maxCharCount = 16;

procedure Init();
var
  Files: TStringDynArray;
  names: TSkillAudioNames;
  count : integer;
begin
  count := 0;
  SetLength(charTypes, 0);
  Files := TDirectory.GetFiles(ExtractFilePath(ParamStr(0)) +
    'Resourses\Characters\');
  for var FileName in Files do
  begin
    if (count < maxCharCount) and IsValidChar(FileName) then
    begin
      Inc(count);
      SetLength(charTypes, Length(charTypes) + 1);
      charTypes[Length(charTypes) - 1].name := FileName;
      names := GetSkillAudioNames(FileName);

      charTypes[Length(charTypes) - 1].attackPlayer :=
        TMediaPlayer.Create(form2);
      charTypes[Length(charTypes) - 1].attackPlayer.FileName :=
        ExtractFilePath(ParamStr(0)) + 'Resourses\Audio\Skills\' +
        names[0] + '.wav';

      charTypes[Length(charTypes) - 1].skill1Player :=
        TMediaPlayer.Create(form2);
      charTypes[Length(charTypes) - 1].skill1Player.FileName :=
        ExtractFilePath(ParamStr(0)) + 'Resourses\Audio\Skills\' +
        names[1] + '.wav';

      charTypes[Length(charTypes) - 1].skill2Player :=
        TMediaPlayer.Create(form2);
      charTypes[Length(charTypes) - 1].skill2Player.FileName :=
        ExtractFilePath(ParamStr(0)) + 'Resourses\Audio\Skills\' +
        names[2] + '.wav';

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

procedure AnimateMoving(c: TCharacter);
var
  Item: TAnimationQueueItem;
  AnimX, AnimY: TCharacterAnimation;
  Image: TImage;
begin
  if c.AnimationQueue.Count <> 0 then
  begin
    c.IsAnimating := true;
    Item := c.AnimationQueue.Peek;

    Image := c.img;

    AnimX := TCharacterAnimation.Create(Image);
    AnimX.Parent := Image;
    AnimX.PropertyName := 'Position.X';
    AnimX.StartValue := Item.source.Image.Position.x;
    AnimX.StopValue := Item.dest.Image.Position.x;
    AnimX.duration := movingDuration;

    AnimY := TCharacterAnimation.Create(Image);
    AnimY.Parent := Image;
    AnimY.PropertyName := 'Position.Y';
    AnimY.StartValue := Item.source.Image.Position.y;
    AnimY.StopValue := Item.dest.Image.Position.y;
    AnimY.duration := movingDuration;

    AnimX.c := c;
    AnimY.c := c;

    AnimX.removeFromQueue := true;
    AnimY.removeFromQueue := false;

    AnimX.OnFinish := Form2.DeleteAnimation;
    AnimY.OnFinish := Form2.DeleteAnimation;

    AnimX.Start;
    AnimY.Start;
  end;
end;

procedure MoveCharacter(source, dest: TCellData);
var
  Item: TAnimationQueueItem;
begin
  // ������
  selectedCharacter := dest.decardPos;

  dest.character := source.character;
  dest.character.pos := dest.decardPos;

  source.OnExit();
  dest.OnEnter();
  source.character := nil;
  CharacterDataVisualisator.ReDraw();
  // ���������

  Item.source := source;
  Item.dest := dest;
  dest.character.AnimationQueue.Enqueue(Item);
  if not dest.character.IsAnimating then
    AnimateMoving(dest.character);
end;

procedure TryMoveCharacter(source, dest: TCellData);
begin
  if IsCorrectDest(source, dest) then
  begin
    MoveCharacter(source, dest);
    if dest.character <> nil then
      if dest.cType = cDifficult then
        dest.character.movePoints := dest.character.movePoints - 2
      else
        dest.character.movePoints := dest.character.movePoints - 1;

    form2.MovePlayer.CurrentTime := 0;
    form2.MovePlayer.Play();
  end
  else
  begin
    form2.IncorrectPlayer.CurrentTime := 0;
    form2.IncorrectPlayer.Play();
  end;
  ReDraw();
end;

const
  characterOffset: Vector2 = (x: - 14; y: 0);

procedure CreateCharacter(cell: TCellData; charID: integer);
begin
  var
    c: TCharacter;
  c := LoadCharacter(charTypes[charID].name);
  c.owner := curPlayer;

  c.pos := cell.decardPos;
  cell.character := c;

  var
    myImage: TImage;
  myImage := TImage.Create(form2);
  myImage.Parent := form2.CharactersOrigin;

  myImage.Position.x := cell.Image.Position.x + characterOffset.x;
  myImage.Position.y := cell.Image.Position.y + characterOffset.y;
  myImage.Height := cell.Image.Height;
  myImage.Width := cell.Image.Width;
  myImage.BringToFront();

  myImage.HitTest := false;
  c.img := myImage;

  c.Init(form2, c.maxHp);
  cell.ReDraw();

  c.hp := c.maxHp;

  currentPlayer.AddCharacter(c);
  c.ReDraw();
end;

function TryCreateCharacter(cell: TCellData): boolean;
begin
  result := false;

  if (cell.cType <> cBlocked) and (cell.character = nil) then
  begin
    CreateCharacter(cell, placableCharacterId);
    placableCharacterId := -1;
    result := true;

    if prepareMode then
    begin
      TryEndPrepare();
      form2.CharacterPlacePlayer.CurrentTime := 0;
      form2.CharacterPlacePlayer.Play();
    end;
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
      cDefault, cDanger:
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
      cDefault, cDanger:
        result := src.character.movePoints > 0;
      cDifficult:
        result := src.character.movePoints > 1;
    end
  else
    result := false;
end;

function GetCharList(): TStringDynArray;
begin
  SetLength(result, Length(charTypes));

  for var i := Low(charTypes) to High(charTypes) do
    result[i] := charTypes[i].name;
end;

end.
