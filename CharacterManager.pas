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
  CharacterDataVisualisator, System.IOUtils, SkillManager, DiceManager,
  FMX.Media;

const
  movingDuration = 0.5;

type
  TCharType = record
    name: string;
    attackPlayer, skill1Player, skill2Player: TMediaPlayer;
  end;

  skillData = array [0 .. 7] of string;
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
  for var c := 0 to 15 do
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

procedure Init();
var
  Files: TStringDynArray;
  names: TSkillAudioNames;
begin
  SetLength(charTypes, 0);
  Files := TDirectory.GetFiles(ExtractFilePath(ParamStr(0)) +
    'Resourses\Characters\');
  for var FileName in Files do
  begin
    if true then // Добавить валидацию
    begin
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

      charTypes[Length(charTypes) - 1].attackPlayer.Volume := 0;
      charTypes[Length(charTypes) - 1].attackPlayer.Play();

      charTypes[Length(charTypes) - 1].skill1Player.Volume := 0;
      charTypes[Length(charTypes) - 1].skill1Player.Play();

      charTypes[Length(charTypes) - 1].skill2Player.Volume := 0;
      charTypes[Length(charTypes) - 1].skill2Player.Play();

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

  { for var i in GetCellBetween(decardToCube(pos), currentPlayer.portalCell.cubePos) do
    begin
    i.IsSelected := true;
    i.ReDraw();
    end; }

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
    // UnselectMap();
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
  AnimX.duration := movingDuration;
  AnimX.Start;

  AnimY := TFloatAnimation.Create(Image);
  AnimY.Parent := Image;
  AnimY.PropertyName := 'Position.Y';
  AnimY.StartValue := source.Image.Position.y;
  AnimY.StopValue := dest.Image.Position.y;
  AnimY.duration := movingDuration;
  AnimY.Start;

  AnimX.OnFinish := form2.DeleteAnimation;
  AnimY.OnFinish := form2.DeleteAnimation;

  CharacterDataVisualisator.ReDraw();
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
