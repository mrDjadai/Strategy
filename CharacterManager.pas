unit CharacterManager;

interface

uses DataTypes, FMX.Objects, System.Classes, FMX.Controls, FMX.Ani;

procedure SelectCharacter(pos : Vector2);

procedure UnselectCharacter();

procedure TryMoveCharacter(source, dest : TCellData);

function TryCreateCharacter(cell : TCellData) : boolean;

function IsCorrectDest(character : TCharacter; dest : TCellData) : boolean; overload;

function IsCorrectDest(src, dest : TCellData) : boolean; overload;

procedure Init();

implementation

uses PlayerManager, CellManager, System.SysUtils, Window, CharacterDataVisualisator, System.IOUtils, System.Types, SkillManager, DiceManager;


const
  movingDuration = 0.5;


var
  charTypes : Array of string;

type
  skillData = array[0..5] of string;

function LoadSkill(data : skillData) : TSkill;
var line : string;
    id : integer;
begin
  case StrToInt(data[0]) of
    0 :begin
      var splash : SplashAttack;
      splash := SplashAttack.Create;
      splash.radius := StrToInt(data[3]);
      var damage : dicesCount;
      for var i := 0 to Length(damage)-1 do
        damage[i] := 0;
      for var i := 1 to Length(data[4]) do
        damage[i-1] := Ord(data[4][i]) - Ord('0');
      splash.damage := damage;
      splash.friendlyFire := data[5] = '+';
      result := splash;
      result.hasTarget := false;
    end;
    1 :begin
      var target : TargetAttack;
      target := TargetAttack.Create;
      target.radius := StrToInt(data[3]);
      var damage : dicesCount;
      for var i := 0 to Length(damage)-1 do
        damage[i] := 0;
      for var i := 1 to Length(data[4]) do
        damage[i-1] := Ord(data[4][i]) - Ord('0');
      target.damage := damage;
      result := target;
      result.hasTarget := true;
    end;
    2 :begin
      var heal : SplashHeal;
      heal := SplashHeal.Create;
      heal.radius := StrToInt(data[3]);
      var damage : dicesCount;
      for var i := 0 to Length(damage)-1 do
        damage[i] := 0;
      for var i := 1 to Length(data[4]) do
        damage[i-1] := Ord(data[4][i]) - Ord('0');
      heal.heals := damage;
      result := heal;
      result.hasTarget := false;
    end;
  end;

  result.name := data[1];
  result.reloadTime := StrToInt(data[2]);
end;

function LoadCharacter(FileName : string) : TCharacter;
var line : string;
  f : TextFile;
  data : skillData;
begin
  result := TCharacter.Create;

  AssignFile(f, FileName);
  Reset(f);
  readln(f, line);
  result.name := line;

  readln(f, line);
  result.sprite := line;

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
var Files: TStringDynArray;
begin
  SetLength(charTypes, 0);
  Files := TDirectory.GetFiles(ExtractFilePath(ParamStr(0)) + 'Resourses\Characters\');
  for var FileName in Files do
  begin
    if True then      //Добавить валидацию
    begin
      SetLength(charTypes, Length(charTypes) + 1);
      charTypes[Length(charTypes) - 1] := FileName;
    end;
  end;
end;

procedure SelectCharacter(pos : Vector2);
var last : vector2;
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
var last : vector2;
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

procedure MoveCharacter(source, dest : TCellData);
var
  AnimX, AnimY: TFloatAnimation;
  Image : TImage;
begin
  Image := source.character.img;
  selectedCharacter := dest.decardPos;


  dest.character := source.character;
  dest.character.pos := dest.decardPos;

  source.OnExit();
  dest.OnEnter();
  source.character := nil;

                                                   //Анимация

  AnimX := TFloatAnimation.Create(Image);
  AnimX.Parent := Image;
  AnimX.PropertyName := 'Position.X';
  AnimX.StartValue := source.Image.Position.X;
  AnimX.StopValue := dest.Image.Position.X;
  AnimX.Duration := movingDuration;
  AnimX.Start;

  AnimY := TFloatAnimation.Create(Image);
  AnimY.Parent := Image;
  AnimY.PropertyName := 'Position.Y';
  AnimY.StartValue := source.Image.Position.Y;
  AnimY.StopValue := dest.Image.Position.Y;
  AnimY.Duration := movingDuration;
  AnimY.Start;

  AnimX.OnFinish := Form2.DeleteAnimation;
  AnimY.OnFinish := Form2.DeleteAnimation;
end;

procedure TryMoveCharacter(source, dest : TCellData);
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


const characterOffset : vector2 = (x : -14; y : 0);
procedure CreateCharacter(cell : TCellData; charID : integer);
begin
var c : TCharacter;
    c := LoadCharacter(charTypes[charId]);
    c.owner := curPlayer;

    c.pos := cell.decardPos;
    cell.character := c;

    var myImage : TImage;
    myImage := TImage.Create(TComponent(cell.Image).Owner);
    myImage.Parent := TControl(cell.Image).Parent;

    MyImage.Position.x := cell.Image.Position.x + characterOffset.x;
    MyImage.Position.y := cell.Image.Position.y + characterOffset.y;
    MyImage.Height := cell.Image.Height;
    MyImage.Width := cell.Image.Width;
    MyImage.BringToFront();

    MyImage.HitTest := false;
    c.img := myImage;

    c.Init(form2);
    cell.ReDraw();


    c.hp := c.maxHp;

    currentPlayer.AddCharacter(c);
end;

function TryCreateCharacter(cell : TCellData) : boolean;
begin
  result := false;

  if cell.cType <> cBlocked then
  begin
    CreateCharacter(cell, 0);
    result := true;
  end;
end;

function IsCorrectDest(character : TCharacter; dest : TCellData) : boolean; overload;
begin
  if (GetDistance(decardToCube(character.pos), dest.cubePos) = 1) and (dest.character = nil) then
    case dest.cType of
    cBlocked: result := false;
    cDefault: result := character.movePoints > 0;
    cDifficult: result := character.movePoints > 1;
    end
  else
    result := false;
end;

function IsCorrectDest(src, dest : TCellData) : boolean; overload;
begin
  if (GetDistance(src.cubePos, dest.cubePos) = 1)  then
    case dest.cType of
    cBlocked: result := false;
    cDefault: result := src.character.movePoints > 0;
    cDifficult: result := src.character.movePoints > 1;
    end
  else
    result := false;
end;

end.
