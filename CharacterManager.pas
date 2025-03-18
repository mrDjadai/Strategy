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
  charTypes : Array of TCharacter;

procedure LoadCharacter(FileName : string);
var line : string;
  cur : TCharacter;
  f : TextFile;
begin
  SetLength(charTypes, Length(charTypes) + 1);
  cur := TCharacter.Create;
  charTypes[Length(charTypes) - 1] := cur;

  AssignFile(f, FileName);
  Reset(f);
  readln(f, line);
  cur.name := line;

  readln(f, line);
  cur.sprite := line;

  readln(f, line);
  cur.maxHp := StrToInt(line);

  readln(f, line);
  cur.speed := StrToInt(line);

  readln(f, line);
  cur.armor := StrToInt(line);

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
      LoadCharacter(FileName);
    end;
  end;
end;

procedure SelectCharacter(pos : Vector2);
begin
  if selectedCharacter.x <> -1 then
  begin
    GetCell(selectedCharacter).character.ReDraw();
  end;

  selectedCharacter := pos;
  SetCaharcter(GetCell(pos).character);
  GetCell(pos).character.ReDraw();
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


const testDamage : DicesCount = (0,2,1,0,0); //тест
const characterOffset : vector2 = (x : -14; y : 0);
procedure CreateCharacter(cell : TCellData; charID : integer);
begin
var c : TCharacter;
    c := TCharacter.Create;
    c.sprite := charTypes[charId].sprite;       //Загрузка визуала перса
    c.name := charTypes[charId].name;
    c.owner := curPlayer;

    c.pos := cell.decardPos;
    cell.character := c;

    var myImage : TImage;                                       //Настройка картинки
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

    c.maxHp := charTypes[charId].maxHp;           //Загрузка параметров перса
    c.hp := c.maxHp;
    c.speed := charTypes[charId].speed;
    c.armor := charTypes[charId].armor;

    c.movePoints := 0;

    currentPlayer.AddCharacter(c);
                                 //Для теста
    var a : SplashAttack;
    a := SplashAttack.Create;
    a.friendlyFire := false;
    a.damage := testDamage;
    a.radius := 2;
    c.atack := a;
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
