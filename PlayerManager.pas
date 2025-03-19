unit PlayerManager;

interface

uses DataTypes;

var selectedCharacter : vector2;
currentPlayer : TPlayer;
curPlayer : integer;
players : Array[0..1] of TPlayer;

targetSelectionMode : boolean;
selectedSkill : TSkill;
selectedCaster : TCellData;

procedure Init();

procedure NextMove();

function GetActionCount() : integer;

procedure SetActionCount(c : integer);

implementation

uses Window, System.SysUtils, Drawer, System.UITypes, CharacterManager;

const actionsPerRound = 3;

var
  round : integer;
  actions : integer;

procedure ShowMoveData();
begin
  Form2.RoundText.Text := IntToStr(round);

  if curPlayer = 0 then
    DrawColoredImage(Form2.PlayerIndicator, 'd6.png', TAlphaColors.blue)
  else
    DrawColoredImage(Form2.PlayerIndicator, 'd6.png', TAlphaColors.red);
end;

procedure Init();
begin
  targetSelectionMode := false;

  selectedCharacter.x := -1;
  selectedCharacter.y := -1;

  players[0] := TPlayer.Create;
  players[1] := TPlayer.Create;

  curPlayer := 0;
  round := 1;
  currentPlayer := players[0];

  SetActionCount(actionsPerRound);
  ShowMoveData();
  players[0].Init();
  players[1].Init();
end;

procedure StartNewRound();
begin
  Inc(round);
  players[0].OnRoundStart();
  players[1].OnRoundStart();
end;

procedure NextMove();
begin
  curPlayer := 1 - curPlayer;
  if curPlayer = 0 then
    StartNewRound();

  currentPlayer := players[curPlayer];
  UnselectCharacter();
  SetActionCount(actionsPerRound);
  ShowMoveData();
end;

function GetActionCount() : integer;
begin
  result := actions;
end;

procedure SetActionCount(c : integer);
begin
  actions := c;

  Form2.ActionsText.Text := IntToStr(actions) + '/' + IntToStr(actionsPerRound);
end;

end.
