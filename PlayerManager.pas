unit PlayerManager;

interface

uses DataTypes;

var
  selectedCharacter: vector2;
  currentPlayer: TPlayer;
  curPlayer: integer;
  players: Array [0 .. 1] of TPlayer;

  targetSelectionMode: boolean;
  prepareMode: boolean;
  selectedSkill: TSkill;
  selectedCaster: TCellData;

  placableCharacterId: integer;
  placableBuildingId: integer;

  curseDamageMultiplier: real;
  addedMoneyMultiplier: real;


procedure ClearPlayerData();

procedure Win(player: TPlayer);

procedure Lose(player: TPlayer);

procedure Init(baseMoney: integer; roundMoney: integer);

procedure NextMove();

function GetActionCount(): integer;

procedure SetActionCount(c: integer);

procedure TryEndPrepare();

implementation

uses Window, System.SysUtils, Drawer, System.UITypes, CharacterManager, CellManager;

const
  actionsPerRound = 3;

var

  round: integer;
  actions: integer;
  moneyPerRound: integer;

procedure ClearPlayerData();
var
  current, next: charList;
begin
  for var p in players do
  begin
    current := p.characters;

    while current <> nil do
    begin
      next := current^.next;
      Dispose(current);
      current := next;
    end;

    p.characters := nil;
  end;
end;

procedure ShowMoveData();
begin
  Form2.RoundText.Text := IntToStr(round);

  if curPlayer = 0 then
    DrawColoredImage(Form2.PlayerIndicator, 'playerIndicator.png', TAlphaColors.Darkblue)
  else
    DrawColoredImage(Form2.PlayerIndicator, 'playerIndicator.png', TAlphaColors.Darkred);
end;

procedure Init(baseMoney: integer; roundMoney: integer);
begin
  targetSelectionMode := false;

  moneyPerRound := roundMoney;

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

  players[0].Money := baseMoney;
  players[1].Money := baseMoney;

  placableCharacterId := -1;
  placableBuildingId := -1;
end;

procedure StartNewRound();
begin
  Inc(round);
  players[0].Money := players[0].Money + moneyPerRound;
  players[1].Money := players[1].Money + moneyPerRound;
  players[0].OnRoundStart();
  players[1].OnRoundStart();
end;

procedure NextMove();
begin
  curPlayer := 1 - curPlayer;

  if prepareMode = false then
  begin
    if curPlayer = 0 then
      StartNewRound();
  end
  else
    SelectMap(TPlayer.CorrectHalf, nil);

  currentPlayer := players[curPlayer];
  UnselectCharacter();
  SetActionCount(actionsPerRound);
  ShowMoveData();
  Form2.MoneyText.Text := IntToStr(currentPlayer.Money);
end;

function GetActionCount(): integer;
begin
  result := actions;
end;

procedure SetActionCount(c: integer);
begin
  actions := c;

  Form2.ActionsText.Text := IntToStr(actions) + '/' + IntToStr(actionsPerRound);

//  if actions = 0 then
//      NextMove();
end;

procedure TryEndPrepare();
begin
  if players[1 - curPlayer].CanPlace then
  begin
    NextMove();
    ShowPlacersCount(currentPlayer);
  end
  else if currentPlayer.CanPlace = false then
  begin
    NextMove();
    prepareMode := false;

    for var item in charPlacers do
      item.txt.Text := IntToStr(item.price);
    for var item1 in buildPlacers do
      if item1 <> nil then
      begin
        item1.Visible := false;
        item1.txt.Visible := false;
      end;

    Form2.SkipRound.Enabled := true;
    curPlayer := 0;
    currentPlayer := players[0];
    UnselectMap();
  end
  else
    ShowPlacersCount(currentPlayer);
  ShowMoveData();
end;

procedure Win(player: TPlayer);
begin
  if currentPlayer <> player then
    curPlayer := 1 - curPlayer;

  if curPlayer = 0 then
    DrawColoredImage(Form2.WinnerIndicator, 'playerIndicator.png', TAlphaColors.Darkblue)
  else
    DrawColoredImage(Form2.WinnerIndicator, 'playerIndicator.png', TAlphaColors.Darkred);

  Form2.WinnerText.Text := Form2.WinnerText.Text + IntToStr(1 + curPlayer);
  Form2.WinPanel.Visible := true;

  Form2.WinPlayer.CurrentTime := 0;
  Form2.WinPlayer.Play();
end;

procedure Lose(player: TPlayer);
begin
  if player = players[0] then
    Win(players[1])
  else
    Win(players[0]);
end;

end.
