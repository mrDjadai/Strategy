unit CharacterManager;

interface

uses DataTypes;

procedure SelectCharacter(pos : Vector2);

implementation

uses PlayerManager, CellManager;

procedure SelectCharacter(pos : Vector2);
begin
  if selectedCharacter.x <> -1 then
  begin
    GetCell(selectedCharacter).character.isSelected := false;
    GetCell(selectedCharacter).character.ReDraw();
  end;

  selectedCharacter := pos;
  GetCell(pos).character.isSelected := true;
  GetCell(pos).character.ReDraw();
end;

end.
