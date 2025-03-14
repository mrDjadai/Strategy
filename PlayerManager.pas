unit PlayerManager;

interface

uses DataTypes;

var selectedCharacter : vector2;

implementation

initialization
begin
  selectedCharacter.x := -1;
  selectedCharacter.y := -1;
end;

end.
