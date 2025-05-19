unit SettingsManager;

interface

procedure Init();

procedure ChangeSound(val: single);
procedure ChangeMusic(val: single);

implementation

uses Window, System.SysUtils, FMX.Media;

type
  settings = record
    sound: single;
    music: single;
  end;

var
  currentSettings: settings;

procedure Init();
var
  f: File of settings;
begin
  AssignFile(f, ExtractFilePath(ParamStr(0)) + 'settings.dll');
  if FileExists(ExtractFilePath(ParamStr(0)) + 'settings.dll') then
  begin
    Reset(f);
    Read(f, currentSettings);
    form2.MusicBar.Value := currentSettings.music;
    form2.SoundBar.Value := currentSettings.sound;
  end
  else
  begin
    Rewrite(f);
    form2.MusicBar.Value := 1;
    form2.SoundBar.Value := 1;

    Write(f, currentSettings);
  end;
  CloseFile(f);

  ChangeMusic(currentSettings.music);
  ChangeSound(currentSettings.sound);
end;

procedure SaveSettings();
var
  f: File of settings;
begin
  AssignFile(f, ExtractFilePath(ParamStr(0)) + 'settings.dll');
  Rewrite(f);
  Write(f, currentSettings);
  CloseFile(f);
end;

procedure ChangeSound(val: single);
begin
  currentSettings.sound := val;
  for var i := 0 to form2.componentCount - 1 do
    if form2.components[i] is TMediaPlayer then
      TMediaPlayer(form2.components[i]).Volume := currentSettings.sound;

  ChangeMusic(currentSettings.music);
end;

procedure ChangeMusic(val: single);
begin
  form2.MusicPlayer.Volume := currentSettings.music;
  currentSettings.music := val;
  SaveSettings();
end;

end.
