unit uSoSoundKeeper;

interface

uses
  System.SysUtils, FMX.Media,
  uSoTypes, uSoContainerTypes, uSoBasePart, uSoBaseOperator, uSoSound, uSoObject,
  uSoObjectDefaultProperties, uSoProperty;

type


  TSoSoundKeeper = class(TSoOperator<TSoSound>)
  private type
    TSoSoundFriend = class(TSoSound);
  private
    FTemplates: TDict<string, string>;
    FMediaPlayer: TMediaPlayer;
    FInited: Boolean;
    function PropertyName: string; override;
    procedure InitMediaPlayer(const AName: string);
  public
    procedure Add(const AItem: TSoSound; const AName: string = ''); override;
    procedure AddTemplate(const ATemplateName, AFileName: string);
    function AddFromTemplate(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoSound; override;
    function AddByFileName(const ASubject: TSoObject; const AFileName: string; const AName: string = ''): TSoSound;
    constructor Create(const ACritical: TCriticalSection); override;
    destructor Destroy; override;
  end;

implementation

{ TSoSoundKeeper }

procedure TSoSoundKeeper.Add(const AItem: TSoSound; const AName: string);
var
  vName: string;
begin
  if FList.Count <= 0 then
    InitMediaPlayer(AItem.FileName);
  TSoSoundFriend(AItem).Load;

  AddAsProperty(AItem, AName);

  {$I .\Template\uItemAdd.inc}
end;

function TSoSoundKeeper.AddByFileName(const ASubject: TSoObject; const AFileName, AName: string): TSoSound;
begin
  Add(TSoSound.Create(ASubject, AFileName), AName);
end;

function TSoSoundKeeper.AddFromTemplate(const ASubject: TSoObject;
  const ATemplateName, AName: string): TSoSound;
begin
  Add(TSoSound.Create(ASubject, FTemplates[ATemplateName]), AName);
end;

procedure TSoSoundKeeper.AddTemplate(const ATemplateName, AFileName: string);
begin
  FTemplates.Add(ATemplateName, AFileName);
end;

constructor TSoSoundKeeper.Create(const ACritical: TCriticalSection);
begin
  inherited;

  FInited := False;
  FTemplates := TDict<string, string>.Create;
end;

destructor TSoSoundKeeper.Destroy;
var
  vObj: TSoObject;
begin
  FTemplates.Free;
  if Assigned(FMediaPlayer) then
    FMediaPlayer.Free;

  inherited;
end;

procedure TSoSoundKeeper.InitMediaPlayer(const AName: string);
begin
  // it's hack. Without it you can't load any TMedia, you will got Unsupported File format
  FMediaPlayer := TMediaPlayer.Create(nil);
  FMediaPlayer.FileName := AName;
end;

function TSoSoundKeeper.PropertyName: string;
begin
  Result := Sound;
end;

end.
