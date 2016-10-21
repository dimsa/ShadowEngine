unit uSoundKeeper;

interface

uses
  System.SysUtils, FMX.Media,
  uSoTypes, uSoContainerTypes, uSoBasePart, uSoBaseOperator, uSoSound, uSoObject,
  uSoObjectDefaultProperties;

type


  TSoSoundKeeper = class(TSoOperator<TSoSound>)
  private type
    TSoSoundFriend = class(TSoSound);
  private
    FTemplates: TList<string>;
    FMediaPlayer: TMediaPlayer;
    FInited: Boolean;
    procedure OnItemDestroy(ASender: TObject);
    procedure InitMediaPlayer(const AName: string);
  public
    procedure Add(const AItem: TSoSound; const AName: string = ''); override;
    function AddFromTemplate(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoSound; override;
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

  {  FRenditionsBySubject[AItem.Subject].Add(AItem);

  if not AItem.Subject.HasProperty(Rendition) then
  begin
    vProp := AItem.Subject.AddProperty(Rendition);
    vProp.Obj := AItem;
  end; }

  {$I .\Template\uItemAdd.inc}
end;

function TSoSoundKeeper.AddFromTemplate(const ASubject: TSoObject;
  const ATemplateName, AName: string): TSoSound;
begin

end;

constructor TSoSoundKeeper.Create(const ACritical: TCriticalSection);
begin
  inherited;

  FInited := False;
  FTemplates := TList<string>.Create;
end;

destructor TSoSoundKeeper.Destroy;
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

procedure TSoSoundKeeper.OnItemDestroy(ASender: TObject);
begin
  FList.Delete(TSoSound(ASender));
end;

end.
