unit uSoundKeeper;

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
    FSoundBySubject: TDict<TSoObject, TList<TSoSound>>;
    FMediaPlayer: TMediaPlayer;
    FInited: Boolean;
    procedure OnItemDestroy(ASender: TObject);
    procedure InitMediaPlayer(const AName: string);
    procedure AddAsProperty(const AItem: TSoSound; const AName: string);
  public
    procedure Add(const AItem: TSoSound; const AName: string = ''); override;
    procedure AddTemplate(const ATemplateName, AFileName: string);
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

  AddAsProperty(AItem, AName);

  {$I .\Template\uItemAdd.inc}
end;

procedure TSoSoundKeeper.AddAsProperty(const AItem: TSoSound;
  const AName: string);
var
  vProp: TSoProperty;
begin
  if not FSoundBySubject.ContainsKey(AItem.Subject) then
    FSoundBySubject.Add(AItem.Subject, TList<TSoSound>.Create);

  FSoundBySubject[AItem.Subject].Add(AItem);

  if not AItem.Subject.HasProperty(Sound) then
  begin
    vProp := AItem.Subject.AddProperty(Sound);
    vProp.Obj := AItem;
  end;

  vProp := AItem.Subject.AddProperty(Rendition + IntToStr(FSoundBySubject[AItem.Subject].Count));
  vProp.Obj := AItem;
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
  FSoundBySubject := TDict<TSoObject, TList<TSoSound>>.Create;
end;

destructor TSoSoundKeeper.Destroy;
var
  vObj: TSoObject;
begin
  FTemplates.Free;
  if Assigned(FMediaPlayer) then
    FMediaPlayer.Free;

  for vObj in FSoundBySubject.Keys do
    FSoundBySubject[vObj].Free;
  FSoundBySubject.Free;

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
  FSoundBySubject[TSoSound(ASender).Subject].Remove(TSoSound(ASender));
  if FSoundBySubject[TSoSound(ASender).Subject].Count <= 0 then
    FSoundBySubject.Remove(TSoSound(ASender).Subject);
end;

end.
