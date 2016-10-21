unit uSoundKeeper;

interface

uses
  System.SysUtils, FMX.Media,
  uSoTypes, uSoContainerTypes, uSoBasePart, uSoBaseOperator, uSoSound, uSoObject,
  uSoObjectDefaultProperties;

type
  TSoSoundKeeper = class(TSoOperator<TSoSound>)
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

  FMediaPlayer := TMediaPlayer.Create(nil);
end;

destructor TSoSoundKeeper.Destroy;
begin
  FTemplates.Free;
  FMediaPlayer.Free;
  inherited;
end;

procedure TSoSoundKeeper.InitMediaPlayer(const AName: string);
begin

end;

procedure TSoSoundKeeper.OnItemDestroy(ASender: TObject);
begin
  FList.Delete(TSoSound(ASender));
end;

end.
