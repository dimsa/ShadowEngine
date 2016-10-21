unit uSoundKeeper;

interface

uses
  System.SysUtils,
  uSoTypes, uSoContainerTypes, uSoBasePart, uSoBaseOperator, uSoSound, uSoObject,
  uSoObjectDefaultProperties;

type
  TSoSoundKeeper = class(TSoOperator<TSoSound>)
  private
    FTemplates: TList<string>;
    procedure OnItemDestroy(ASender: TObject);
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
  FTemplates := TList<string>.Create;
end;

destructor TSoSoundKeeper.Destroy;
begin
  FTemplates.Free;
  inherited;
end;

procedure TSoSoundKeeper.OnItemDestroy(ASender: TObject);
begin
  FList.Delete(TSoSound(ASender));
end;

end.
