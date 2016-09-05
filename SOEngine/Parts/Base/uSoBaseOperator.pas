// it's unit with the base class for all parts on SoEngine, like Renderer,
// Animator, Formattor and ETC.
unit uSoBaseOperator;

interface

uses
  System.SyncObjs, System.SysUtils,
  uEngine2DClasses, uSoContainer;

type
  TSoOperator<T> = class abstract
  protected
    FList: TEngine2DNamedList<T>;
    FAddedObjects: Integer;
    FCritical: TCriticalSection;
    procedure OnItemDestroy(ASender: T);
  public
    procedure Add(const AItem: T; const AName: string = ''); virtual;
    function AddFromTemplate(const ASubject: TSoContainer; const ATemplateName: string; const AName: string = ''): T; virtual; abstract;
    constructor Create(const ACritical: TCriticalSection); virtual;
    destructor Destroy; override;
  end;

implementation

{ TSoOperator<T> }

procedure TSoOperator<T>.Add(const AItem: T; const AName: string);
begin
  FList.Add(AName, AItem);
end;

constructor TSoOperator<T>.Create(const ACritical: TCriticalSection);
begin
  FCritical := ACritical;
  FList := TEngine2DNamedList<T>.Create(ACritical);
  FAddedObjects := 0;
end;

destructor TSoOperator<T>.Destroy;
var
  i: Integer;
  vObj: TObject;
begin
  for i := 0 to FList.Count - 1 do
  begin
    vObj := @FList;
    vObj.Free;
  end;

  FList.Clear;
  FList.Free;

  inherited;
end;

procedure TSoOperator<T>.OnItemDestroy(ASender: T);
begin
  FList.Delete(ASender);
end;

end.
