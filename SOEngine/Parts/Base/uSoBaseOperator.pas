// it's unit with the base class for all parts on SoEngine, like Renderer,
// Animator, Formattor and ETC.
unit uSoBaseOperator;

interface

uses
  System.SyncObjs, System.SysUtils,
  uEngine2DClasses, uSoObject, uSoContainerTypes;

type
  TSoOperator<T> = class abstract
  protected
    FList: TEngine2DNamedList<T>;
    FAddedObjects: Integer;
    FCritical: TCriticalSection;
    FOnAdd: TEvent<TOnAddContainerEventArgs>;
    procedure OnItemDestroy(ASender: T);
  public
    procedure Add(const AItem: T; const AName: string = ''); virtual;
    property OnAdd: TEvent<TOnAddContainerEventArgs> read FOnAdd write FOnAdd;
    function AddFromTemplate(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): T; virtual; abstract;
    constructor Create(const ACritical: TCriticalSection); virtual;
    destructor Destroy; override;
  end;

implementation

{ TSoOperator<T> }

procedure TSoOperator<T>.Add(const AItem: T; const AName: string);
begin
  FCritical.Leave;
  FList.Add(AName, AItem);
  FCritical.Leave;
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
  FCritical.Enter;
  for i := 0 to FList.Count - 1 do
  begin
    vObj := @FList;
    vObj.Free;
  end;

  FList.Clear;
  FList.Free;
  FCritical.Leave;
  inherited;
end;

procedure TSoOperator<T>.OnItemDestroy(ASender: T);
begin
  FCritical.Enter;
  FList.Delete(ASender);
  FCritical.Leave;
end;

end.
