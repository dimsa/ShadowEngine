// it's unit with the base class for all parts on SoEngine, like Renderer,
// Animator, Formattor and ETC.
unit uSoBaseOperator;

interface

uses
  System.SyncObjs, System.SysUtils,
  uEngine2DClasses;

type
  TSoOperator<T> = class
  protected
    FList: TEngine2DNamedList<T>;
    FCritical: TCriticalSection;
    procedure OnItemDestroy(ASender: T);
  public
    procedure Add(const AItem: T; const AName: string = ''); virtual;
    constructor Create(const ACritical: TCriticalSection);
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
end;

destructor TSoOperator<T>.Destroy;
var
  i: Integer;
begin

  FList.Clear;
  FList.Free;

  inherited;
end;

procedure TSoOperator<T>.OnItemDestroy(ASender: T);
begin
  FList.Delete(ASender);
end;

end.
