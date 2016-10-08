unit uCommonClasses;

interface

uses
  System.Generics.Collections, System.Classes;

type
  TDelegate<T> = function: T of object;
  TParameteredDelegate<T1, T2> = function(ASender: T1): T2 of object;
  TEvent<T> = procedure(ASender: TObject; AEventArgs: T) of object;
  TNotifyEvent<T> = procedure(ASender: T) of object;
  TStaticNotifyEvent<T> = procedure(ASender: T);

  // In fact it is THandlerList
  TEventList<T> = class
  private
    FList: TList<TEvent<T>>;
  public
    procedure Add(AItem: TEvent<T>);
    procedure Remove(AItem: TEvent<T>);
    procedure RaiseEvent(ASender: TObject; AEventArgs: T);
    constructor Create;
    destructor Destroy; override;
  end;

  TNotifyEventList = class
  private
    FList: TList<TNotifyEvent>;
  public
    procedure Add(AItem: TNotifyEvent);
    procedure Remove(AItem: TNotifyEvent);
    procedure RaiseEvent(ASender: TObject);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TEventList<T> }

procedure TEventList<T>.Add(AItem: TEvent<T>);
begin
  FList.Add(AItem);
end;

constructor TEventList<T>.Create;
begin
  FList := TList<TEvent<T>>.Create;
end;

destructor TEventList<T>.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TEventList<T>.RaiseEvent(ASender: TObject; AEventArgs: T);
var
  i: Integer;
  vEventHandler: TEvent<T>;
begin
  for i := 0 to FList.Count - 1 do
  begin
    vEventHandler := FList[i];
    vEventHandler(ASender, AEventARgs);
  end;
end;

procedure TEventList<T>.Remove(AItem: TEvent<T>);
begin
  FList.Remove(AItem);
end;

{ TNotifyEventList }

procedure TNotifyEventList.Add(AItem: TNotifyEvent);
begin
  FList.Add(AItem)
end;

constructor TNotifyEventList.Create;
begin
  FList := TList<TNotifyEvent>.Create;
end;

destructor TNotifyEventList.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TNotifyEventList.RaiseEvent(ASender: TObject);
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
    FList[i](ASender);
end;

procedure TNotifyEventList.Remove(AItem: TNotifyEvent);
begin
  FList.Remove(AItem);
end;

end.
