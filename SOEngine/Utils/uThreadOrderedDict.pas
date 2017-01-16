unit uThreadOrderedDict;

interface

uses
  uOrderedDict;

{$X+}
type
  TThreadOrderedDict<TKey, TValue> = class
  private
    FDict: TOrderedDict<TKey, TValue>;
    FLock: TObject;
    procedure Lock;
    procedure Unlock;
  public
    procedure Add(AKey: TKey; AValue: TValue);
    procedure Insert(AIndex: Integer; AKey: TKey; AValue: TValue);
    procedure Remove(AKey: TKey);
    procedure RemoveAllValues(AValue: TValue);
    procedure Delete(AIndex: Integer);
    procedure Clear;
    procedure Move(const ACurIndex, ANewIndex: Integer);
    procedure SetValueByIndex(const AIndex: Integer; const AValue: TValue);
    procedure SetValueByKey(const AKey: TKey; const AValue: TValue);

    // vvv Non-blocking methods vvv
    function ContainsKey(AKey: TKey): Boolean;
    function ContainsValue(AValue: TValue): Boolean;

    function TryGetValueByIndex(const AIndex: Integer; out AValue: TValue): Boolean;
    function TryGetValueByKey(const AKey: TKey; out AValue: TValue): Boolean;
    function IndexOf(const AKey: TKey): Integer;

    function Count: Integer;
    // ^^^ Non-blocking methods ^^^

    function LockPointer: TOrderedDict<TKey,TValue>;
    procedure UnlockPointer;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TThreadOrderedDict<TKey, TValue> }

procedure TThreadOrderedDict<TKey, TValue>.Add(AKey: TKey; AValue: TValue);
begin
  Lock;
  try
    FDict.Add(AKey, AValue);
  finally
    Unlock;
  end;
end;

procedure TThreadOrderedDict<TKey, TValue>.Clear;
begin
  Lock;
  try
    FDict.Clear;
  finally
    Unlock;
  end;
end;

function TThreadOrderedDict<TKey, TValue>.ContainsKey(AKey: TKey): Boolean;
begin
  try
    Exit(FDict.ContainsKey(AKey));
  except
    Exit(False);
  end;
end;

function TThreadOrderedDict<TKey, TValue>.ContainsValue(
  AValue: TValue): Boolean;
begin
  try
    Exit(FDict.ContainsValue(AValue));
  except
    Exit(False);
  end;
end;

function TThreadOrderedDict<TKey, TValue>.Count: Integer;
begin
  try
    Exit(FDict.Count);
  except
    Exit(0);
  end;
end;

constructor TThreadOrderedDict<TKey, TValue>.Create;
begin
  FLock := TObject.Create;
  FDict := TOrderedDict<TKey, TValue>.Create;
end;

procedure TThreadOrderedDict<TKey, TValue>.Delete(AIndex: Integer);
begin
  Lock;
  try
    if FDict.Count > AIndex then
      FDict.Delete(AIndex);
  finally
    Unlock;
  end;
end;

destructor TThreadOrderedDict<TKey, TValue>.Destroy;
begin
  Lock;
  try
    FDict.Free;
    inherited Destroy;
  finally
    Unlock;
    FLock.Free;
  end;
  inherited;
end;

function TThreadOrderedDict<TKey, TValue>.TryGetValueByIndex(
  const AIndex: Integer; out AValue: TValue): Boolean;
begin
  try
    AValue := FDict.Item[AIndex];
    Exit(True);
  except
    Exit(False);
  end;
end;

function TThreadOrderedDict<TKey, TValue>.TryGetValueByKey(const AKey: TKey; out AValue: TValue): Boolean;
begin
  try
    AValue := FDict.Item[AKey];
    Exit(True);
  except
    Exit(False);
  end;
end;

procedure TThreadOrderedDict<TKey, TValue>.SetValueByIndex(const AIndex: Integer; const AValue: TValue);
begin
  Lock;
  try
    FDict.Item[AIndex] := AValue;
  finally
    Unlock;
  end;
end;

procedure TThreadOrderedDict<TKey, TValue>.SetValueByKey(const AKey: TKey; const AValue: TValue);
begin
  Lock;
  try
    FDict.Item[AKey] := AValue;
  finally
    Unlock;
  end;
end;

function TThreadOrderedDict<TKey, TValue>.IndexOf(const AKey: TKey): Integer;
begin
  Exit(FDict.IndexOf(AKey));
end;

procedure TThreadOrderedDict<TKey, TValue>.Insert(AIndex: Integer; AKey: TKey; AValue: TValue);
begin
  Lock;
  try
    FDict.Insert(AIndex, AKey, AValue);
  finally
    Unlock;
  end;
end;

procedure TThreadOrderedDict<TKey, TValue>.Lock;
begin
  TMonitor.Enter(FLock);
end;

function TThreadOrderedDict<TKey, TValue>.LockPointer: TOrderedDict<TKey, TValue>;
begin
  Lock;
  Result := FDict;
end;

procedure TThreadOrderedDict<TKey, TValue>.Move(const ACurIndex,
  ANewIndex: Integer);
begin
  Lock;
  try
    FDict.Move(ACurIndex, ANewIndex);
  finally
    Unlock;
  end;
end;

procedure TThreadOrderedDict<TKey, TValue>.Remove(AKey: TKey);
begin
  Lock;
  try
    if (FDict.ContainsKey(AKey)) then
      FDict.Remove(AKey);
  finally
    Unlock;
  end;
end;

procedure TThreadOrderedDict<TKey, TValue>.RemoveAllValues(AValue: TValue);
begin
  Lock;
  try
    FDict.RemoveAllValues(AValue);
  finally
    Unlock;
  end;
end;

procedure TThreadOrderedDict<TKey, TValue>.Unlock;
begin
  TMonitor.Exit(FLock);
end;

procedure TThreadOrderedDict<TKey, TValue>.UnlockPointer;
begin
  Unlock;
end;

end.
