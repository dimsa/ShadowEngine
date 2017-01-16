unit uThreadUniqueList;

interface

uses
  uUniqueList, System.SyncObjs;

type
  TThreadUniqueList<T> = class
  private
    FList: TUniqueList<T>;
    FLock: TObject;
    procedure Lock;
    procedure Unlock;
  public
    procedure Add(AItem: T);
    procedure Insert(AIndex: Integer; AItem: T);
    procedure Remove(AItem: T);
    procedure Delete(AIndex: Integer);
    procedure Swap(const AIndex1, AIndex2: Integer); overload;
    procedure Swap(const AItem1, AItem2: T); overload;
    procedure Clear;
    procedure Move(const ACurIndex, ANewIndex: Integer);
    procedure SetValue(const AIndex: Integer; AValue: T);

    // vvv Non-blocking methods vvv
    function TryGetValue(const AIndex: Integer; out AValue: T): Boolean;
    function Contains(const AItem: T): Boolean;
    function IndexOf(const AItem: T): Integer;
    function Count: Integer;
    // ^^^ Non-blocking methods ^^^

    // It's is not threadsave if you use pointer after Unlocking
    function LockPointer: TUniqueList<T>;
    procedure UnlockPointer;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TThreadUniqueList<T> }

procedure TThreadUniqueList<T>.Add(AItem: T);
begin
  Lock;
  try
    FList.Add(AItem);
  finally
    Unlock;
  end;
end;

procedure TThreadUniqueList<T>.Clear;
begin
  Lock;
  try
    FList.Clear;
  finally
    Unlock;
  end;
end;

function TThreadUniqueList<T>.Contains(const AItem: T): Boolean;
begin
  Exit(FList.Contains(AItem));
end;

function TThreadUniqueList<T>.Count: Integer;
begin
  Exit(FList.Count);
end;

constructor TThreadUniqueList<T>.Create;
begin
  FList := TUniqueList<T>.Create;
end;

procedure TThreadUniqueList<T>.Delete(AIndex: Integer);
begin
  Lock;
  try
    FList.Delete(AIndex);
  finally
    Unlock;
  end;
end;

destructor TThreadUniqueList<T>.Destroy;
begin
  Lock;
  try
    FList.Free;
    inherited Destroy;
  finally
    Unlock;
    FLock.Free;
  end;
end;

function TThreadUniqueList<T>.IndexOf(const AItem: T): Integer;
begin
  Result := FList.IndexOf(AItem);
end;

procedure TThreadUniqueList<T>.Insert(AIndex: Integer; AItem: T);
begin
  Lock;
  try
    FList.Insert(AIndex, AItem);
  finally
    Unlock;
  end;
end;

procedure TThreadUniqueList<T>.Lock;
begin
  TMonitor.Enter(FLock);
end;

function TThreadUniqueList<T>.LockPointer: TUniqueList<T>;
begin
  Lock;
  Result := FList;
end;

procedure TThreadUniqueList<T>.Move(const ACurIndex, ANewIndex: Integer);
begin
  Lock;
  try
    FList.Move(ACurIndex, ANewIndex);
  finally
    Unlock;
  end;
end;

procedure TThreadUniqueList<T>.Remove(AItem: T);
begin
  Lock;
  try
    FList.Remove(AItem);
  finally
    Unlock;
  end;
end;

procedure TThreadUniqueList<T>.SetValue(const AIndex: Integer; AValue: T);
begin
  Lock;
  try
    FList[AIndex] := AValue;
  finally
    Unlock;
  end;
end;

procedure TThreadUniqueList<T>.Swap(const AItem1, AItem2: T);
begin
  Lock;
  try
    FList.Swap(AItem1, AItem2);
  finally
    Unlock;
  end;
end;

procedure TThreadUniqueList<T>.Swap(const AIndex1, AIndex2: Integer);
begin
  Lock;
  try
    FList.Swap(AIndex1, AIndex2);
  finally
    Unlock;
  end;
end;

function TThreadUniqueList<T>.TryGetValue(const AIndex: Integer; out AValue: T): Boolean;
begin
  try
    AValue := FList[AIndex];
    Exit(True);
  except
    Exit(False);
  end;
end;

procedure TThreadUniqueList<T>.Unlock;
begin
  TMonitor.Exit(FLock);
end;

procedure TThreadUniqueList<T>.UnlockPointer;
begin
  Unlock;
end;

end.
