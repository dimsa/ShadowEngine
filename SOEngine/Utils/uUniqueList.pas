unit uUniqueList;

interface

uses
  System.Generics.Collections, System.SysUtils;

type
  TUniqueList<T> = class
  private
    FList: TList<T>;
    FDict: TDictionary<T, Integer>;
    function GetItem(AIndex: Integer): T;
    procedure SetItem(AIndex: Integer; const AValue: T);
  public
    procedure Add(AItem: T);
    procedure Insert(AIndex: Integer; AItem: T);
    procedure Remove(AItem: T);
    procedure Delete(AIndex: Integer);
    procedure Swap(const AIndex1, AIndex2: Integer); overload;
    procedure Swap(const AItem1, AItem2: T); overload;

    procedure Clear;
    procedure Move(const ACurIndex, ANewIndex: Integer);

    property Items[AIndex: Integer]: T read GetItem write SetItem; default;
    function Contains(const AItem: T): Boolean;
    function IndexOf(const AItem: T): Integer;
    function Count: Integer;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TUniqueList<T> }

procedure TUniqueList<T>.Add(AItem: T);
begin
  if FDict.ContainsKey(AItem) then
    raise Exception.Create('Duplicates are not allowed');

  FDict.Add(AItem, FList.Add(AItem));
end;

procedure TUniqueList<T>.Clear;
begin
  FList.Clear;
  FDict.Clear;
end;

function TUniqueList<T>.Contains(const AItem: T): Boolean;
begin
  Result := FDict.ContainsKey(AItem);
end;

function TUniqueList<T>.Count: Integer;
begin
  Exit(FList.Count)
end;

constructor TUniqueList<T>.Create;
begin
  FList := TList<T>.Create;
  FDict := TDictionary<T, Integer>.Create;
end;

procedure TUniqueList<T>.Delete(AIndex: Integer);
begin
  Remove(FList[AIndex]);
end;

destructor TUniqueList<T>.Destroy;
begin
  FList.Free;
  FDict.Free;
  inherited;
end;

function TUniqueList<T>.GetItem(AIndex: Integer): T;
begin
  Result := FList[AIndex];
end;

function TUniqueList<T>.IndexOf(const AItem: T): Integer;
begin
  if FDict.ContainsKey(AItem) then
    Exit(FDict[AItem]);

  Result := -1;
end;

procedure TUniqueList<T>.Insert(AIndex: Integer; AItem: T);
begin
  if FDict.ContainsKey(AItem) then
    raise Exception.Create('Duplicates are not allowed');

  FList.Insert(AIndex, AItem);
  FDict.Add(AItem, AIndex);
end;

procedure TUniqueList<T>.Move(const ACurIndex, ANewIndex: Integer);
begin
  FDict[FList[ACurIndex]] := ANewIndex;
  FDict[FList[ANewIndex]] := ACurIndex;
  FList.Move(ACurIndex, ANewIndex);
end;

procedure TUniqueList<T>.Remove(AItem: T);
var
  vInd: Integer;
  i: Integer;
  vItem: T;
begin
  vInd := FDict[AItem];
  FDict.Remove(AItem);
  FList.Remove(AItem);

  for vItem in FDict.Keys do
    if FDict[vItem] > vInd then
      FDict[vItem] := FDict[vItem] - 1;

  {  for i := vInd to FList.Count - 1 do
      Dec(FDict[FList[i]]); }
end;

procedure TUniqueList<T>.SetItem(AIndex: Integer; const AValue: T);
begin
  if FDict.ContainsKey(AValue) then
    raise Exception.Create('Duplicates are not allowed');

  FList[AIndex] := AValue;
  FDict.Add(AValue, AIndex);
end;

procedure TUniqueList<T>.Swap(const AItem1, AItem2: T);
var
  vOld: Integer;
begin
  FList.Exchange(FDict[AItem1], FDict[AItem2]);
  vOld := FDict[AItem1];
  FDict[AItem1] := FDict[AItem2];
  FDict[AItem2] := vOld;
end;

procedure TUniqueList<T>.Swap(const AIndex1, AIndex2: Integer);
var
  vOld: Integer;
begin
  vOld := FDict[FList[AIndex1]];
  FDict[FList[AIndex1]] := FDict[FList[AIndex2]];
  FDict[FList[AIndex2]] := vOld;

  FList.Exchange(AIndex1, AIndex2);
end;

end.
