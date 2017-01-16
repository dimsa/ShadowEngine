unit uOrderedDict;

interface

uses
  System.Generics.Collections;

type
  TOrderedDict<TKey, TValue> = class
  private
    FOrderedList: TList<TValue>;
    FDict: TDictionary<TKey, TValue>;
    FLinks: TList<TKey>;
    function GetItemInt(AIndex: Integer): TValue;
    procedure SetItemInt(AIndex: Integer; const AValue: TValue);
    function GetItemKey(AKey: TKey): TValue;
    procedure SetItemKey(AKey: TKey; const AValue: TValue);
    function GetKeyByIndex(AIndex: Integer): TKey;
  public
    property Item[AIndex: Integer]: TValue read GetItemInt write SetItemInt; default;
    property Item[AKey: TKey]: TValue read GetItemKey write SetItemKey; default;
    property KeyByIndex[AIndex: Integer]: TKey read GetKeyByIndex;
    function Count: Integer;
    procedure Add(AKey: TKey; AValue: TValue);
    procedure Insert(AIndex: Integer; AKey: TKey; AValue: TValue);
    procedure Remove(AKey: TKey);
    procedure RemoveAllValues(AValue: TValue);
    procedure Delete(AIndex: Integer);
    procedure Clear;
    function ContainsKey(AKey: TKey): Boolean;
    function ContainsValue(AValue: TValue): Boolean;
    function IndexOf(AKey: TKey): Integer;
    procedure Move(const ACurIndex, ANewIndex: Integer);
    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TOrderedDict<TKey, TValue> }

procedure TOrderedDict<TKey, TValue>.Add(AKey: TKey; AValue: TValue);
begin
  FDict.Add(AKey, AValue);
  FOrderedList.Add(AValue);
  FLinks.Add(AKey);
end;

procedure TOrderedDict<TKey, TValue>.Clear;
begin
  FDict.Clear;
  FOrderedList.Clear;
  FLinks.Clear;
end;

function TOrderedDict<TKey, TValue>.ContainsKey(AKey: TKey): Boolean;
begin
  Result := FDict.ContainsKey(AKey);
end;

function TOrderedDict<TKey, TValue>.ContainsValue(AValue: TValue): Boolean;
begin
  Result := FDict.ContainsValue(AValue);
end;

function TOrderedDict<TKey, TValue>.Count: Integer;
begin
  Result := FOrderedList.Count;
end;

constructor TOrderedDict<TKey, TValue>.Create;
begin
  FDict := TDictionary<TKey, TValue>.Create;
  FOrderedList := TList<TValue>.Create;
  FLinks := TList<TKey>.Create;
end;

procedure TOrderedDict<TKey, TValue>.Delete(AIndex: Integer);
var
  vKey: TKey;
begin
  vKey := FLinks[AIndex];
  FOrderedList.Delete(AIndex);
  FLinks.Delete(AIndex);
  FDict.Remove(vKey);
end;

destructor TOrderedDict<TKey, TValue>.Destroy;
begin
  FDict.Free;
  FOrderedList.Free;
  FLinks.Free;
  inherited;
end;

function TOrderedDict<TKey, TValue>.GetItemInt(AIndex: Integer): TValue;
begin
  Result := FOrderedList[AIndex];
end;

function TOrderedDict<TKey, TValue>.GetItemKey(AKey: TKey): TValue;
begin
  Result := FDict[AKey];
end;

function TOrderedDict<TKey, TValue>.GetKeyByIndex(AIndex: Integer): TKey;
begin
  Result := FLinks[AIndex];
end;

function TOrderedDict<TKey, TValue>.IndexOf(AKey: TKey): Integer;
begin
  Result := FLinks.IndexOf(AKey);
end;

procedure TOrderedDict<TKey, TValue>.Insert(AIndex: Integer; AKey: TKey; AValue: TValue);
begin
  FOrderedList.Insert(AIndex, AValue);
  FLinks.Insert(AIndex, AKey);
  FDict.Add(AKey, AValue);
end;

procedure TOrderedDict<TKey, TValue>.Move(const ACurIndex, ANewIndex: Integer);
begin
  FOrderedList.Move(ACurIndex, ANewIndex);
  FLinks.Move(ACurIndex, ANewIndex);
end;

procedure TOrderedDict<TKey, TValue>.Remove(AKey: TKey);
var
  vValue: TValue;
begin
  vValue := FDict[AKey];
  FDict.Remove(AKey);
  FLinks.Delete(FOrderedList.Remove(vValue));
end;

procedure TOrderedDict<TKey, TValue>.RemoveAllValues(AValue: TValue);
var
  vKey: TKey;
begin
  while FOrderedList.Contains(AValue) do
  begin
    vKey := FLinks[FOrderedList.Remove(AValue)];
    FLinks.Remove(vKey);
    FDict.Remove(vKey);
  end;
end;

procedure TOrderedDict<TKey, TValue>.SetItemInt(AIndex: Integer; const AValue: TValue);
begin
  FOrderedList[AIndex] := AValue;
end;

procedure TOrderedDict<TKey, TValue>.SetItemKey(AKey: TKey; const AValue: TValue);
begin
  FDict[AKey] := AValue;
end;

end.
