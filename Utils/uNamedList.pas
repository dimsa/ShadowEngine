// TNamedList v.2 is the generic TList where some Items can to have names and
// can be called by the name. Author: Dmitry Sorokin

unit uNamedList;

interface

Uses
  System.SysUtils, System.Generics.Collections, System.Generics.Defaults,
  System.Types, uClasses;

type
  TNameAndValue = record
    Name: String;
    Index: Integer;
  end;

  TNamedList<T> = class(TEnumerable<T>)
  strict private
    FParent: Pointer; // Pointer to master
  //  FNames: TList<TNameAndValue>;
    FAdded: Integer; // Считает количество добавленных всего элементов
    FDict: TDictionary<String,T>;
    FList: TList<T>;
    FLink: TList<String>;//array of string;// TDictionary<Integer,String>;
    function GetItemS(Name: String): T;
    procedure SetItemS(AName: String; const Value: T);
  private
    function GetItemI(Index: Integer): T;
    procedure SetItemI(Index: Integer; const Value: T);
    function GetCount: Integer;
  protected
    function DoGetEnumerator: TEnumerator<T>; override;
  public
    property Parent: Pointer read FParent write FParent;

    property Items[Index: Integer]: T read GetItemI write SetItemI; default;
    property Items[Name: String]: T read GetItemS write SetItemS; default;
    property Count: Integer read GetCount;{ write SetCount;}

    function Add(const AName: String; const AValue: T): Integer; reintroduce; overload; virtual;
    function Add(AValue: T): Integer; reintroduce; overload; virtual;

    function Insert(const AIndex: Integer; const AName: String; const AValue: T): Integer; reintroduce; overload; virtual;
    function Insert(const AIndex: Integer; AValue: T): Integer; reintroduce; overload; virtual;

    function AddIfNo(const AName: String; Const AValue: T): Integer; overload; virtual;
    function AddIfNo(AValue: T): Integer; overload; virtual;

    procedure Delete(const AName: String); overload; virtual;
    procedure Delete(const AT: T); overload; virtual;
    procedure Delete(const AIndex: Integer); overload; virtual;
   // procedure Remove(AT: T);

//    function CountNames: Integer;
    function ByName(const AName: String): T;
    function NameOf(const AObject: T): String; overload; // Возвращает имя по объекту
    function NameOf(const AIndex: Integer): String; overload; // Возвращает имя по объекту
    function NameIfHere(const AObject: T): String; overload; // Возвращает имя по объекту
    function IsHere(const AName: String): Boolean; overload;
    function IsHere(const AObject: T): Boolean; overload;
    function IndexOf(const AName: String): Integer; overload;
    function IndexOfItem(const Value: T; Direction: TDirection): Integer; overload;
//    function IndexOfItem(const AName: String): Integer; overload;
//    function Count: Integer; reintroduce;

    procedure AddName(const AIndex: Integer; AName: String); overload;
    procedure AddName(const AValue: T; AName: String); overload;
{    procedure DeleteName(const AIndex: Integer); overload;
    procedure DeleteName(const AName: String); overload; }

    procedure AddList(const AList: TNamedList<T>);
    procedure AddListIfNo(const AList: TNamedList<T>);
    procedure Clear; reintroduce;

    constructor Create{(const AParent: Pointer)}; virtual;
    destructor Destroy; override;

    function GetEnumerator: TEnumerator<T>; reintroduce;

    const
      CClassName = 'TNamedList';
  end;


implementation

{ TEngine2DAnimations }

procedure TNamedList<T>.AddName(const AIndex: Integer; AName: String);
var
  vValue: TNameAndValue;
  vPair: TPair<string,T>;
begin
  vPair := FDict.ExtractPair(FLink[AIndex]);
  FDict.Add(AName, vPair.Value);
  FLink[AIndex] := AName;
end;

function TNamedList<T>.Add(const AName: String;
  const AValue: T): Integer;
var
  vExisting: Integer;
  vIndex: Integer;
  vT: T;
begin
  // Если это имя уже есть, то выходим с индексом существующего
 { vExisting := IndexOf(AName);
  if vExisting >= 0 then
    Exit(vExisting);   }

{  if IsHere(AName) then
    Exit;}
  Inc(FAdded);
  FDict.Add(AName, AValue);
  vIndex := FList.Add(AValue);
  FLink.Add(AName);

  Result := vIndex;
end;

function TNamedList<T>.Add(AValue: T): Integer;
var
  vIndex: Integer;
  vName: String;
  Guid: TGUID;
  i: Integer;
begin

  // Почему-то андройд по-другому не создает уникальный ГУИД
  Guid := Guid.Empty;
  Guid.D1 := Random64;
  Guid.D2 := Random(Word.MaxValue+1);
  Guid.D3 := Random(Word.MaxValue+1);

  for i := 0 to 7 do
    Guid.D4[i] := Random(Byte.MaxValue+1);

  vName := Guid.ToString;
  vName := StringReplace(vName, '-', '', [rfReplaceAll]);
  System.Delete(vName, 1, 1);
  System.Delete(vName, Length(vName), 1);
  vName:= 'GUID'+vName + IntToStr(FAdded);//+IntToStr(Random(65536));
  Result := Self.Add(vName, AValue);
end;

function TNamedList<T>.AddIfNo(const AName: String; const AValue: T): Integer;
begin
  if not FDict.ContainsKey(AName) then
    Self.Add(AName, AValue);
end;

function TNamedList<T>.AddIfNo(AValue: T): Integer;
begin
  if not FDict.ContainsValue(AValue) then
    Self.Add(AValue);
end;

procedure TNamedList<T>.AddList(const AList: TNamedList<T>);
var
  vV: T;
begin
  for vV in AList do
    Self.Add(AList.NameOf(vV),vV);
end;

procedure TNamedList<T>.AddListIfNo(const AList: TNamedList<T>);
var
  vV: T;
begin
  for vV in AList do
    Self.AddIfNo(AList.NameOf(vV),vV);
end;

procedure TNamedList<T>.AddName(const AValue: T; AName: String);
var
  vValue: TNameAndValue;
  vS: String;
  vIndex: Integer;
begin
  vIndex := FList.IndexOf(AValue);
  if vIndex < 0 then
  begin
    vS := 'Error in adding name "' + AName +
          '". Object not fount in the list in class "' + CClassName + '"';
    Raise Exception.Create(vS);
  end;
  Self.AddName(vIndex, AName);
end;

function TNamedList<T>.ByName(const AName: String): T;
var
  vN, i: Integer;
  vS: String;
begin
  if FDict.ContainsKey(AName) then
    Result := FDict[AName] else
  begin
    vS := 'Can not find name "' + AName +
        '". in class "' + CClassName + '"';
    Raise Exception.Create(vS);
  end;
end;

procedure TNamedList<T>.Clear;
begin
  FList.Clear;
  FDict.Clear;
  FLink.Clear;
end;

{function TNamedList<T>.CountNames: Integer;
begin
  Exit(FNames.Count);
end;  }

constructor TNamedList<T>.Create{(const AParent: Pointer)};
begin
  FList := TList<T>.Create;
  FDict := TDictionary<String,T>.Create;
  FLink := TList<String>.Create;//TDictionary<Integer,String>.Create;
  FAdded := 0;
end;

procedure TNamedList<T>.Delete(const AName: String);
var
  vN, vIndex: Integer;
  vObj: T;
begin
  vObj := FDict[AName];

  Delete(FList.IndexOf(vObj));
end;

procedure TNamedList<T>.Delete(const AIndex: Integer);
begin
  FList.Delete(AIndex);
  FDict.Remove(FLink[AIndex]);
  FLink.Delete(AIndex);
 // EnsureIntegerity(AIndex);
end;

procedure TNamedList<T>.Delete(const AT: T);
var
  vIndex: Integer;
begin
  vIndex := FList.IndexOf(AT);
  FList.Delete(vIndex);
  FDict.Remove(FLink[vIndex]);
  FLink.Delete(vIndex);
end;

destructor TNamedList<T>.Destroy;
begin
  FLink.Free;
  FDict.Free;
  FList.Free;
  inherited;
end;

function TNamedList<T>.DoGetEnumerator: TEnumerator<T>;
begin
  Result := GetEnumerator;
end;

{procedure TNamedList<T>.EnsureIntegerity(const AIndex: Integer);
var
  i, vN: Integer;
  vPair: TPair<Integer, T>;
begin
  vN := FList.Count - 1;
  for i := AIndex to vN - 1 do
  begin
    FLink[i] := FLink[i+1];
    //vPair := FLink.ExtractPair(i);
  end;

//vPair := FDict.ExtractPair(FLink[AIndex]);
 // FDict.Add(AName, vPair.Value);

end;     }

{function TNamedList<T>.GetItemInt(Index: Integer): T;
begin
  Result := (self as TList<T>).Items[Index];
end;}

function TNamedList<T>.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TNamedList<T>.GetEnumerator: TEnumerator<T>;
begin
  Result := FList.GetEnumerator;
end;

function TNamedList<T>.GetItemI(Index: Integer): T;
begin
  Result := FList[Index];
end;

function TNamedList<T>.GetItemS(Name: String): T;
begin
  Result := FDict[Name];
end;

function TNamedList<T>.IndexOf(const AName: String): Integer;
begin
  Result :=
    FList.IndexOf(
      FDict[AName]
    );
end;

function TNamedList<T>.IndexOfItem(const Value: T;
  Direction: TDirection): Integer;
begin
  Result := FList.IndexOfItem(Value, Direction);
end;

function TNamedList<T>.Insert(const AIndex: Integer; const AName: String;
  const AValue: T): Integer;
var
  vExisting: Integer;
  vIndex: Integer;
  vT: T;
  i: Integer;
begin
  Inc(FAdded);
  FDict.Add(AName, AValue);
  FList.Insert(AIndex, AValue);
  FLink.Insert(AIndex, AName);

  Result := AIndex;
end;

function TNamedList<T>.Insert(const AIndex: Integer; AValue: T): Integer;
var
  vIndex: Integer;
  vName: String;
  Guid: TGUID;
  i: Integer;
begin

  // Почему-то андройд по-другому не создает уникальный ГУИД
  Guid := Guid.Empty;
  Guid.D1 := Random64;
  Guid.D2 := Random(Word.MaxValue+1);
  Guid.D3 := Random(Word.MaxValue+1);

  for i := 0 to 7 do
    Guid.D4[i] := Random(Byte.MaxValue+1);

  vName := Guid.ToString;
  vName := StringReplace(vName, '-', '', [rfReplaceAll]);
  System.Delete(vName, 1, 1);
  System.Delete(vName, Length(vName), 1);
  vName:= 'GUID'+vName + IntToStr(FAdded);
  Result := Self.Insert(AIndex, vName, AValue);
end;

function TNamedList<T>.IsHere(const AObject: T): Boolean;
begin
  if FList.IndexOfItem(AObject, TDirection.FromBeginning) > -1 then
      Exit(True);

  Result := False;
end;

function TNamedList<T>.IsHere(const AName: String): Boolean;
begin
  Result := FDict.ContainsKey(AName);
end;

function TNamedList<T>.NameIfHere(const AObject: T): String;
var
//  vN, i: Integer;
  vS: String;
  vA: Integer;
begin
//  vN := FList.Count - 1;

  vA := FList.IndexOfItem(AObject, TDirection.FromBeginning);
  if vA > -1 then
      Exit(Self.NameOf(vA));
  Result := '';
end;

function TNamedList<T>.NameOf(const AIndex: Integer): String;
begin
  Result := FLink[AIndex];
end;

function TNamedList<T>.NameOf(const AObject: T): String;
var
  vN, i: Integer;
  vS: String;
  vA: Integer;
begin
  vN := FList.Count - 1;

  vA := FList.IndexOfItem(AObject, TDirection.FromBeginning);
  if vA > -1 then
      Exit(Self.NameOf(vA));

  // If value with name not found
  vS := 'Can not find value "' +
        '". in class "' + CClassName + '"';
  Raise Exception.Create(vS);
end;

{procedure TNamedList<T>.SetItemInt(Index: Integer; const Value: T);
begin
  (self as TList<T>).Items[Index] := Value;
end;}

{procedure TNamedList<T>.SetCount(const Value: Integer);
begin
  FList.Count := Value;
end; }

procedure TNamedList<T>.SetItemI(Index: Integer; const Value: T);
begin
  FList[Index] := Value;
  FDict[FLink[Index]] := Value;
end;

procedure TNamedList<T>.SetItemS(AName: String; const Value: T);
var
  vN, i: Integer;
  vObj: T;
begin
  vObj := FDict[AName];
  FList[FList.IndexOf(vObj)] := Value;
  FDict[AName] := Value;
end;

end.




