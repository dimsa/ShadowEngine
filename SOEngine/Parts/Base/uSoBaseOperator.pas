// it's unit with the base class for all parts on SoEngine, like Renderer,
// Animator, Formattor and ETC.
unit uSoBaseOperator;

interface

uses
  System.SysUtils, uSoTypes, uSoBasePart,
  uEngine2DClasses, uSoObject, uSoContainerTypes, uCommonClasses, uSoProperty;

type
  TSoOperator<T> = class abstract
  protected
    FList: TEngine2DNamedList<T>;
    FElementBySubject: TDict<TSoObject, TList<TSoBasePart>>;
    FAddedObjects: Integer;
    FCritical: TCriticalSection;
    FOnAdd: TEvent<TOnAddContainerEventArgs>;
    procedure OnItemDestroy(ASender: TObject); virtual;
    procedure AddAsProperty(const AItem: TSoBasePart; const AName: string);
    function PropertyName: string; virtual; abstract;
    procedure Add(const AItem: T; const AName: string = ''); virtual;
  public
    property OnAdd: TEvent<TOnAddContainerEventArgs> read FOnAdd write FOnAdd;
    function Contains(const AName: string): Boolean; overload;
    function Contains(const AItem: T): Boolean; overload;
    function NameOf(const AItem: T): string;
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

function TSoOperator<T>.Contains(const AName: string): Boolean;
begin
  Result := FList.IsHere(AName);
end;

procedure TSoOperator<T>.AddAsProperty(const AItem: TSoBasePart; const AName: string);
var
  vProp: TSoProperty;
  vSubject: TSoObject;
begin
  vSubject := AItem.Subject;
  if not FElementBySubject.ContainsKey(vSubject) then
    FElementBySubject.Add(vSubject, TList<TSoBasePart>.Create);

  FElementBySubject[vSubject].Add(AItem);

  if not vSubject.HasProperty(PropertyName) then
  begin
    vProp := vSubject.AddProperty(PropertyName);
    vProp.Obj := AItem;
  end;

  vProp := vSubject.AddProperty(PropertyName + IntToStr(FElementBySubject[vSubject].Count));
  vProp.Obj := AItem;
end;

function TSoOperator<T>.Contains(const AItem: T): Boolean;
begin
  Result := FList.IsHere(AItem);
end;

constructor TSoOperator<T>.Create(const ACritical: TCriticalSection);
begin
  FCritical := ACritical;
  FList := TEngine2DNamedList<T>.Create(ACritical);
  FElementBySubject := TDict<TSoObject, TList<TSoBasePart>>.Create;
  FAddedObjects := 0;
end;

destructor TSoOperator<T>.Destroy;
var
  i: Integer;
  vObj: TObject;
  vSubj: TSoObject;
begin
  FCritical.Enter;
  for i := 0 to FList.Count - 1 do
  begin
    vObj := @FList;
    vObj.Free;
  end;

  FList.Clear;
  FList.Free;

  for vSubj in FElementBySubject.Keys do
    FElementBySubject[vSubj].Free;
  FElementBySubject.Free;

  FCritical.Leave;
  inherited;
end;

function TSoOperator<T>.NameOf(const AItem: T): string;
begin
  Result := FList.NameIfHere(AItem);
end;

procedure TSoOperator<T>.OnItemDestroy(ASender: TObject);
var
  vPart: TSoBasePart;
begin
//  vObj := T(ASender);
  FCritical.Enter;
  FList.Delete(T((@ASender)^));


  vPart := TSoBasePart(ASender);
  FElementBySubject[vPart.Subject].Remove(vPart);
  if FElementBySubject[vPart.Subject].Count <= 0 then
    FElementBySubject.Remove(vPart.Subject);
  FCritical.Leave;
end;

end.
