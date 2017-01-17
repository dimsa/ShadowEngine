// it's unit with the base class for all parts on SoEngine, like Renderer,
// Animator, Formattor and ETC.
unit uSoBaseOperator;

interface

uses
  System.SysUtils, uSoTypes, uSoBasePart,
  uEngine2DClasses, uSoObject, uCommonClasses, uSoProperty, uSoIOperator, uSoIDelegateCollector;

type
  TSoOperator<T: class> = class abstract(TInterfacedObject, ISoOperator)
  protected
    FList: TThreadUniqueList<TObject>;//OrderedDict<string, TObject>;//TEngine2DNamedList<T>;
    FElementBySubject: TDict<TSoObject, TList<TSoBasePart>>;
    FAddedObjects: Integer;
    FCritical: TCriticalSection;
    procedure OnItemDestroy(ASender: TObject); virtual;
    procedure AddAsProperty(const AItem: TSoBasePart; const AName: string);
    function PropertyName: string; virtual; abstract;
    procedure Add(const AItem: T); virtual;

    function ContainsObj(const AItem: TObject): Boolean;
    function NameOfObj(const AItem: TObject): string;

    function GetObj(const ASubject: TSoObject): TObject; overload;

    function AddObjFromTemplate(const ASubject: TSoObject; const ATemplateName: string): TObject; virtual; abstract;
    procedure AddObj(const AItem: TObject);//; const AName: string = '');

    procedure VisitByDelegateCollector(const ADelegateCollector: IDelegateCollector);
  public
    function OperatorItemClass: TClass; virtual; abstract;
    function Contains(const AItem: T): Boolean; overload;
    function NameOf(const AItem: T): string;

    function AddFromTemplate(const ASubject: TSoObject; const ATemplateName: string): T; virtual; abstract;
    constructor Create(const ACritical: TCriticalSection); virtual;
    destructor Destroy; override;
  end;

implementation

{ TSoOperator<T> }

procedure TSoOperator<T>.Add(const AItem: T);
begin
  AddObj(AItem);
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

procedure TSoOperator<T>.AddObj(const AItem: TObject);
begin
  FList.Add(AItem);
end;

function TSoOperator<T>.Contains(const AItem: T): Boolean;
begin
  Result := Contains(AItem); //FList.IsHere(AItem);
end;

function TSoOperator<T>.ContainsObj(const AItem: TObject): Boolean;
begin
  Result := FList.Contains(AItem);
end;

constructor TSoOperator<T>.Create(const ACritical: TCriticalSection);
begin
  FCritical := ACritical;
  FList := TThreadUniqueList<TObject>.Create;
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

function TSoOperator<T>.GetObj(const ASubject: TSoObject): TObject;
begin
//  REsult := FList
end;

function TSoOperator<T>.NameOf(const AItem: T): string;
begin
  Result := NameOfObj(AItem); // FList.NameIfHere(AItem);
end;

function TSoOperator<T>.NameOfObj(const AItem: TObject): string;
begin

end;

procedure TSoOperator<T>.OnItemDestroy(ASender: TObject);
var
  vPart: TSoBasePart;
  vItem: string;
begin
  FList.Remove(ASender);

  vPart := TSoBasePart(ASender);
  FElementBySubject[vPart.Subject].Remove(vPart);
  if FElementBySubject[vPart.Subject].Count <= 0 then
    FElementBySubject.Remove(vPart.Subject);
  FCritical.Leave;
end;

procedure TSoOperator<T>.VisitByDelegateCollector(
  const ADelegateCollector: IDelegateCollector);
begin

end;

end.

