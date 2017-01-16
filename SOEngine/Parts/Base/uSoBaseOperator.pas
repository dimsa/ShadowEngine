// it's unit with the base class for all parts on SoEngine, like Renderer,
// Animator, Formattor and ETC.
unit uSoBaseOperator;

interface

uses
  System.SysUtils, uSoTypes, uSoBasePart,
  uEngine2DClasses, uSoObject, uCommonClasses, uSoProperty, uSoIOperator,
  uSoIDelegateCollector;

type
  TSoOperator<T: class> = class abstract(TInterfacedObject, ISoOperator)
  protected
    FList: TEngine2DNamedList<T>;
    FElementBySubject: TDict<TSoObject, TList<TSoBasePart>>;
    FAddedObjects: Integer;
    FCritical: TCriticalSection;
    procedure OnItemDestroy(ASender: TObject); virtual;
    procedure AddAsProperty(const AItem: TSoBasePart; const AName: string);
    function PropertyName: string; virtual; abstract;
    procedure Add(const AItem: T; const AName: string = ''); overload; virtual;
    function OperatorItemClass: TClass; virtual; abstract;
    function Contains(const AItem: TObject): Boolean; overload;
    function NameOf(const AItem: TObject): string; overload;
    function Get(const ASubject: TSoObject): TObject; overload; virtual;
    function Get(const ASubject: TSoObject; const AName: string): TObject; overload; virtual;
    function AddFromTemplate(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TObject; overload; virtual; abstract;
    procedure Add(const AItem: TObject; const AName: string = ''); overload;
  public
    function Contains(const AName: string): Boolean; overload;
    function Contains(const AItem: T): Boolean; overload;
    function NameOf(const AItem: T): string; overload;
    procedure VisitByDelegateCollector(const ADelegateCollector: IDelegateCollector); virtual;
   // function Get(const ASubject: TSoObject): T; overload;
    function OperatorClass: TClass;
//    function AddFromTemplate(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): T; overload; virtual; abstract;
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

function TSoOperator<T>.Contains(const AItem: TObject): Boolean;
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

function TSoOperator<T>.Get(const ASubject: TSoObject): (where T is class);
begin

end;

function TSoOperator<T>.NameOf(const AItem: TObject): string;
begin

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

function TSoOperator<T>.OperatorClass: TClass;
begin

end;

end.

