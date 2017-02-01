unit uSoDelegateCollector;

interface

uses
  uSoTypes,
  uSoIDelegateCollector;

type
  TSoDelegateCollector = class(TInterfacedObject, IRegisterDelegateCollector)
  private
    FGetDict: TDict<TClass, TDelegateGet>;
    FGetByNameDict: TDict<TClass, TDelegateGetByName>;
    FAddDict: TDict<TClass, TDelegateAdd>;
    FAddByTemplateDict: TDict<TClass, TDelegateAddByTemplate>;
    FAddCustomDict: TDict<TClass, TList<TDelegateAddCustom>>;
  public
    procedure RegisterDelegateGet(const AClass: TClass; const ADelegate: TDelegateGet);
    procedure RegisterDelegateGetByName(const AClass: TClass; const ADelegate: TDelegateGetByName);
    procedure RegisterDelegateAdd(const AClass: TClass; const ADelegate: TDelegateAdd);
    procedure RegisterDelegateAddByTemplate(const AClass: TClass; const ADelegate: TDelegateAddByTemplate);
    procedure RegisterDelegateAddCustom(const AClass: TClass; const ADelegate: TDelegateAddCustom);

    function GetDelegate(const AClass: TClass): TDelegateGet;
    function GetByNameDelegate(const AClass: TClass): TDelegateGetByName;
    function AddDelegate(const AClass: TClass): TDelegateAdd;
    function AddByTemplateDelegate(const AClass: TClass): TDelegateAddByTemplate;
    function AddCustomDelegate(const AClass: TClass): TList<TDelegateAddCustom>;

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TSoDelegateCollector }

function TSoDelegateCollector.AddDelegate(const AClass: TClass): TDelegateAdd;
begin
  Result := FAddDict[AClass];
end;

function TSoDelegateCollector.AddByTemplateDelegate(const AClass: TClass): TDelegateAddByTemplate;
begin
  Result := FAddByTemplateDict[AClass];
end;

function TSoDelegateCollector.AddCustomDelegate(const AClass: TClass): TList<TDelegateAddCustom>;
begin
  Result := FAddCustomDict[AClass];
end;

function TSoDelegateCollector.GetDelegate(const AClass: TClass): TDelegateGet;
begin
  Result := FGetDict[AClass];
end;

function TSoDelegateCollector.GetByNameDelegate(const AClass: TClass): TDelegateGetByName;
begin
  Result := FGetByNameDict[AClass];
end;

constructor TSoDelegateCollector.Create;
begin
  FGetDict := TDict<TClass, TDelegateGet>.Create;
  FGetByNameDict := TDict<TClass, TDelegateGetByName>.Create;
  FAddDict := TDict<TClass, TDelegateAdd>.Create;
  FAddByTemplateDict := TDict<TClass, TDelegateAddByTemplate>.Create;
  FAddCustomDict := TDict<TClass, TList<TDelegateAddCustom>>.Create;
end;

destructor TSoDelegateCollector.Destroy;
begin
  FGetDict.Free;
  FGetByNameDict.Free;
  FAddDict.Free;
  FAddByTemplateDict.Free;
  FAddCustomDict.Free;

  inherited;
end;

procedure TSoDelegateCollector.RegisterDelegateAdd(const AClass: TClass; const ADelegate: TDelegateAdd);
begin
  FAddDict.Add(AClass, ADelegate);
end;

procedure TSoDelegateCollector.RegisterDelegateAddByTemplate(const AClass: TClass; const ADelegate: TDelegateAddByTemplate);
begin
  FAddByTemplateDict.Add(AClass, ADelegate);
end;

procedure TSoDelegateCollector.RegisterDelegateAddCustom(const AClass: TClass; const ADelegate: TDelegateAddCustom);
begin
  if not FAddCustomDict.ContainsKey(AClass) then
    FAddCustomDict.Add(AClass, TList<TDelegateAddCustom>.Create);

  FAddCustomDict[AClass].Add(ADelegate);

end;

procedure TSoDelegateCollector.RegisterDelegateGet(const AClass: TClass; const ADelegate: TDelegateGet);
begin
  FGetDict.Add(AClass, ADelegate);
end;

procedure TSoDelegateCollector.RegisterDelegateGetByName(const AClass: TClass; const ADelegate: TDelegateGetByName);
begin
  FGetByNameDict.Add(AClass, ADelegate);
end;

end.
