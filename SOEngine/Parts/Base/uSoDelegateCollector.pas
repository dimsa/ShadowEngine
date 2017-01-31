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

    constructor Create;
    destructor Destroy; override;
  end;

implementation

{ TSoDelegateCollector }

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

end;

procedure TSoDelegateCollector.RegisterDelegateAddByTemplate(const AClass: TClass; const ADelegate: TDelegateAddByTemplate);
begin

end;

procedure TSoDelegateCollector.RegisterDelegateAddCustom(const AClass: TClass; const ADelegate: TDelegateAddCustom);
begin

end;

procedure TSoDelegateCollector.RegisterDelegateGet(const AClass: TClass; const ADelegate: TDelegateGet);
begin

end;

procedure TSoDelegateCollector.RegisterDelegateGetByName(const AClass: TClass; const ADelegate: TDelegateGetByName);
begin

end;

end.
