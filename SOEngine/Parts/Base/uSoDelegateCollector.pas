unit uSoDelegateCollector;

interface

uses
  uSoIDelegateCollector;

type
  TSoDelegateCollector = class(TInterfacedObject, IRegisterDelegateCollector)
  public
    procedure RegisterDelegateGet(const AClass: TClass; const ADelegate: TDelegateGet);
    procedure RegisterDelegateGetByName(const AClass: TClass; const ADelegate: TDelegateGetByName);
    procedure RegisterDelegateAdd(const AClass: TClass; const ADelegate: TDelegateAdd);
    procedure RegisterDelegateAddByTemplate(const AClass: TClass; const ADelegate: TDelegateAddByTemplate);
    procedure RegisterDelegateAddCustom(const AClass: TClass; const ADelegate: TDelegateAddCustom);

    constructor Create;
  end;

implementation

{ TSoDelegateCollector }

constructor TSoDelegateCollector.Create;
begin

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
