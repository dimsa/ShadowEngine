unit uSoIDelegateCollector;

interface

uses
  uSoTypes, uSoObject;

type
  TDelegateGet = function(const ASubject: TSoObject; const AClass: TClass): TObject;
  TDelegateGetByName = function(const ASubject: TSoObject; const AClass: TClass; const AName: string = ''): TObject;

  TDelegateAdd = procedure (const AItem: TObject; const AName: string = '');
  TDelegateAddByTemplate = function(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TObject;
  TDelegateAddCustom = function(const ASubject: TSoObject; const AParams: TList<TObject>; const AName: string = ''): TObject;

  IRegisterDelegateCollector = interface
    procedure RegisterDelegateGet(const AClass: TClass; const ADelegate: TDelegateGet);
    procedure RegisterDelegateGetByName(const AClass: TClass; const ADelegate: TDelegateGetByName);
    procedure RegisterDelegateAdd(const AClass: TClass; const ADelegate: TDelegateAdd);
    procedure RegisterDelegateAddByTemplate(const AClass: TClass; const ADelegate: TDelegateAddByTemplate);
    procedure RegisterDelegateAddCustom(const AClass: TClass; const ADelegate: TDelegateAddCustom);
  end;

implementation

end.
