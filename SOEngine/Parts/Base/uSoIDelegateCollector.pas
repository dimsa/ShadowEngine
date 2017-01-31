unit uSoIDelegateCollector;

interface

uses
  uSoTypes, uSoObject;

type
  TDelegateGet = function(const ASubject: TSoObject): TObject of object; // Get First Item Part
  TDelegateGetByName = function(const ASubject: TSoObject; const AName: string = ''): TObject of object; // Get Item Part By Name

  TDelegateAdd = procedure (const AItem: TObject; const AName: string = '') of object;
  TDelegateAddByTemplate = function(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TObject of object;
  TDelegateAddCustom = function(const ASubject: TSoObject; const AParams: TList<TObject>; const AName: string = ''): TObject of object;

  IRegisterDelegateCollector = interface
    procedure RegisterDelegateGet(const AClass: TClass; const ADelegate: TDelegateGet);
    procedure RegisterDelegateGetByName(const AClass: TClass; const ADelegate: TDelegateGetByName);
    procedure RegisterDelegateAdd(const AClass: TClass; const ADelegate: TDelegateAdd);
    procedure RegisterDelegateAddByTemplate(const AClass: TClass; const ADelegate: TDelegateAddByTemplate);
    procedure RegisterDelegateAddCustom(const AClass: TClass; const ADelegate: TDelegateAddCustom);
  end;

implementation

end.
