unit uSoIOperator;

interface

uses
  uSoTypes, uSoObject, uSoIDelegateCollector;

type
  ISoOperator = interface
  ['{3493EB24-9718-45FB-B79C-46D91A202F46}']
    function OperatorItemClass: TClass;

    function ContainsObj(const AItem: TObject): Boolean; overload;
    function NameOfObj(const AItem: TObject): string;

    function GetObj(const ASubject: TSoObject): TObject;

    function AddObjFromTemplate(const ASubject: TSoObject; const ATemplateName: string): TObject;
    procedure AddObj(const AItem: TObject);

    procedure VisitByDelegateCollector(const ADelegateCollector: IRegisterDelegateCollector);
  end;

implementation

end.
