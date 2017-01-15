unit uSoIOperator;

interface

uses
  uSoTypes, uSoObject, uSoIDelegateCollector;

type
  ISoOperator = interface
  ['{3493EB24-9718-45FB-B79C-46D91A202F46}']
    function OperatorItemClass: TClass;

    function Contains(const AName: string): Boolean; overload;
    function Contains(const AItem: TObject): Boolean; overload;
    function NameOf(const AItem: TObject): string;

    function Get(const ASubject: TSoObject): TObject; overload;
    function Get(const ASubject: TSoObject; const AName: string): TObject; overload;

    function AddFromTemplate(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TObject;
    procedure Add(const AItem: TObject; const AName: string = '');

    procedure VisitByDelegateCollector(const ADelegateCollector: IDelegateCollector);
  end;

implementation

end.
