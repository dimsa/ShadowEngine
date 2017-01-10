unit uSoContainerTypes;

interface

uses
  uSoObject, uSoBasePart;

type

  TContainerElementByTemplate = record
    Name: string;
    TemplateName: string;
    constructor Create(AName, ATemplateName: string);
  end;

  TContainerElement = record
    Name: string;
    Element: TObject;
    constructor Create(AElement: TObject; AName: string);
  end;

  TDoubleParameteredDelegate<TClassTemplate, T> =
    function(AClass: TClassTemplate; AName: T): TObject of object;

  TGetContainerElementDelegate = TDoubleParameteredDelegate<TClass, string>;
  TAddContainerElementDelegate = TDoubleParameteredDelegate<TClass, TContainerElement>;
  TAddContainerElementByTemplateDelegate = TDoubleParameteredDelegate<TClass, TContainerElementByTemplate>;

 TTripleParameteredDelegate<T1, T2, T3> =
    function(ASender: T1; AClass: T2; AName: T3): TObject of object;

 TContainerOnGetDelegate = TTripleParameteredDelegate<TSoObject, TClass, string>;
 TContainerOnAddDelegate = TTripleParameteredDelegate<TSoObject, TClass, TContainerElement>;
 TContainerOnAddByTemplateDelegate = TTripleParameteredDelegate<TSoObject, TClass, TContainerElementByTemplate>;

  TOnAddContainerEventArgs = record
    Subject: TSoObject;
    BasePart: TSoBasePart;
    constructor Create(const ASubject: TSoObject; ABasePart: TSoBasePart);
  end;

implementation

{ TOnAddContainerEventArgs }

constructor TOnAddContainerEventArgs.Create(const ASubject: TSoObject; ABasePart: TSoBasePart);
begin
  Subject := ASubject;
  BasePart := ABasePart;
end;

{ TContainerElementDescription }

constructor TContainerElementByTemplate.Create(AName, ATemplateName: string);
begin
  Name := AName;
  TemplateName := ATemplateName;
end;

{ TContainerElement }

constructor TContainerElement.Create(AElement: TObject; AName: string);
begin
  Element := AElement;
  Name := AName;
end;

end.
