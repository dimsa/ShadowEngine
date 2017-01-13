unit uSoContainerAdder;

interface

uses
  uCommonClasses, uSoContainerTypes,
  uE2DRendition, uSoColliderObject, uSoMouseHandler, uSoKeyHandler, uSoSound,
  uSoFormatter, uSoAnimation, uSoLogic, uSoProperty, uSoObject, uSoContainerDelegateCollector;

type
  TSoContainerAdder = class
  private
    FOnElementAdd: TAddContainerElementDelegate;
    FOnElementByTemplateAdd: TAddContainerElementByTemplateDelegate;
    FSubject: TSoObject;
    FContainerDelegateCollector: TSoContainerDelegateCollector;
    property OnElementAdd: TAddContainerElementDelegate read FOnElementAdd;
    property OnElementByTemplateAdd: TAddContainerElementByTemplateDelegate read FOnElementByTemplateAdd;
    function RaiseOnAdd(AClass: TClass; ADescription: TContainerElementByTemplate): TObject;
  public
    function Rendition(const ATemplateName: string; const AName: string = ''): TEngine2DRendition; overload;
    function Collider(const ATemplateName: string; const AName: string = ''): TSoColliderObj; overload;
    function MouseHandler(const ATemplateName: string; const AName: string = ''): TSoMouseHandler; overload;
    function KeyHandler(const ATemplateName: string; const AName: string = ''): TSoKeyHandler; overload;
    function Sound(const ATemplateName: string; const AName: string = ''): TSoSound; overload;
    function Formatter(const ATemplateName: string; const AName: string = ''): TSoFormatter; overload;
    function Animation(const ATemplateName: string; const AName: string = ''): TSoAnimation; overload;
    function Logic(const ATemplateName: string; const AName: string = ''): TSoLogic; overload;
    function Prop(const ATemplateName: string; const AName: string = ''): TSoProperty; overload;

    function Any<T: class>(const ATemplateName: string; const AName: string = ''): T; overload;
    function Any<T: class>(const AObject: TObject; const AName: string = ''): T; overload;

    constructor Create(
      const ASubject: TSoObject;
      const AContainerDelegateCollector: TSoContainerDelegateCollector);
    destructor Destroy; override;
  end;

implementation

{ TSoContainerAdder }

function TSoContainerAdder.Logic(const ATemplateName, AName: string): TSoLogic;
begin
  Result := TSoLogic(
    RaiseOnAdd(TSoLogic, TContainerElementByTemplate.Create(AName, ATemplateName)));
end;

function TSoContainerAdder.Animation(const ATemplateName: string; const AName: string): TSoAnimation;
begin
  Result := TSoAnimation(
    RaiseOnAdd(TSoAnimation, TContainerElementByTemplate.Create(AName, ATemplateName)));
end;

function TSoContainerAdder.Any<T>(const AObject: TObject; const AName: string): T;
begin
  Result := T(FOnElementAdd(T, TContainerElement.Create(AObject, AName)));
end;

function TSoContainerAdder.Any<T>(const ATemplateName, AName: string): T;
begin
  Result := T(RaiseOnAdd(T, TContainerElementByTemplate.Create(AName, ATemplateName)));
end;

function TSoContainerAdder.Collider(const ATemplateName: string; const AName: string): TSoColliderObj;
begin
  Result := TSoColliderObj(
    RaiseOnAdd(TSoColliderObj, TContainerElementByTemplate.Create(AName, ATemplateName)));
end;

constructor TSoContainerAdder.Create(
      const ASubject: TSoObject;
      const AContainerDelegateCollector: TSoContainerDelegateCollector);
begin
  FSubject := ASubject;
  FContainerDelegateCollector := AContainerDelegateCollector;
end;

destructor TSoContainerAdder.Destroy;
begin
  FOnElementAdd := nil;
  FOnElementByTemplateAdd := nil;
  inherited;
end;

function TSoContainerAdder.Formatter(const ATemplateName: string; const AName: string): TSoFormatter;
begin
  Result := TSoFormatter(
    RaiseOnAdd(TSoFormatter, TContainerElementByTemplate.Create(AName, ATemplateName)));
end;

function TSoContainerAdder.KeyHandler(const ATemplateName: string; const AName: string): TSoKeyHandler;
begin
  Result := TSoKeyHandler(
    RaiseOnAdd(TSoKeyHandler, TContainerElementByTemplate.Create(AName, ATemplateName)));
end;

function TSoContainerAdder.MouseHandler(const ATemplateName: string; const AName: string): TSoMouseHandler;
begin
  Result := TSoMouseHandler(
    RaiseOnAdd(TSoMouseHandler, TContainerElementByTemplate.Create(AName, ATemplateName)));
end;

function TSoContainerAdder.Prop(const ATemplateName: string; const AName: string): TSoProperty;
begin
  Result := TSoProperty(
    RaiseOnAdd(TSoProperty, TContainerElementByTemplate.Create(AName, ATemplateName)));
end;

function TSoContainerAdder.RaiseOnAdd(AClass: TClass; ADescription: TContainerElementByTemplate): TObject;
begin
  Result := FOnElementByTemplateAdd(AClass, ADescription);
end;

function TSoContainerAdder.Rendition(const ATemplateName: string; const AName: string): TEngine2DRendition;
begin
  Result := TEngine2DRendition(
    RaiseOnAdd(TEngine2DRendition, TContainerElementByTemplate.Create(AName, ATemplateName)));
end;

function TSoContainerAdder.Sound(const ATemplateName: string; const AName: string): TSoSound;
begin
  Result := TSoSound(
    RaiseOnAdd(TSoSound, TContainerElementByTemplate.Create(AName, ATemplateName)));
end;

end.
