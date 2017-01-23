unit uSoContainerAdder;

interface

uses
  uSoTypes,
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
    FElementAdded: TNotifyEvent<string>;
    property OnElementAdd: TAddContainerElementDelegate read FOnElementAdd;
    property OnElementByTemplateAdd: TAddContainerElementByTemplateDelegate read FOnElementByTemplateAdd;
    function RaiseOnAdd(AClass: TClass; ADescription: TContainerElementByTemplate): TObject;
  public
    property ElementAdded: TNotifyEvent<string> write FElementAdded;
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

function TSoContainerAdder.Any<T>(const AObject: TObject; const AName: string): T;
begin
  Result := T(FOnElementAdd(T, TContainerElement.Create(AObject, AName)));
end;

function TSoContainerAdder.Any<T>(const ATemplateName, AName: string): T;
begin
  if (T = TSoAnimation) then
    Exit(T(Animation(ATemplateName, AName)));

  if (T = TEngine2DRendition) then
    Exit(T(Rendition(ATemplateName, AName)));

  if (T = TSoMouseHandler) then
    Exit(T(MouseHandler(ATemplateName, AName)));

  if (T = TSoKeyHandler) then
    Exit(T(KeyHandler(ATemplateName, AName)));

  if (T = TSoSound) then
    Exit(T(Sound(ATemplateName, AName)));

  if (T = TSoFormatter) then
    Exit(T(Formatter(ATemplateName, AName)));

  if (T = TSoAnimation) then
    Exit(T(Animation(ATemplateName, AName)));

  if (T = TSoLogic) then
    Exit(T(Logic(ATemplateName, AName)));

    raise Exception.Create('Can not produce class ' + T.ClassName);
end;

function TSoContainerAdder.RaiseOnAdd(AClass: TClass; ADescription: TContainerElementByTemplate): TObject;
begin
  Result := FOnElementByTemplateAdd(AClass, ADescription);
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

function TSoContainerAdder.Collider(const ATemplateName: string; const AName: string): TSoColliderObj;
begin
  Result :=
    FContainerDelegateCollector.ColliderObjContainerDelegateByTemplate(FSubject, ATemplateName, AName);
end;

function TSoContainerAdder.Logic(const ATemplateName, AName: string): TSoLogic;
begin
  Result :=
    FContainerDelegateCollector.LogicContainerDelegateByTemplate(FSubject, ATemplateName, AName);
end;

function TSoContainerAdder.Animation(const ATemplateName: string; const AName: string): TSoAnimation;
begin
  Result :=
    FContainerDelegateCollector.AnimationContainerDelegateByTemplate(FSubject, ATemplateName, AName);
end;

function TSoContainerAdder.Formatter(const ATemplateName: string; const AName: string): TSoFormatter;
begin
  Result :=
    FContainerDelegateCollector.Formatter—ontainerDelegateByTemplate(FSubject, ATemplateName, AName);
end;

function TSoContainerAdder.KeyHandler(const ATemplateName: string; const AName: string): TSoKeyHandler;
begin
  Result :=
    FContainerDelegateCollector.KeyHandlerContainerDelegateByTemplate(FSubject, ATemplateName, AName);
end;

function TSoContainerAdder.MouseHandler(const ATemplateName: string; const AName: string): TSoMouseHandler;
begin
  Result :=
    FContainerDelegateCollector.MouseHandlerContainerDelegateByTemplate(FSubject, ATemplateName, AName);
end;

function TSoContainerAdder.Prop(const ATemplateName: string; const AName: string): TSoProperty;
begin
  raise Exception.Create('Adding of property is not implemented');
{  Result :=
    FContainerDelegateCollector.PropertiesDelegateByTemplate(FSubject, ATemplateName, AName);}
end;

function TSoContainerAdder.Rendition(const ATemplateName: string; const AName: string): TEngine2DRendition;
begin
  Result :=
    FContainerDelegateCollector.RenditionContainerDelegateByTemplate(FSubject, ATemplateName, AName);
end;

function TSoContainerAdder.Sound(const ATemplateName: string; const AName: string): TSoSound;
begin
  Result :=
    FContainerDelegateCollector.SoundContainerDelegateByTemplate(FSubject, ATemplateName, AName);
end;

end.
