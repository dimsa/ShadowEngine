unit uSoContainerAdder;

interface

uses
  uCommonClasses, uSoContainerTypes,
  uSoObject, uE2DRendition, uSoColliderObject, uSoMouseHandler, uSoKeyHandler, uSoSound,
  uSoFormatter, uSoAnimation, uSoLogic, uSoProperty;

type
  TSoContainerAdder = class
  private
    FOnAnyAdd: TAddContainerElementDelegate;
    property OnAnyAdd: TAddContainerElementDelegate read FOnAnyAdd;
    function RaiseOnAdd(AClass: TClass; ADescription: TContainerElementDescription): TObject;
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

    function Any<T: class>(AObject: TObject; const AName: string = ''): T;

    constructor Create(const AOnAnyAdd: TAddContainerElementDelegate);
  end;

implementation

{ TSoContainerAdder }

function TSoContainerAdder.Logic(const ATemplateName, AName: string): TSoLogic;
begin
  Result := TSoLogic(
    RaiseOnAdd(TSoLogic, TContainerElementDescription.Create(AName, ATemplateName)));
end;

function TSoContainerAdder.Animation(const ATemplateName: string; const AName: string): TSoAnimation;
begin
  Result := TSoAnimation(
    RaiseOnAdd(TSoAnimation, TContainerElementDescription.Create(AName, ATemplateName)));
end;

function TSoContainerAdder.Any<T>(AObject: TObject; const AName: string): T;
begin

end;

function TSoContainerAdder.Collider(const ATemplateName: string; const AName: string): TSoColliderObj;
begin
  Result := TSoColliderObj(
    RaiseOnAdd(TSoColliderObj, TContainerElementDescription.Create(AName, ATemplateName)));
end;

constructor TSoContainerAdder.Create(const AOnAnyAdd: TAddContainerElementDelegate);
begin
  FOnAnyAdd := AOnAnyAdd;
end;

function TSoContainerAdder.Formatter(const ATemplateName: string; const AName: string): TSoFormatter;
begin
  Result := TSoFormatter(
    RaiseOnAdd(TSoFormatter, TContainerElementDescription.Create(AName, ATemplateName)));
end;

function TSoContainerAdder.KeyHandler(const ATemplateName: string; const AName: string): TSoKeyHandler;
begin
  Result := TSoKeyHandler(
    RaiseOnAdd(TSoKeyHandler, TContainerElementDescription.Create(AName, ATemplateName)));
end;

function TSoContainerAdder.MouseHandler(const ATemplateName: string; const AName: string): TSoMouseHandler;
begin
  Result := TSoMouseHandler(
    RaiseOnAdd(TSoMouseHandler, TContainerElementDescription.Create(AName, ATemplateName)));
end;

function TSoContainerAdder.Prop(const ATemplateName: string; const AName: string): TSoProperty;
begin
  Result := TSoProperty(
    RaiseOnAdd(TSoProperty, TContainerElementDescription.Create(AName, ATemplateName)));
end;

function TSoContainerAdder.RaiseOnAdd(AClass: TClass; ADescription: TContainerElementDescription): TObject;
begin
  Result := FOnAnyAdd(AClass, ADescription);
end;

function TSoContainerAdder.Rendition(const ATemplateName: string; const AName: string): TEngine2DRendition;
begin
  Result := TEngine2DRendition(
    RaiseOnAdd(TEngine2DRendition, TContainerElementDescription.Create(AName, ATemplateName)));
end;

function TSoContainerAdder.Sound(const ATemplateName: string; const AName: string): TSoSound;
begin
  Result := TSoSound(
    RaiseOnAdd(TSoSound, TContainerElementDescription.Create(AName, ATemplateName)));
end;

end.
