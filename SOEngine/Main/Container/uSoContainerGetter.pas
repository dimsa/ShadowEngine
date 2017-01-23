unit uSoContainerGetter;

interface

uses
  uSoTypes, uE2DRendition, uSoColliderObject, uSoMouseHandler, uSoKeyHandler, uSoSound,
  uSoFormatter, uSoAnimation, uSoLogic, uSoProperty, uSoContainerTypes;

type
  TSoContainerGetter = class
  private type
    TNameDict = TDict<string, TObject>;
  private
    FPartByName: TDict<TClass, TNameDict>;
    FOnAnyGet: TGetContainerElementDelegate;
    property OnAnyGet: TGetContainerElementDelegate read FOnAnyGet;
    function RaiseOnGet(AClass: TClass; AName: string): TObject;
  public
    function Rendition(const AName: string = ''): TEngine2DRendition; overload;
    function Collider(const AName: string = ''): TSoColliderObj; overload;
    function MouseHandler(const AName: string = ''): TSoMouseHandler; overload;
    function KeyHandler(const AName: string = ''): TSoKeyHandler; overload;
    function Sound(const AName: string = ''): TSoSound; overload;
    function Formatter(const AName: string = ''): TSoFormatter; overload;
    function Animation(const AName: string = ''): TSoAnimation; overload;
    function Logic(const AName: string = ''): TSoLogic; overload;
    function Prop(const AName: string = ''): TSoProperty; overload;

    function Any<T: class>(const AName: string = ''): T;
    constructor Create(const AOnAnyGet: TGetContainerElementDelegate);
    destructor Destroy; override;
  end;

implementation

{ TSoContainerGetter }

function TSoContainerGetter.Animation(const AName: string): TSoAnimation;
begin
  Result := TSoAnimation(RaiseOnGet(TSoAnimation, AName));
end;

function TSoContainerGetter.Any<T>(const AName: string): T;
begin
  Result := T(RaiseOnGet(T, AName));
end;

function TSoContainerGetter.Collider(const AName: string): TSoColliderObj;
begin
  Result := TSoColliderObj(RaiseOnGet(TSoColliderObj, AName));
end;

constructor TSoContainerGetter.Create(const AOnAnyGet: TGetContainerElementDelegate);
begin
  FOnAnyGet := AOnAnyGet;
end;

destructor TSoContainerGetter.Destroy;
begin
  FOnAnyGet := nil;
  inherited;
end;

function TSoContainerGetter.Formatter(const AName: string): TSoFormatter;
begin
  Result := TSoFormatter(RaiseOnGet(TSoFormatter, AName));
end;

function TSoContainerGetter.KeyHandler(const AName: string): TSoKeyHandler;
begin
  Result := TSoKeyHandler(RaiseOnGet(TSoKeyHandler, AName));
end;

function TSoContainerGetter.Logic(const AName: string): TSoLogic;
begin
  Result := TSoLogic(RaiseOnGet(TSoLogic, AName));
end;

function TSoContainerGetter.MouseHandler(const AName: string): TSoMouseHandler;
begin
  Result := TSoMouseHandler(RaiseOnGet(TSoMouseHandler, AName));
end;

function TSoContainerGetter.Prop(const AName: string): TSoProperty;
begin
  Result := TSoProperty(RaiseOnGet(TSoProperty, AName));
end;

function TSoContainerGetter.RaiseOnGet(AClass: TClass; AName: string): TObject;
begin
  Result := FOnAnyGet(AClass, AName);
end;

function TSoContainerGetter.Rendition(const AName: string): TEngine2DRendition;
begin
  Result := TEngine2DRendition(RaiseOnGet(TEngine2DRendition, AName));
end;

function TSoContainerGetter.Sound(const AName: string): TSoSound;
begin
  Result := TSoSound(RaiseOnGet(TSoSound, AName));
end;

end.
