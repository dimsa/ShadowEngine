unit uSoContainerGetter;

interface

uses
  uSoObject, uE2DRendition, uSoColliderObject, uSoMouseHandler, uSoKeyHandler, uSoSound,
  uSoFormatter, uSoAnimation, uSoLogic, uSoProperty, uSoModel;

type
  TSoContainerGetter = class
  private type
    TSoModelFriend = class(TSoModel);
    TSoObjectFriend = class(TSoObject);
  private
    FObject: TSoObject;
    FModel: TSoModelFriend;
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
    constructor Create(const AObject: TSoObject; const AModel: TSoModel);
  end;

implementation

{ TSoContainerGetter }

function TSoContainerGetter.Animation(const AName: string): TSoAnimation;
begin

end;

function TSoContainerGetter.Any<T>(const AName: string): T;
begin

end;

function TSoContainerGetter.Collider(const AName: string): TSoColliderObj;
begin

end;

constructor TSoContainerGetter.Create(const AObject: TSoObject;
  const AModel: TSoModel);
begin
  FModel := TSoModelFriend(AModel);
  FObject := AObject;
end;

function TSoContainerGetter.Formatter(const AName: string): TSoFormatter;
begin

end;

function TSoContainerGetter.KeyHandler(const AName: string): TSoKeyHandler;
begin

end;

function TSoContainerGetter.Logic(const AName: string): TSoLogic;
begin

end;

function TSoContainerGetter.MouseHandler(const AName: string): TSoMouseHandler;
begin

end;

function TSoContainerGetter.Prop(const AName: string): TSoProperty;
begin

end;

function TSoContainerGetter.Rendition(const AName: string): TEngine2DRendition;
begin

end;

function TSoContainerGetter.Sound(const AName: string): TSoSound;
begin

end;

end.
