unit uSoContainerAdder;

interface

uses
  uSoObject, uE2DRendition, uSoColliderObject, uSoMouseHandler, uSoKeyHandler, uSoSound,
  uSoFormatter, uSoAnimation, uSoLogic, uSoProperty, uSoModel;

type
  TSoContainerAdder = class
  private type
    TSoModelFriend = class(TSoModel);
    TSoObjectFriend = class(TSoObject);
  private
    FObject: TSoObject;
    FModel: TSoModelFriend;
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
    constructor Create(const AObject: TSoObject; const AModel: TSoModel);
  end;

implementation

{ TSoContainerAdder }

function TSoContainerAdder.Logic(const ATemplateName, AName: string): TSoLogic;
begin

end;

function TSoContainerAdder.Animation(const ATemplateName: string; const AName: string): TSoAnimation;
begin

end;

function TSoContainerAdder.Any<T>(AObject: TObject; const AName: string): T;
begin

end;

function TSoContainerAdder.Collider(const ATemplateName: string; const AName: string): TSoColliderObj;
begin

end;

constructor TSoContainerAdder.Create(const AObject: TSoObject; const AModel: TSoModel);
begin
  FModel := TSoModelFriend(AModel);
  FObject := AObject;
end;

function TSoContainerAdder.Formatter(const ATemplateName: string; const AName: string): TSoFormatter;
begin

end;

function TSoContainerAdder.KeyHandler(const ATemplateName: string; const AName: string): TSoKeyHandler;
begin

end;

function TSoContainerAdder.MouseHandler(const ATemplateName: string; const AName: string): TSoMouseHandler;
begin

end;

function TSoContainerAdder.Prop(const ATemplateName: string; const AName: string): TSoProperty;
begin

end;

function TSoContainerAdder.Rendition(const ATemplateName: string; const AName: string): TEngine2DRendition;
begin

end;

function TSoContainerAdder.Sound(const ATemplateName: string; const AName: string): TSoSound;
begin

end;

end.
