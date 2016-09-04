unit uSoManager;

interface

uses
  uSoModel, uSoContainer,
  uE2DRendition, uSoColliderObject, uSoMouseHandler, uSoKeyHandler, uSoFormatter, uSoAnimation,
  uSoLogic;

type
  TSoModelFriend = class(TSoModel);

  TSoManager = class
  private
    FModel: TSoModelFriend;
    FActiveContainer: TSoContainer;
  public
    function AddRendition(const ATemplateName: string): TEngine2DRendition; overload;
    function AddRendition(const AObject: TEngine2DRendition): TEngine2DRendition; overload;

    function AddColliderObj(const ATemplateName: string): TSoColliderObj; overload;
    function AddColliderObj(const AObject: TSoColliderObj): TSoColliderObj; overload;

    function AddMouseHandler(const ATemplateName: string): TSoMouseHandler; overload;
    function AddMouseHandler(const AObject: TSoMouseHandler): TSoMouseHandler; overload;

    function AddKeyHandler(const ATemplateName: string): TSoKeyHandler; overload;
    function AddKeyHandler(const AObject: TSoKeyHandler): TSoKeyHandler; overload;

    function AddFormatter(const ATemplateName: string): TSoFormatter; overload;
    function AddFormatter(const AObject: TSoFormatter): TSoFormatter; overload;

    function AddAnimation(const ATemplateName: string): TSoAnimation; overload;
    function AddAnimation(const AObject: TSoAnimation): TSoAnimation; overload;

    function AddLogic(const ATemplateName: string): TSoLogic; overload;
    function AddLogic(const AObject: TSoLogic): TSoLogic; overload;

    procedure Activate(const AContainer: TSoContainer);
    constructor Create(const AModel: TSoModel);
  end;

implementation

{ TSoManager }

procedure TSoManager.Activate(const AContainer: TSoContainer);
begin
  FActiveContainer := AContainer;
end;

function TSoManager.AddColliderObj(const ATemplateName: string): TSoColliderObj;
begin
  Result := FModel.Collider.AddFromTemplate(FActiveContainer, ATemplateName);
end;

function TSoManager.AddAnimation(const ATemplateName: string): TSoAnimation;
begin
  Result := FModel.Animator.AddFromTemplate(FActiveContainer, ATemplateName);
end;

function TSoManager.AddAnimation(const AObject: TSoAnimation): TSoAnimation;
begin
  FModel.Animator.Add(AObject);
  Result := AObject;
end;

function TSoManager.AddColliderObj(const AObject: TSoColliderObj): TSoColliderObj;
begin
  FModel.Collider.Add(AObject);
  Result := AObject;
end;

function TSoManager.AddFormatter(const ATemplateName: string): TSoFormatter;
begin
  FModel.Formattor.AddFromTemplate(FActiveContainer, ATemplateName);
end;

function TSoManager.AddFormatter(const AObject: TSoFormatter): TSoFormatter;
begin
  FModel.Formattor.Add(AObject);
  Result:= AObject;
end;

function TSoManager.AddKeyHandler(const ATemplateName: string): TSoKeyHandler;
begin
  Result := FModel.KeyProcessor.AddFromTemplate(FActiveContainer, ATemplateName);
end;

function TSoManager.AddKeyHandler(const AObject: TSoKeyHandler): TSoKeyHandler;
begin
  FModel.KeyProcessor.Add(AObject);
  Result := AObject;
end;

function TSoManager.AddLogic(const ATemplateName: string): TSoLogic;
begin
  Result := FModel.LogicKeeper.AddFromTemplate(FActiveContainer, ATemplateName);
end;

function TSoManager.AddLogic(const AObject: TSoLogic): TSoLogic;
begin
  FModel.LogicKeeper.Add(AObject);
  Result := AObject;
end;

function TSoManager.AddMouseHandler(const AObject: TSoMouseHandler): TSoMouseHandler;
begin
  FModel.MouseProcessor.Add(AObject);
  Result := AObject;
end;

function TSoManager.AddMouseHandler(const ATemplateName: string): TSoMouseHandler;
begin
  Result := FModel.MouseProcessor.AddFromTemplate(FActiveContainer, ATemplateName);
end;

function TSoManager.AddRendition(const AObject: TEngine2DRendition): TEngine2DRendition;
begin
  FModel.Renderer.Add(AObject);
end;

function TSoManager.AddRendition(const ATemplateName: string): TEngine2DRendition;
begin
  FModel.Renderer.AddFromTemplate(FActiveContainer, ATemplateName);
end;

constructor TSoManager.Create(const AModel: TSoModel);
begin
  FModel := TSoModelFriend(AModel);
end;

end.
