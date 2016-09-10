unit uUnitManager;

interface

uses
  uSoModel, uSoContainer,
  uE2DRendition, uSoColliderObject, uSoMouseHandler, uSoKeyHandler, uSoFormatter, uSoAnimation,
  uSoLogic;

type
  TSoModelFriend = class(TSoModel);

  TUnitManager = class
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

    function AddContainer(const AName: string = ''): TSoContainer;
    function Manage(const AContainer: TSoContainer): TUnitManager;
    function ManageNew(const AName: string = ''): TUnitManager;

    property ActiveContainer: TSoContainer read FActiveContainer;
    procedure Activate(const AContainer: TSoContainer);
    constructor Create(const AModel: TSoModel);
  end;

  TManageDelegate = function(const AContainer: TSoContainer): TUnitManager of object;
  TManageNewDelegate = function(const AName: string = ''): TUnitManager of object;

implementation

{ TSoManager }

procedure TUnitManager.Activate(const AContainer: TSoContainer);
begin
  FActiveContainer := AContainer;
end;

function TUnitManager.AddColliderObj(const ATemplateName: string): TSoColliderObj;
begin
  Result := FModel.Collider.AddFromTemplate(FActiveContainer, ATemplateName);
end;

function TUnitManager.AddAnimation(const ATemplateName: string): TSoAnimation;
begin
  Result := FModel.Animator.AddFromTemplate(FActiveContainer, ATemplateName);
end;

function TUnitManager.AddAnimation(const AObject: TSoAnimation): TSoAnimation;
begin
  FModel.Animator.Add(AObject);
  Result := AObject;
end;

function TUnitManager.AddColliderObj(const AObject: TSoColliderObj): TSoColliderObj;
begin
  FModel.Collider.Add(AObject);
  Result := AObject;
end;

function TUnitManager.AddContainer(const AName: string): TSoContainer;
begin
  Result := FModel.ContainerKeeper.AddNewContainer(AName)
end;

function TUnitManager.AddFormatter(const ATemplateName: string): TSoFormatter;
begin
  FModel.Formattor.AddFromTemplate(FActiveContainer, ATemplateName);
end;

function TUnitManager.AddFormatter(const AObject: TSoFormatter): TSoFormatter;
begin
  FModel.Formattor.Add(AObject);
  Result:= AObject;
end;

function TUnitManager.AddKeyHandler(const ATemplateName: string): TSoKeyHandler;
begin
  Result := FModel.KeyProcessor.AddFromTemplate(FActiveContainer, ATemplateName);
end;

function TUnitManager.AddKeyHandler(const AObject: TSoKeyHandler): TSoKeyHandler;
begin
  FModel.KeyProcessor.Add(AObject);
  Result := AObject;
end;

function TUnitManager.AddLogic(const ATemplateName: string): TSoLogic;
begin
  Result := FModel.LogicKeeper.AddFromTemplate(FActiveContainer, ATemplateName);
end;

function TUnitManager.AddLogic(const AObject: TSoLogic): TSoLogic;
begin
  FModel.LogicKeeper.Add(AObject);
  Result := AObject;
end;

function TUnitManager.AddMouseHandler(const AObject: TSoMouseHandler): TSoMouseHandler;
begin
  FModel.MouseProcessor.Add(AObject);
  Result := AObject;
end;

function TUnitManager.AddMouseHandler(const ATemplateName: string): TSoMouseHandler;
begin
  Result := FModel.MouseProcessor.AddFromTemplate(FActiveContainer, ATemplateName);
end;

function TUnitManager.AddRendition(const AObject: TEngine2DRendition): TEngine2DRendition;
begin
  FModel.Renderer.Add(AObject);
end;

function TUnitManager.AddRendition(const ATemplateName: string): TEngine2DRendition;
begin
  FModel.Renderer.AddFromTemplate(FActiveContainer, ATemplateName);
end;

constructor TUnitManager.Create(const AModel: TSoModel);
begin
  FModel := TSoModelFriend(AModel);
end;

function TUnitManager.Manage(const AContainer: TSoContainer): TUnitManager;
begin
  FActiveContainer := AContainer;
  Result := Self;
end;

function TUnitManager.ManageNew(const AName: string = ''): TUnitManager;
begin
  Result := Manage(AddContainer(AName));
end;

end.

