unit uUnitManager;

interface

uses
  uCommonClasses, uSoTypes,
  uSoModel, uSoObject,
  uE2DRendition, uSoColliderObject, uSoMouseHandler, uSoKeyHandler, uSoFormatter, uSoAnimation,
  uSoLogic, uSoProperties, uSoProperty, uColliderDefinition, uSoSound,
  uSoContainer, uSoContainerKeeper,
  uSoLayout, uISoPositionAdapter, uSoLayoutFactory;

type
  TSoModelFriend = class(TSoModel);
  TSoObjectFriend = class(TSoObject);

  TUnitManager = class
  private
    FModel: TSoModelFriend;
    FContainerKeeper: TSoContainerKeeper;

    FActiveContainer: TSoObject;
    FLayoutAdded: TNotifyEvent;

    function AddContainer(const AName: string = ''): TSoObject;
    procedure Activate(const AContainer: TSoObject);
    procedure RaiseLayoutAdded(ALayout: TSoLayout);
  protected
    property LayoutAdded: TNotifyEvent read FLayoutAdded write FLayoutAdded;
  public
    function AddRendition(const ATemplateName: string): TEngine2DRendition; overload;
    function AddRendition(const AObject: TEngine2DRendition): TEngine2DRendition; overload;

    function AddColliderObj(const ATemplateName: string): TSoColliderObj; overload;
    function AddColliderObj(const ADefinition: TColliderDefinition): TSoColliderObj; overload;

    function AddMouseHandler(const ATemplateName: string): TSoMouseHandler; overload;
    function AddMouseHandler(const AObject: TSoMouseHandler): TSoMouseHandler; overload;

    function AddKeyHandler(const ATemplateName: string): TSoKeyHandler; overload;
    function AddKeyHandler(const AObject: TSoKeyHandler): TSoKeyHandler; overload;

    function AddSound(const APath: string): TSoSound; overload;
    function AddSoundFromTemplate(const ATemplateName: string): TSoSound; overload;
    function AddSound(const AObject: TSoSound): TSoSound; overload;

    function AddFormatter(const ATemplateName: string): TSoFormatter; overload;
    function AddFormatter(const AObject: TSoFormatter): TSoFormatter; overload;

    function AddAnimation(const ATemplateName: string): TSoAnimation; overload;
    function AddAnimation(const AObject: TSoAnimation): TSoAnimation; overload;

    function AddLogic(const ATemplateName: string; const AName: string = ''): TSoLogic; overload;
    function AddLogic(const AObject: TSoLogic; const AName: string = ''): TSoLogic; overload;
    function AddNewLogic(const AHandler: TNotifyEvent<TSoObject>; const AName: string = ''): TSoLogic; overload;
    function AddNewLogic(const AHandler: TStaticNotifyEvent<TSoObject>; const AName: string = ''): TSoLogic; overload;

    function AddProperty(const ATemplateName: string): TSoProperty; overload;
    function AddProperty(const AName: string; const AValue: TObject): TSoProperty; overload;

    function ByObject(const AContainer: TSoObject): TUnitManager; overload;
    function ByName(const AContainerName: string): TUnitManager; overload;
    function New(const AName: string = ''): TUnitManager;
    function ObjectByName(const AObjectName: string): TSoObject;

    function Add320: TSoContainer; // Add SoContainer with SoObject with 320x240 PositionAdapter
    function AddAbs: TSoContainer; // Add SoContainer with SoObject with Absolute PositionAdapter

    property ActiveContainer: TSoObject read FActiveContainer;

    constructor Create(const AModel: TSoModel; const ALayoutFactory: TSoLayoutFactory);
    destructor Destroy; override;
  end;

implementation

{ TSoManager }

procedure TUnitManager.Activate(const AContainer: TSoObject);
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

function TUnitManager.Add320: TSoContainer;
begin
  Result := FContainerKeeper.Add320;
end;

function TUnitManager.AddAbs: TSoContainer;
begin
  Result := FContainerKeeper.AddAbs;
end;

function TUnitManager.AddAnimation(const AObject: TSoAnimation): TSoAnimation;
begin
  FModel.Animator.Add(AObject);
  Result := AObject;
end;

function TUnitManager.AddColliderObj(
  const ADefinition: TColliderDefinition): TSoColliderObj;
begin
  FModel.Collider.Add(FActiveContainer, ADefinition);
end;

function TUnitManager.AddContainer(const AName: string): TSoObject;
begin
  Result := FModel.ObjectKeeper.AddNewObject(AName)
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

function TUnitManager.AddLogic(const ATemplateName: string; const AName: string): TSoLogic;
begin
  Result := FModel.LogicKeeper.AddFromTemplate(FActiveContainer, ATemplateName, AName);
end;

function TUnitManager.AddLogic(const AObject: TSoLogic; const AName: string): TSoLogic;
begin
  FModel.LogicKeeper.Add(AObject, AName);
  Result := AObject;
end;

function TUnitManager.AddMouseHandler(const AObject: TSoMouseHandler): TSoMouseHandler;
begin
  FModel.MouseProcessor.Add(AObject);
  Result := AObject;
end;

function TUnitManager.AddProperty(const ATemplateName: string): TSoProperty;
begin
  raise Exception.Create('Add property from template not Implemented');
end;

function TUnitManager.AddProperty(const AName: string; const AValue: TObject): TSoProperty;
begin
  Result := TSoObjectFriend(FActiveContainer).FProperties.Add(AName);
  Result.Obj := AValue;
end;

function TUnitManager.AddMouseHandler(const ATemplateName: string): TSoMouseHandler;
begin
  Result := FModel.MouseProcessor.AddFromTemplate(FActiveContainer, ATemplateName);
end;

function TUnitManager.AddRendition(const AObject: TEngine2DRendition): TEngine2DRendition;
begin
  FModel.Renderer.Add(AObject);
end;

function TUnitManager.AddSound(const APath: string): TSoSound;
begin
  Result := FModel.SoundKeeper.AddByFileName(FActiveContainer, APath);
end;

function TUnitManager.AddSound(const AObject: TSoSound): TSoSound;
begin
  FModel.SoundKeeper.Add(AObject);
end;

function TUnitManager.AddSoundFromTemplate(const ATemplateName: string): TSoSound;
begin
  Result := FModel.SoundKeeper.AddFromTemplate(FActiveContainer, ATemplateName);
end;

function TUnitManager.AddRendition(const ATemplateName: string): TEngine2DRendition;
begin
  FModel.Renderer.AddFromTemplate(FActiveContainer, ATemplateName);
end;

constructor TUnitManager.Create(const AModel: TSoModel; const ALayoutFactory: TSoLayoutFactory);
begin
  FModel := TSoModelFriend(AModel);
end;

destructor TUnitManager.Destroy;
begin

  inherited;
end;

function TUnitManager.ByObject(const AContainer: TSoObject): TUnitManager;
begin
  FActiveContainer := AContainer;
  Result := Self;
end;

function TUnitManager.ByName(const AContainerName: string): TUnitManager;
begin
  FActiveContainer := FModel.ObjectKeeper[AContainerName];
  Result := Self;
end;

function TUnitManager.New(const AName: string = ''): TUnitManager;
begin
  Result := ByObject(AddContainer(AName));
end;

function TUnitManager.ObjectByName(const AObjectName: string): TSoObject;
begin
  Result := FModel.ObjectByName(AObjectName);
end;

procedure TUnitManager.RaiseLayoutAdded(ALayout: TSoLayout);
begin
  if Assigned(FLayoutAdded) then
    FLayoutAdded(ALayout);
end;

function TUnitManager.AddNewLogic(const AHandler: TNotifyEvent<TSoObject>; const AName: string): TSoLogic;
begin
  Result := TSoObjectLogic.Create(FActiveContainer, AHandler);
  FModel.LogicKeeper.Add(Result, AName);
end;

function TUnitManager.AddNewLogic(const AHandler: TStaticNotifyEvent<TSoObject>;
  const AName: string): TSoLogic;
begin
  Result := TSoStaticLogic.Create(FActiveContainer, AHandler);
  FModel.LogicKeeper.Add(Result, AName);
end;

end.

