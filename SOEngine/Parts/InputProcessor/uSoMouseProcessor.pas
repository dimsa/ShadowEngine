unit uSoMouseProcessor;

interface

uses
  System.SysUtils,
  uSoTypes, uSoBaseOperator, uSoMouseHandler, uSoObject, uSoContainerTypes,
  uSoBasePart, uSoObjectDefaultProperties, uSoMouseHandleCheckers;

type
  TSoMouseHandlerFriend = class(TSoMouseHandler);

  TSoMouseProcessor = class(TSoOperator<TSoMouseHandler>)
  private
    FContainers: TDict<TSoObject, TSoMouseHandler>;
    FTemplates: TDict<string, TCheckMouseHandleBehavior>;
    FMouseOver, FOldMouseOver, FMouseDowned, FMouseUpped: TList<TSoObject>;
    procedure OnItemDestroy(ASender: TObject);
    procedure PrepareTemplates;
  public
    procedure ExecuteMouseUp(Args: TMouseEventArgs); // Process mouse on tick
    procedure ExecuteMouseDown(Args: TMouseEventArgs); // Process mouse on tick
    procedure ExecuteMouseMove(Args: TMouseMoveEventArgs); // Process mouse position on tick
    procedure Add(const AItem: TSoMouseHandler; const AName: string = ''); override;
    function AddFromTemplate(const ASubject: TSoObject; const ATemplateName: string; const AName: string = ''): TSoMouseHandler; override;
    constructor Create(const ACritical: TCriticalSection);
    destructor Destroy; override;
  end;

implementation

{ TSoMouseProcessor }

procedure TSoMouseProcessor.Add(const AItem: TSoMouseHandler;
  const AName: string);
var
  vName: string;
begin
  {$I .\Template\uItemAdd.inc}
  FContainers.Add(AItem.Subject, AItem);
end;

function TSoMouseProcessor.AddFromTemplate(const ASubject: TSoObject;
  const ATemplateName, AName: string): TSoMouseHandler;
begin
  Result := TSoMouseHandler.Create(ASubject, FTemplates[ATemplateName]);
  Add(Result, AName);
end;

constructor TSoMouseProcessor.Create(const ACritical: TCriticalSection);
begin
  inherited Create(ACritical);

  FOldMouseOver := TList<TSoObject>.Create;
  FMouseOver := TList<TSoObject>.Create;
  FMouseDowned := TList<TSoObject>.Create;
  FMouseUpped := TList<TSoObject>.Create;

  FContainers := TDict<TSoObject, TSoMouseHandler>.Create;
  FTemplates := TDict<string, TCheckMouseHandleBehavior>.Create;

  PrepareTemplates;
end;

destructor TSoMouseProcessor.Destroy;
begin
  FContainers.Clear;
  FContainers.Free;
  FTemplates.Free;

  FOldMouseOver.Free;
  FMouseOver.Free;
  FMouseDowned.Free;
  FMouseUpped.Free;
  inherited;
end;

procedure TSoMouseProcessor.ExecuteMouseDown(Args: TMouseEventArgs);
var
  i: Integer;
begin
  for i := 0 to FMouseOver.Count - 1 do
  begin
    TSoMouseHandlerFriend(FContainers[FMouseOver[i]]).MouseDown(Args);
  end;
  FMouseDowned := FMouseOver;
end;

procedure TSoMouseProcessor.ExecuteMouseMove(Args: TMouseMoveEventArgs);
var
  i, j: Integer;
  vWas: Boolean;
  vItem: TSoObject;
begin
  // Mouse moving at object
  FMouseOver.Clear;
  for i := 0 to FList.Count - 1 do
  begin
    if FList[i].CanExecute(Args.X {- FList[i].Subject.X} ,Args.Y{ - FList[i].Subject.Y}) then
    begin
      TSoMouseHandlerFriend(FContainers[FList[i].Subject]).MouseMove(Args);
      FMouseOver.Add(FList[i].Subject)
    end;
  end;

  for i := 0 to FMouseOver.Count - 1 do
  begin
    vWas := False;
    for j := i + 1 to FOldMouseOver.Count - 1 do
    begin
      if FMouseOver[i] = FOldMouseOver[j] then
      begin
        vWas := True;
        FOldMouseOver[j] := nil;
        Break;
      end;
    end;
    if vWas then
      TSoMouseHandlerFriend(FContainers[FMouseOver[i]]).MouseLeave
    else
      TSoMouseHandlerFriend(FContainers[FMouseOver[i]]).MouseEnter;
  end;

  FOldMouseOver.Clear;
  for i := 0 to FMouseOver.Count - 1 do
    FOldMouseOver.Add(FMouseOver[i]);
end;

procedure TSoMouseProcessor.ExecuteMouseUp(Args: TMouseEventArgs);
var
  i: Integer;
begin
  for i := 0 to FMouseOver.Count - 1 do
    TSoMouseHandlerFriend(FContainers[FMouseOver[i]]).MouseDown(Args);
  FMouseUpped := FMouseOver;
end;

procedure TSoMouseProcessor.OnItemDestroy(ASender: TObject);
begin
  FList.Delete(TSoMouseHandler(ASender));
  FContainers.Remove(TSoMouseHandler(ASender).Subject);
end;

procedure TSoMouseProcessor.PrepareTemplates;
begin
  FTemplates.Add(ByCollider, CanMouseHandleByColliderCheck);
  FTemplates.Add(ByMaxRadius, CanMouseHandleBySqrMaxRadiusCheck);
  FTemplates.Add(ByStaticRect, CanMouseHandleByStaticRectCheck);
end;

end.
