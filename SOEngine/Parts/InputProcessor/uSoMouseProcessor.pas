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
    procedure ExecuteMouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single); // Process mouse on tick
    procedure ExecuteMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single); // Process mouse on tick
    procedure ExecuteMouseMove(X, Y: Single); // Process mouse position on tick
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
  inherited;
end;

procedure TSoMouseProcessor.ExecuteMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  i: Integer;
begin
  for i := 0 to FMouseOver.Count - 1 do
  begin
  //  if TSoMouseHandlerFriend(FContainers[FMouseOver[i]]).CanExecute(X, Y) then
      TSoMouseHandlerFriend(FContainers[FMouseOver[i]]).MouseDown(Button, Shift, X, Y);
  end;
  FMouseDowned := FMouseOver;
end;

procedure TSoMouseProcessor.ExecuteMouseMove(X, Y: Single);
var
  i, j: Integer;
  vWas: Boolean;
  vItem: TSoObject;
begin
//  FMouseOver := FCollider.Contains(X, Y);
  FMouseOver.Clear;
  for i := 0 to FList.Count - 1 do
  begin
    if FList[i].CanExecute(X - FList[i].Subject.X ,Y - FList[i].Subject.Y) then
      FMouseOver.Add(FList[i].Subject)
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
      TSoMouseHandlerFriend(FContainers[FMouseOver[i]]).MouseLeave;
  end;

  for j := 0 to FOldMouseOver.Count - 1 do
    if FOldMouseOver[j] <> nil then
      TSoMouseHandlerFriend(FContainers[FOldMouseOver[j]]).MouseEnter;

  FOldMouseOver.Clear;
  for i := 0 to FMouseOver.Count - 1 do
    FOldMouseOver.Add(FMouseOver[i]);
end;

procedure TSoMouseProcessor.ExecuteMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var
  i: Integer;
begin
  for i := 0 to FMouseOver.Count - 1 do
    TSoMouseHandlerFriend(FContainers[FMouseOver[i]]).MouseDown(Button, Shift, X, Y);
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
