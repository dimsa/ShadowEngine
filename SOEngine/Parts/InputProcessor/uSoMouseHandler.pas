unit uSoMouseHandler;

interface

uses
  uSoTypes, uSoObject, uSoBasePart, uSoMouseHandleCheckers;

type
  TSoMouseHandler = class(TSoBasePart)
  private
    FEnabled: Boolean;
    FOnMouseDown: TMouseEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseUp: TMouseEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnClick: TNotifyEvent;
    FCheckMouseHandleBehavior: TCheckMouseHandleBehavior;
    procedure SetEnabled(const Value: Boolean);
    procedure SetOnClick(const Value: TNotifyEvent);
    procedure SetOnMouseDown(const Value: TMouseEvent);
    procedure SetOnMouseEnter(const Value: TNotifyEvent);
    procedure SetOnMouseLeave(const Value: TNotifyEvent);
    procedure SetOnMouseUp(const Value: TMouseEvent);
    procedure EmptyNotifyEvent(ASender: TObject);
    procedure EmptyMouseEvent(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MouseEnter;
    procedure MouseLeave;
  public
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnMouseDown: TMouseEvent read FOnMouseDown write SetOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write SetOnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write SetOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write SetOnMouseLeave;
    property OnClick: TNotifyEvent read FOnClick write SetOnClick;
    function CanExecute(const X, Y: Single): Boolean; virtual;
    constructor Create(const ASubject: TSoObject; ACheckMouseHandleBehavior: TCheckMouseHandleBehavior);
    destructor Destroy; override;
  end;

implementation

{ TEngine2DKeyboardProcessor }

function TSoMouseHandler.CanExecute(const X, Y: Single): Boolean;
begin
  Result := FCheckMouseHandleBehavior(FSubject, TPointF.Create(X, Y));
end;

constructor TSoMouseHandler.Create(const ASubject: TSoObject; ACheckMouseHandleBehavior: TCheckMouseHandleBehavior);
begin
  inherited Create(ASubject);
  FOnMouseLeave := EmptyNotifyEvent;
  FOnMouseEnter := EmptyNotifyEvent;
  FOnClick := EmptyNotifyEvent;
  FOnMouseDown := EmptyMouseEvent;
  FOnMouseUp := EmptyMouseEvent;
  FCheckMouseHandleBehavior := ACheckMouseHandleBehavior;
end;

destructor TSoMouseHandler.Destroy;
begin

  inherited;
end;

procedure TSoMouseHandler.EmptyMouseEvent(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin

end;

procedure TSoMouseHandler.EmptyNotifyEvent(ASender: TObject);
begin

end;

procedure TSoMouseHandler.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
//  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TSoMouseHandler.MouseEnter;
begin
//  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Subject);
end;

procedure TSoMouseHandler.MouseLeave;
begin
//  if Assigned(FOnMouseEnter) then
    FOnMouseLeave(Subject);
end;

procedure TSoMouseHandler.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Single);
begin
//  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TSoMouseHandler.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TSoMouseHandler.SetOnClick(const Value: TNotifyEvent);
begin
  FOnClick := Value;
end;

procedure TSoMouseHandler.SetOnMouseDown(const Value: TMouseEvent);
begin
  FOnMouseDown := Value;
end;

procedure TSoMouseHandler.SetOnMouseEnter(const Value: TNotifyEvent);
begin
  FOnMouseEnter := Value;
end;

procedure TSoMouseHandler.SetOnMouseLeave(const Value: TNotifyEvent);
begin
  FOnMouseLeave := Value;
end;

procedure TSoMouseHandler.SetOnMouseUp(const Value: TMouseEvent);
begin
  FOnMouseUp := Value;
end;

end.
