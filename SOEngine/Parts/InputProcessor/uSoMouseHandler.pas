unit uSoMouseHandler;

interface

uses
  System.Classes, FMX.Types, System.UITypes,
  uSoContainer, uSoBasePart;

type
  TSoMouseHandler = class(TSoBasePart)
  private
    FEnabled: Boolean;
    FOnMouseDown: TMouseEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseUp: TMouseEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnClick: TNotifyEvent;
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
    constructor Create(const ASubject: TSoObject); override;
    destructor Destroy; override;
  end;

implementation

{ TEngine2DKeyboardProcessor }

constructor TSoMouseHandler.Create(const ASubject: TSoObject);
begin
  inherited Create(ASubject);
  FOnMouseLeave := EmptyNotifyEvent;
  FOnMouseEnter := EmptyNotifyEvent;
  FOnClick := EmptyNotifyEvent;
  FOnMouseDown := EmptyMouseEvent;
  FOnMouseUp := EmptyMouseEvent;
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
