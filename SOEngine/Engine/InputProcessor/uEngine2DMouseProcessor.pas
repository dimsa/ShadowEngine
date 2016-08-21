unit uEngine2DMouseProcessor;

interface

uses
  System.Classes, FMX.Types, System.UITypes,
  uBaseContainer;

type
  TEngine2DKeyboardProcessor = class
  private
    FSubject: TBaseUnitContainer;
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
  public
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnMouseDown: TMouseEvent read FOnMouseDown write SetOnMouseDown;
    property OnMouseUp: TMouseEvent read FOnMouseUp write SetOnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write SetOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write SetOnMouseLeave;
    property OnClick: TNotifyEvent read FOnClick write SetOnClick;
    constructor Create(const ASubject: TBaseUnitContainer);
    destructor Destroy; override;
  end;

implementation

{ TEngine2DKeyboardProcessor }

constructor TEngine2DKeyboardProcessor.Create(const ASubject: TBaseUnitContainer);
begin
  FSubject := ASubject;
  FOnMouseLeave := EmptyNotifyEvent;
  FOnMouseEnter := EmptyNotifyEvent;
  FOnClick := EmptyNotifyEvent;
  FOnMouseDown := EmptyMouseEvent;
  FOnMouseUp := EmptyMouseEvent;
end;

destructor TEngine2DKeyboardProcessor.Destroy;
begin
  FSubject := nil;
  inherited;
end;

procedure TEngine2DKeyboardProcessor.EmptyMouseEvent(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin

end;

procedure TEngine2DKeyboardProcessor.EmptyNotifyEvent(ASender: TObject);
begin

end;

procedure TEngine2DKeyboardProcessor.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TEngine2DKeyboardProcessor.SetOnClick(const Value: TNotifyEvent);
begin
  FOnClick := Value;
end;

procedure TEngine2DKeyboardProcessor.SetOnMouseDown(const Value: TMouseEvent);
begin
  FOnMouseDown := Value;
end;

procedure TEngine2DKeyboardProcessor.SetOnMouseEnter(const Value: TNotifyEvent);
begin
  FOnMouseEnter := Value;
end;

procedure TEngine2DKeyboardProcessor.SetOnMouseLeave(const Value: TNotifyEvent);
begin
  FOnMouseLeave := Value;
end;

procedure TEngine2DKeyboardProcessor.SetOnMouseUp(const Value: TMouseEvent);
begin
  FOnMouseUp := Value;
end;

end.
