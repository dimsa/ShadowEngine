unit uWorldManager;

interface

uses
  uSoTypes, uClasses, uEngine2DClasses, uSoRenderer, uCommonClasses, uSoObject, uSoEngineEvents;

type
  TWorldManager = class
  private
    FRenderer: TSoRenderer;
    FOnResize: TNotifyEventList;
    FOnMouseDown, FOnMouseUp: TEventList<TMouseEventArgs>;
    FOnMouseMove: TEventList<TMouseMoveEventArgs>;
    procedure SetOnPaintBackground(const Value: TEvent<TAnonImage>);
    procedure SetOnBeginPaint(const Value: TEvent<TAnonImage>);
    procedure SetOnEndPaint(const Value: TEvent<TAnonImage>);
  public
    constructor Create(const ARenderer: TSoRenderer; const AEvents: TSoEngineEvents);
    property OnPaintBackground: TEvent<TAnonImage> write SetOnPaintBackground; // Procedure to Paint Background. It can be default or Parallax(like in Asteroids example) or any type you want
    property OnBeginPaint: TEvent<TAnonImage> write SetOnBeginPaint; // Method is called before Paint
    property OnEndPaint: TEvent<TAnonImage> write SetOnEndPaint; // Method is called after Paint
    property OnResize: TNotifyEventList read FOnResize;
    property OnMouseDown: TEventList<TMouseEventArgs> read FOnMouseDown;
    property OnMouseUp: TEventList<TMouseEventArgs> read FOnMouseUp;
    property OnMouseMove: TEventList<TMouseMoveEventArgs> read FOnMouseMove;
  end;

implementation

{ TWorldManager }

constructor TWorldManager.Create(const ARenderer: TSoRenderer; const AEvents: TSoEngineEvents);
begin
  FRenderer := ARenderer;
  FOnResize := AEvents.OnResize;
  FOnMouseDown := AEvents.OnMouseDown;
  FOnMouseUp:= AEvents.OnMouseUp;
  FOnMouseMove := AEvents.OnMouseMove;
end;

procedure TWorldManager.SetOnBeginPaint(const Value: TEvent<TAnonImage>);
begin
  FRenderer.OnBeginPaint := Value;
end;

procedure TWorldManager.SetOnEndPaint(const Value: TEvent<TAnonImage>);
begin
  FRenderer.OnEndPaint := Value;
end;

procedure TWorldManager.SetOnPaintBackground(const Value: TEvent<TAnonImage>);
begin
  FRenderer.OnPaintBackground := Value;
end;

end.

