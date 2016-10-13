unit uSoEngineEvents;

interface

uses
  uCommonClasses, uSoTypes;

type
  TSoEngineEvents = class
  private
    FImage: TAnonImage;
    FOnResize: TNotifyEventList;
//    FOnMouseEnter, FOnMouseLeave: TNotifyEventList;
    FOnMouseDown, FOnMouseUp: TEventList<TMouseEventArgs>;
    FOnMouseMove: TEventList<TMouseMoveEventArgs>;
    procedure OnMouseDownHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnMouseUpHandler(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure OnMouseMoveHandler(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure OnResizeHandler(Sender: TObject);
  public
    constructor Create(const AImage: TAnonImage);
    property OnResize: TNotifyEventList read FOnResize;
    property OnMouseDown: TEventList<TMouseEventArgs> read FOnMouseDown;
    property OnMouseUp: TEventList<TMouseEventArgs> read FOnMouseUp;
    property OnMouseMove: TEventList<TMouseMoveEventArgs> read FOnMouseMove;
  end;

implementation

{ TSoEngineEvents }

constructor TSoEngineEvents.Create(const AImage: TAnonImage);
begin
  FImage := AImage;

  FOnMouseDown := TEventList<TMouseEventArgs>.Create;
  FOnMouseUp := TEventList<TMouseEventArgs>.Create;
  FOnMouseMove := TEventList<TMouseMoveEventArgs>.Create;
  FOnResize := TNotifyEventList.Create; //TEventList<TAnonImage>.Create;

  FImage.OnMouseDown := OnMouseDownHandler;
  FImage.OnMouseUp := OnMouseUpHandler;
  FImage.OnMouseMove := OnMouseMoveHandler;
  FImage.OnResize := OnResizeHandler;
end;

procedure TSoEngineEvents.OnMouseDownHandler(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FOnMouseDown.RaiseEvent(Sender, TMouseEventArgs.Create(Button, Shift, X, Y));
end;

procedure TSoEngineEvents.OnMouseMoveHandler(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
begin
  FOnMouseMove.RaiseEvent(Sender, TMouseMoveEventArgs.Create(Shift, X, Y));
end;

procedure TSoEngineEvents.OnMouseUpHandler(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FOnMouseUp.RaiseEvent(Sender, TMouseEventArgs.Create(Button, Shift, X, Y));
end;

procedure TSoEngineEvents.OnResizeHandler(Sender: TObject);
begin
  FOnResize.RaiseEvent(Sender);
end;

end.

