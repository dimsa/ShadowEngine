unit uItemView;

interface

uses
  FMX.Types, FMX.Objects, FMX.Graphics, FMX.Controls,
  uIItemView;

type

  TItemView = class(TInterfacedObject, IItemView)
  private
    FImage: TImage;
    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);
    function GetHeight: Integer;
    procedure SetHeight(AValue: Integer);
    function GetTop: Integer;
    procedure SetTop(AValue: Integer);
    function GetLeft: Integer;
    procedure SetLeft(AValue: Integer);
    function GetOnMouseDown: TMouseEvent;
    function GetOnMouseMove: TMouseMoveEvent;
    function GetOnMouseUp: TMouseEvent;
    procedure SetOnMouseDown(const Value: TMouseEvent);
    procedure SetOnMouseMove(const Value: TMouseMoveEvent);
    procedure SetOnMouseUp(const Value: TMouseEvent);
  public
    procedure AssignBitmap(ABmp: TBitmap);
    property OnMouseMove: TMouseMoveEvent read GetOnMouseMove write SetOnMouseMove;
    property OnMouseDown: TMouseEvent read GetOnMouseDown write SetOnMouseDown;
    property OnMouseUp: TMouseEvent read GetOnMouseUp write SetOnMouseUp;
    constructor Create(AOwner: TControl);
    destructor Destroy; override;
  end;

implementation
{ TItem }

procedure TItemView.AssignBitmap(ABmp: TBitmap);
begin
  FImage.Width := ABmp.Width;
  FImage.Height:= ABmp.Height;
  FImage.Bitmap.Assign(ABmp);
end;

constructor TItemView.Create(AOwner: TControl);
begin
  FImage := TImage.Create(AOwner);
  FImage.Parent := AOwner;
end;

destructor TItemView.Destroy;
begin
  FImage.Free;
end;

function TItemView.GetHeight: Integer;
begin
  Result := Round(FImage.Height);
end;

function TItemView.GetLeft: Integer;
begin
  Result := Round(FImage.Position.X);
end;

function TItemView.GetOnMouseDown: TMouseEvent;
begin
  Result := FImage.OnMouseDown;
end;

function TItemView.GetOnMouseMove: TMouseMoveEvent;
begin
  Result := FImage.OnMouseMove;
end;

function TItemView.GetOnMouseUp: TMouseEvent;
begin
  Result := FImage.OnMouseUp;
end;

function TItemView.GetTop: Integer;
begin
  Result := Round(FImage.Position.Y);
end;

function TItemView.GetWidth: Integer;
begin
  Result := Round(FImage.Width);
end;

procedure TItemView.SetHeight(AValue: Integer);
begin
  FImage.Height := AValue;
end;

procedure TItemView.SetLeft(AValue: Integer);
begin
  FImage.Position.X := AValue;
end;

procedure TItemView.SetOnMouseDown(const Value: TMouseEvent);
begin
  FImage.OnMouseDown := Value;
end;

procedure TItemView.SetOnMouseMove(const Value: TMouseMoveEvent);
begin
  FImage.OnMouseMove := Value;
end;

procedure TItemView.SetOnMouseUp(const Value: TMouseEvent);
begin
  FImage.OnMouseUp := Value;
end;

procedure TItemView.SetTop(AValue: Integer);
begin
  FImage.Position.Y := AValue;
end;

procedure TItemView.SetWidth(AValue: Integer);
begin
  FImage.Width := AValue;
end;

end.
