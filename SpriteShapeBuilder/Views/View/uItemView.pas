unit uItemView;

interface

uses
  System.UITypes, System.Classes, System.Types,
  FMX.Types, FMX.Objects, FMX.Graphics, FMX.Controls, uSSBTypes,
  uIItemView, uItemImagerPresenter, uIItemPresenter,
  uEasyDevice;

type

  TItemView = class(TInterfacedObject, IItemView)
  private
    FPresenter: IItemPresenter;
    FImage: TImage;
    FParentTopLeft: TPointFunction;
    FLastMousePos: TPointF;
    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);
    function GetHeight: Integer;
    procedure SetHeight(AValue: Integer);
    function GetTop: Integer;
    procedure SetTop(AValue: Integer);
    function GetLeft: Integer;
    procedure SetLeft(AValue: Integer);
    procedure MouseLeftDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    function GetPresenter: IItemPresenter;
    procedure SetPresenter(AValue: IItemPresenter);
  public
    property Presenter: IItemPresenter read GetPresenter write SetPresenter;
    procedure ChangeCursor(const ACursor: TCursor);
    property Image: TImage read FImage write FImage;
    function MousePos: TPointF;
    procedure AssignBitmap(ABmp: TBitmap);
    constructor Create(AOwner: TControl; AParentTopLeft: TPointFunction);
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

procedure TItemView.ChangeCursor(const ACursor: TCursor);
begin
  if ACursor = FImage.Cursor then
    Exit;
  FImage.Cursor := ACursor;
end;

constructor TItemView.Create(AOwner: TControl; AParentTopLeft: TPointFunction);
begin
  FImage := TImage.Create(AOwner);
  FImage.Parent := AOwner;
  FParentTopLeft := AParentTopLeft;

  FImage.OnMouseDown := MouseLeftDown;
  FImage.OnMouseUp:= MouseUp;
  FImage.OnMouseMove := MouseMove;
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

function TItemView.GetPresenter: IItemPresenter;
begin
  Result := FPresenter;
end;

function TItemView.GetTop: Integer;
begin
  Result := Round(FImage.Position.Y);
end;

function TItemView.GetWidth: Integer;
begin
  Result := Round(FImage.Width);
end;

procedure TItemView.MouseLeftDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FLastMousePos.X := X;
  FLastMousePos.Y := Y;

  if Button = TMouseButton.mbLeft then
    FPresenter.MouseDown;
  if Button = TMouseButton.mbRight then
    FPresenter.ShowOptions;

end;

procedure TItemView.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  FLastMousePos.X := X;
  FLastMousePos.Y := Y;
  FPresenter.MouseMove;
end;

function TItemView.MousePos: TPointF;
var
  a,b,c: TPointF;
begin
  a := uEasyDevice.MousePos;
  b := FParentTopLeft;
  c := FImage.Position.Point;
  Result := uEasyDevice.MousePos - FParentTopLeft - FImage.Position.Point;
end;

procedure TItemView.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FPresenter.MouseUp;
end;

procedure TItemView.SetHeight(AValue: Integer);
begin
  FImage.Height := AValue;
end;

procedure TItemView.SetLeft(AValue: Integer);
begin
  FImage.Position.X := AValue;
end;

procedure TItemView.SetPresenter(AValue: IItemPresenter);
begin
  FPresenter := AValue;
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

