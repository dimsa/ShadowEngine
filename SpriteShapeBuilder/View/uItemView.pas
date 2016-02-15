unit uItemView;

interface

uses
  System.UITypes, System.Classes,
  FMX.Types, FMX.Objects, FMX.Graphics, FMX.Controls,
  uIItemView, uImagerItemPresenter, uItemPresenterFacade;

type

  TItemView = class(TInterfacedObject, IItemView)
  private
    FPresenter: TItemPresenterFacade;
    FImage: TImage;
    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);
    function GetHeight: Integer;
    procedure SetHeight(AValue: Integer);
    function GetTop: Integer;
    procedure SetTop(AValue: Integer);
    function GetLeft: Integer;
    procedure SetLeft(AValue: Integer);
    procedure MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Single);
    procedure MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  public
    procedure AssignBitmap(ABmp: TBitmap);
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

  FPresenter := TItemPresenterFacade.Create;

  FImage.OnMouseDown := MouseDown;
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

function TItemView.GetTop: Integer;
begin
  Result := Round(FImage.Position.Y);
end;

function TItemView.GetWidth: Integer;
begin
  Result := Round(FImage.Width);
end;

procedure TItemView.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FPresenter.Select;
end;

procedure TItemView.MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
begin
  FPresenter.StartDrag;
end;

procedure TItemView.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FPresenter.EndDrag;
end;

procedure TItemView.SetHeight(AValue: Integer);
begin
  FImage.Height := AValue;
end;

procedure TItemView.SetLeft(AValue: Integer);
begin
  FImage.Position.X := AValue;
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

