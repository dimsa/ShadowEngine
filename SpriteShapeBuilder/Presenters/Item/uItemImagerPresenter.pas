unit uItemImagerPresenter;

interface

uses
  System.Classes, System.Types, FMX.Objects, System.SysUtils, uClasses,
  uITableView,
  uIItemView, uIItemPresenter, uSSBTypes, uItemBasePresenter, uSSBModels;

type
  TItemImagerPresenter = class(TItemBasePresenter)
  private
    FItemImageModel: TItemImageModel;
    FTableView: ITableView;
    function GetHeight: Integer;
    function GetImage: TImage;
    function GetPosition: TPoint;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetImage(const Value: TImage);
    procedure SetPosition(const Value: TPoint);
    procedure SetWidth(const Value: Integer);
    procedure OnUpdateModel(ASender: TObject);
    function GetRect: TRectF; override;
    procedure SetRect(const Value: TRectF); override;
  protected
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Position: TPoint read GetPosition write SetPosition;
    property Rect: TRectF read GetRect write SetRect;
    property Image: TImage read GetImage write SetImage;
    property Model: TItemImageModel read FItemImageModel;
  public
    procedure Delete; override;
    procedure MouseDown; override;
    procedure MouseUp; override;
    procedure MouseMove; override;
    procedure ShowOptions; override;

    constructor Create(const AItemView: IItemView; const ATableView: ITableView; const AItemImageModel: TItemImageModel); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TImagerItemPresenter }

constructor TItemImagerPresenter.Create(const AItemView: IItemView; const ATableView: ITableView;
  const AItemImageModel: TItemImageModel);
begin
  inherited Create(AItemView);
  FTableView := ATableView;
  FItemImageModel := AItemImageModel;
  FItemImageModel.UpdateHander := OnUpdateModel;
end;

procedure TItemImagerPresenter.Delete;
begin

end;

destructor TItemImagerPresenter.Destroy;
begin

  inherited;
end;

function TItemImagerPresenter.GetHeight: Integer;
begin
  Result := FItemImageModel.Height;
end;

function TItemImagerPresenter.GetImage: TImage;
begin
  Result := FItemImageModel.OriginalImage;
end;

function TItemImagerPresenter.GetPosition: TPoint;
begin
  Result := FItemImageModel.Position;
end;

function TItemImagerPresenter.GetRect: TRectF;
begin
  Result := System.Types.RectF(
    FItemImageModel.Position.X,
    FItemImageModel.Position.Y,
    FItemImageModel.Position.X + FItemImageModel.Width,
    FItemImageModel.Position.Y + FItemImageModel.Height);
end;

function TItemImagerPresenter.GetWidth: Integer;
begin
  Result := FItemImageModel.Width;
end;

procedure TItemImagerPresenter.MouseDown;
begin
  inherited;
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self);
end;

procedure TItemImagerPresenter.MouseMove;
begin
  inherited;
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self);
end;

procedure TItemImagerPresenter.MouseUp;
begin
  inherited;
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self)
end;

procedure TItemImagerPresenter.OnUpdateModel(ASender: TObject);
begin
  FView.AssignBitmap(FItemImageModel.OriginalImage.Bitmap);
  FView.Width := FItemImageModel.Width;
  FView.Height := FItemImageModel.Height;
  FView.Left := FItemImageModel.Position.X;
  FView.Top := FItemImageModel.Position.Y;
end;

procedure TItemImagerPresenter.SetHeight(const Value: Integer);
begin
  FItemImageModel.Height := Value;
end;

procedure TItemImagerPresenter.SetImage(const Value: TImage);
begin
  FItemImageModel.OriginalImage := Value;
end;

procedure TItemImagerPresenter.SetPosition(const Value: TPoint);
begin
  FItemImageModel.Position := Value;
end;

procedure TItemImagerPresenter.SetRect(const Value: TRectF);
begin
    Position := Value.TopLeft.Round;
    Width := Round(Value.Width);
    Height := Round(Value.Height);
end;

procedure TItemImagerPresenter.SetWidth(const Value: Integer);
begin
  FItemImageModel.Width := Value;
end;

procedure TItemImagerPresenter.ShowOptions;
begin
  inherited;
  if Assigned(FOnOptionsShow) then
    FOnOptionsShow(Self)
end;

{procedure TImagerItemPresenter.Capture;
begin
  FCaptured := True;
  FStartObjectPoint := PointF(FView.Left, FView.Top);
  FStartDragPoint := FView.MousePos;

  if Assigned(FOnCapture) then
    FOnSelect(Self);
end;

procedure TImagerItemPresenter.EndDrag;
begin
  if FCaptured then
  begin
    FView.Left := (FStartObjectPoint + FView.MousePos - FStartDragPoint).Round.X;
    FView.Top:= (FStartObjectPoint + FView.MousePos - FStartDragPoint).Round.Y;
  end;
end;

procedure TImagerItemPresenter.Hover;
begin
  if FCaptured then
  begin
    FView.Left := (FStartObjectPoint + FView.MousePos - FStartDragPoint).Round.X;
    FView.Top:= (FStartObjectPoint + FView.MousePos - FStartDragPoint).Round.Y;
  end;

  if Assigned(FOnHover) then
    FOnHover(Self);
end;

procedure TImagerItemPresenter.Select;
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;

procedure TImagerItemPresenter.StartDrag;
begin

end;

procedure TImagerItemPresenter.UnCapture;
begin
  FCaptured := False;

  if Assigned(FOnUnCapture) then
    FOnSelect(Self);
end;                 }

end.
