unit uItemImagerPresenter;

interface

uses
  System.Classes, System.Types, FMX.Objects,
  uIItemView, uIItemPresenter, uSSBTypes, uItemBasePresenter, uSSBModels;

type
  TItemImagerPresenter = class(TItemBasePresenter)
  private
    FItemImageModel: TItemImageModel;
    function GetHeight: Integer;
    function GetImage: TImage;
    function GetPosition: TPoint;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetImage(const Value: TImage);
    procedure SetPosition(const Value: TPoint);
    procedure SetWidth(const Value: Integer);
    procedure OnUpdateModel(ASender: TObject);
  public
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Position: TPoint read GetPosition write SetPosition;
    property Image: TImage read GetImage write SetImage;

    procedure Delete; override;
    procedure MouseDown; override;
    procedure MouseUp; override;
    procedure MouseMove; override;
    constructor Create(const AItemView: IItemView; const AItemImageModel: TItemImageModel);
  end;

implementation

{ TImagerItemPresenter }

constructor TItemImagerPresenter.Create(const AItemView: IItemView;
  const AItemImageModel: TItemImageModel);
begin
  inherited Create(AItemView);
  FItemImageModel := AItemImageModel;
  FItemImageModel.UpdateHander := OnModelUpdate;
end;

procedure TItemImagerPresenter.Delete;
begin

end;

function TItemImagerPresenter.GetHeight: Integer;
begin
  Result := FView.Height;
end;

function TItemImagerPresenter.GetImage: TImage;
begin

end;

function TItemImagerPresenter.GetPosition: TPoint;
begin
  Result := Point(FView.Left, FView.Top);
end;

function TItemImagerPresenter.GetWidth: Integer;
begin
  Result := FView.Width;
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

end;

procedure TItemImagerPresenter.SetHeight(const Value: Integer);
begin
  FView.Height := Value;
end;

procedure TItemImagerPresenter.SetImage(const Value: TImage);
begin

end;

procedure TItemImagerPresenter.SetPosition(const Value: TPoint);
begin
  FView.Left := Value.X;
  FView.Top := Value.Y;
end;

procedure TItemImagerPresenter.SetWidth(const Value: Integer);
begin
  FView.Width := Value;
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
