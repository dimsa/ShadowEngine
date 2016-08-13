unit uItemImagerPresenter;

interface

uses
  System.Classes, System.Types, FMX.Objects, System.SysUtils, uClasses,
  System.Generics.Collections,
  uITableView,
  uIItemView, uIItemPresenter, uSSBTypes, uItemBasePresenter, uSSBModels;

type
  TItemImagerPresenter = class(TItemBasePresenter)
  private
    FItemImageModel: TItemImageModel;
    FParams: TDictionary<string, string>;
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
    function GetParams: TDictionary<string, string>;
    procedure SetParams(const AValue: TDictionary<string, string>); override;
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

    constructor Create(const AItemView: IItemView; const AItemImageModel: TItemImageModel); reintroduce;
    destructor Destroy; override;
  end;

implementation

{ TImagerItemPresenter }

constructor TItemImagerPresenter.Create(const AItemView: IItemView;
  const AItemImageModel: TItemImageModel);
begin
  inherited Create(AItemView);
  FParams := TDictionary<string, string>.Create;
  FItemImageModel := AItemImageModel;
  FItemImageModel.UpdateHander := OnUpdateModel;
end;

procedure TItemImagerPresenter.Delete;
begin

end;

destructor TItemImagerPresenter.Destroy;
begin
  FParams.Free;
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

function TItemImagerPresenter.GetParams: TDictionary<string, string>;
begin
  with FParams do
  begin
    Clear;
    Add('X', IntToStr(Model.Position.X));
    Add('Y', IntToStr(Model.Position.Y));
    Add('Width', IntToStr(Model.Width));
    Add('Height', IntToStr(Model.Height));
  end;
  Result := FParams;
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
  with FView do
  begin
    AssignBitmap(FItemImageModel.OriginalImage.Bitmap);
    Width := FItemImageModel.Width;
    Height := FItemImageModel.Height;
    Left := FItemImageModel.Position.X;
    Top := FItemImageModel.Position.Y;
  end;
end;

{procedure TItemImagerPresenter.SaveOptions;
begin
  inherited;

  if Assigned(FOnOptionsSave) then
    FOnOptionsSave(Self);

  SetParams(FTableView.TakeParams);
end;  }

procedure TItemImagerPresenter.SetHeight(const Value: Integer);
begin
  FItemImageModel.Height := Value;
end;

procedure TItemImagerPresenter.SetImage(const Value: TImage);
begin
  FItemImageModel.OriginalImage := Value;
end;

procedure TItemImagerPresenter.SetParams(
  const AValue: TDictionary<string, string>);
var
  vErr, vA: Integer;
begin
  Model.Position:= Point(ToInt(AValue['X']), ToInt(AValue['Y']));
  Model.Width := ToInt(AValue['Width']);
  Model.Height := ToInt(AValue['Height']);
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
  if Assigned(FOnOptionsShow) then
    FOnOptionsShow(Self);

  if Assigned(FTableView) then
    FTableView.ShowParams(GetParams);
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
