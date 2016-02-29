unit uItemImagerPresenter;

interface

uses
  System.Classes, System.Types, FMX.Objects,
  uIItemView, uIItemPresenter, uSSBTypes, uItemBasePresenter;

type
  TImagerItemPresenter = class(TItemBasePresenter)
  private
    FStartDragPoint, FStartObjectPoint: TPointF;
    FCaptured: Boolean;
    function GetHeight: Integer;
    function GetImage: TImage;
    function GetPosition: TPoint;
    function GetWidth: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetImage(const Value: TImage);
    procedure SetPosition(const Value: TPoint);
    procedure SetWidth(const Value: Integer);
  public
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Position: TPoint read GetPosition write SetPosition;
    property Image: TImage read GetImage write SetImage;

    procedure Select; override;
    procedure StartDrag; override;
    procedure EndDrag; override;
    procedure Delete; override;
    procedure Capture; override;
    procedure Hover; override;
    procedure UnCapture; override;
  end;

implementation

{ TImagerItemPresenter }

procedure TImagerItemPresenter.Capture;
begin
  FCaptured := True;
  FStartObjectPoint := PointF(FView.Left, FView.Top);
  FStartDragPoint := FView.MousePos;

  if Assigned(FOnCapture) then
    FOnSelect(Self);
end;

procedure TImagerItemPresenter.Delete;
begin

end;

procedure TImagerItemPresenter.EndDrag;
begin
  if FCaptured then
  begin
    FView.Left := (FStartObjectPoint + FView.MousePos - FStartDragPoint).Round.X;
    FView.Top:= (FStartObjectPoint + FView.MousePos - FStartDragPoint).Round.Y;
  end;
end;

function TImagerItemPresenter.GetHeight: Integer;
begin
  Result := FView.Height;
end;

function TImagerItemPresenter.GetImage: TImage;
begin

end;

function TImagerItemPresenter.GetPosition: TPoint;
begin
  Result := Point(FView.Left, FView.Top);
end;

function TImagerItemPresenter.GetWidth: Integer;
begin
  Result := FView.Width;
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

procedure TImagerItemPresenter.SetHeight(const Value: Integer);
begin
  FView.Height := Value;
end;

procedure TImagerItemPresenter.SetImage(const Value: TImage);
begin

end;

procedure TImagerItemPresenter.SetPosition(const Value: TPoint);
begin
  FView.Left := Value.X;
  FView.Top := Value.Y;
end;

procedure TImagerItemPresenter.SetWidth(const Value: Integer);
begin
  FView.Width := Value;
end;

procedure TImagerItemPresenter.StartDrag;
begin

end;

procedure TImagerItemPresenter.UnCapture;
begin
  FCaptured := False;

  if Assigned(FOnUnCapture) then
    FOnSelect(Self);
end;

end.
