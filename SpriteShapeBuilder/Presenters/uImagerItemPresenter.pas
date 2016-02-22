unit uImagerItemPresenter;

interface

uses
  System.Classes, System.Types, FMX.Objects,
  uIItemView, uIItemPresenter, uSSBTypes;

type
  TImagerItemPresenter = class(TInterfacedObject, IItemPresenter)
  private
    FOnSelect: TItemSelectEvent;
    FStartDragPoint, FStartObjectPoint: TPointF;
    FCaptured: Boolean;
    FView: IItemView;
    function GetHeight: Integer;
    function GetImage: TImage;
    function GetPosition: TPoint;
    function GetWidth: Integer;
    procedure SetHeigt(const Value: Integer);
    procedure SetImage(const Value: TImage);
    procedure SetPosition(const Value: TPoint);
    procedure SetWidth(const Value: Integer);
    function GetOnSelect: TItemSelectEvent;
    procedure SetOnSelect(AValue: TItemSelectEvent);
  public
    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeigt;
    property Position: TPoint read GetPosition write SetPosition;
    property Image: TImage read GetImage write SetImage;
    property OnSelect: TItemSelectEvent read FOnSelect write FOnSelect;

    procedure Select;
    procedure StartDrag;
    procedure EndDrag;
    procedure Delete;
    procedure Capture;
    procedure Hover;
    procedure UnCapture;

    constructor Create(const AItemView: IItemView);
  end;

implementation

{ TImagerItemPresenter }

procedure TImagerItemPresenter.Capture;
begin
  FCaptured := True;
  FStartObjectPoint := PointF(FView.Left, FView.Top);
  FStartDragPoint := FView.MousePos;
end;

constructor TImagerItemPresenter.Create(const AItemView: IItemView);
begin
  FView := AItemView;
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

end;

function TImagerItemPresenter.GetImage: TImage;
begin

end;

function TImagerItemPresenter.GetOnSelect: TItemSelectEvent;
begin
  Result := FOnSelect;
end;

function TImagerItemPresenter.GetPosition: TPoint;
begin

end;

function TImagerItemPresenter.GetWidth: Integer;
begin

end;

procedure TImagerItemPresenter.Hover;
begin
  if FCaptured then
  begin
    FView.Left := (FStartObjectPoint + FView.MousePos - FStartDragPoint).Round.X;
    FView.Top:= (FStartObjectPoint + FView.MousePos - FStartDragPoint).Round.Y;
  end;
end;

procedure TImagerItemPresenter.Select;
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self);
end;

procedure TImagerItemPresenter.SetHeigt(const Value: Integer);
begin

end;

procedure TImagerItemPresenter.SetImage(const Value: TImage);
begin

end;

procedure TImagerItemPresenter.SetOnSelect(AValue: TItemSelectEvent);
begin
  FOnSelect := AValue;
end;

procedure TImagerItemPresenter.SetPosition(const Value: TPoint);
begin

end;

procedure TImagerItemPresenter.SetWidth(const Value: Integer);
begin

end;

procedure TImagerItemPresenter.StartDrag;
begin

end;

procedure TImagerItemPresenter.UnCapture;
begin
  FCaptured := False;
end;

end.
