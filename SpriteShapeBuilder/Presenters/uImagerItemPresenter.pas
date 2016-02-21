unit uImagerItemPresenter;

interface

uses
  System.Classes, System.Types, FMX.Objects,
  uIItemView, uIItemPresenter, uSSBTypes;

type
  TImagerItemPresenter = class(TInterfacedObject, IItemPresenter)
  private
    FOnSelect: TItemSelectEvent;

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

    constructor Create(const AItemView: IItemView);
  end;

implementation

{procedure TSSBImagerPresenter.DoMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  SelImg;
end;

procedure TSSBImagerPresenter.DoMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Single);
begin
  StartDragImg;
end;

procedure TSSBImagerPresenter.DoMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  FinishDragImg;
end;}

{ TImagerItemPresenter }

constructor TImagerItemPresenter.Create(const AItemView: IItemView);
begin
  FView := AItemView;
end;

procedure TImagerItemPresenter.Delete;
begin

end;

procedure TImagerItemPresenter.EndDrag;
begin

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

end.
