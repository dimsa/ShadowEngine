unit uItemPresenterProxy;

interface

uses
  System.Classes,
  uIItemPresenter, uIItemView, uSSBTypes;

type

TItemPresenterProxy = class(TInterfacedObject, IItemPresenter)
private
  FStatus: TSSBStatus;
  FItemView: IItemView;
  FPresenters: array [TSSBStatus] of IItemPresenter;
    FOnSelect: TNotifyEvent;
  procedure OnSelectHandler(ASender: TObject);
  function CreatePresenter(const AType: TSSBStatus): IItemPresenter;
  function GetOnSelect: TItemSelectEvent;
  procedure SetOnSelect(AValue: TItemSelectEvent);
public
  procedure Select;
  procedure StartDrag;
  procedure EndDrag;
  procedure Delete;
  procedure Capture;
  procedure Hover;
  procedure UnCapture;
  property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  property Status: TSSBStatus read FStatus write FStatus;
  constructor Create(const AView: IItemView);
  destructor Destroy; override;
end;

implementation

uses
  uImagerItemPresenter;
{ TItemPresenterFacade }

procedure TItemPresenterProxy.Capture;
begin
  CreatePresenter(FStatus);
  FPresenters[FStatus].Capture;
end;

constructor TItemPresenterProxy.Create(const AView: IItemView);
begin
  FItemView := AView;
end;

function TItemPresenterProxy.CreatePresenter(
  const AType: TSSBStatus): IItemPresenter;
begin
  if FPresenters[AType] <> nil then
    Exit;

  case AType of
    sPicture:
    begin
      FPresenters[AType] := TImagerItemPresenter.Create(FItemView);
      FPresenters[AType].OnSelect := OnSelectHandler;
    end;
    sObject: ;
    sShape: ;
    end;
end;

procedure TItemPresenterProxy.Delete;
begin
  if Assigned(FPresenters[FStatus]) then
    FPresenters[FStatus].Delete;
end;

destructor TItemPresenterProxy.Destroy;
var
  i: TSSBStatus;
begin
  for i := Low(TSSBStatus) to High(TSSBStatus) do
    FPresenters[i] := nil;
  inherited;
end;

procedure TItemPresenterProxy.EndDrag;
begin
  if Assigned(FPresenters[FStatus]) then
    FPresenters[FStatus].EndDrag;
end;

function TItemPresenterProxy.GetOnSelect: TItemSelectEvent;
begin
  Result := FOnSelect;
end;

procedure TItemPresenterProxy.Hover;
begin
  CreatePresenter(FStatus);
  FPresenters[FStatus].Hover;
end;

procedure TItemPresenterProxy.OnSelectHandler(ASender: TObject);
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self);
//    FOnSelect(ASender); ћб сделать типа RoutedEvent
end;

procedure TItemPresenterProxy.Select;
begin
  CreatePresenter(FStatus);
  FPresenters[FStatus].Select;
end;

procedure TItemPresenterProxy.SetOnSelect(AValue: TItemSelectEvent);
begin
  FOnSelect := AValue;
end;

procedure TItemPresenterProxy.StartDrag;
begin
  if Assigned(FPresenters[FStatus]) then
    FPresenters[FStatus].StartDrag;
end;

procedure TItemPresenterProxy.UnCapture;
begin
  CreatePresenter(FStatus);
  FPresenters[FStatus].UnCapture;
end;

end.

