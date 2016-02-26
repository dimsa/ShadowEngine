unit uItemPresenterProxy;

interface

uses
  System.Classes,
  uIItemPresenter, uIPresenterEvent, uIItemView, uSSBTypes, uBaseItemPresenter;

type

TItemPresenterProxy = class(TBaseItemPresenter)
private
  FStatus: TSSBStatus;
  FItemView: IItemView;
  FPresenters: array [TSSBStatus] of IItemPresenter;
    FOnSelect: TNotifyEvent;
  procedure OnSelectHandler(ASender: TObject);
  function CreatePresenter(const AType: TSSBStatus): IItemPresenter;
public
  procedure Select; override;
  procedure StartDrag; override;
  procedure EndDrag; override;
  procedure Delete; override;
  procedure Capture; override;
  procedure Hover; override;
  procedure UnCapture; override;
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
var
  vImagerItem: TImagerItemPresenter;
begin
  if FPresenters[AType] <> nil then
    Exit;

  case AType of
    sPicture:
    begin
      vImagerItem := TImagerItemPresenter.Create(FItemView);
      vImagerItem.OnSelect := OnSelectHandler;
      FPresenters[AType] := vImagerItem;
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

