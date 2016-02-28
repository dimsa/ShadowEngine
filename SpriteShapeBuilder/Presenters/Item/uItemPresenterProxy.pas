unit uItemPresenterProxy;

interface

uses
  System.Classes,
  uIItemPresenter, uIItemPresenterEvent, uIItemView, uSSBTypes, uItemBasePresenter;

type

TItemPresenterProxy = class(TItemBasePresenter)
private
  FOnSelect, FOnCapture, FOnUncapture: TNotifyEvent; // Unused from uBaseItemPresenter
  FStatus: TSSBStatus;
  FItemView: IItemView;
  FPresenters: array [TSSBStatus] of TItemBasePresenter;
  procedure OnSelectHandler(ASender: TObject);
  function CreatePresenter(const AType: TSSBStatus): IItemPresenter;
  procedure SetOnSelect(AHandler: TNotifyEvent); override;
  function GetOnSelect: TNotifyEvent; override;
public
  procedure Select; override;
  procedure StartDrag; override;
  procedure EndDrag; override;
  procedure Delete; override;
  procedure Capture; override;
  procedure Hover; override;
  procedure UnCapture; override;
  property OnSelect: TNotifyEvent read GetOnSelect write SetOnSelect;
  property Status: TSSBStatus read FStatus write FStatus;
  constructor Create(const AView: IItemView; const AStatus: TSSBStatus = sPicture);
  destructor Destroy; override;
end;

implementation

uses
  uItemImagerPresenter, uItemObjecterPresenter;
{ TItemPresenterFacade }

procedure TItemPresenterProxy.Capture;
begin
  CreatePresenter(FStatus);
  FPresenters[FStatus].Capture;
end;

constructor TItemPresenterProxy.Create(const AView: IItemView; const AStatus: TSSBStatus);
begin
  FStatus := AStatus;
  FItemView := AView;
end;

function TItemPresenterProxy.CreatePresenter(
  const AType: TSSBStatus): IItemPresenter;
var
  vImagerItem: TImagerItemPresenter;
  vObjecterItem: TObjecterItemPresenter;
begin
  if FPresenters[AType] <> nil then
    Exit;

  case AType of
    sPicture:
    begin
      vImagerItem := TImagerItemPresenter.Create(FItemView);
//      vImagerItem.OnSelect := OnSelectHandler;
      FPresenters[AType] := vImagerItem;
    end;
    sObject:
    begin
      vObjecterItem := TObjecterItemPresenter.Create(FItemView);
      FPresenters[AType] := vObjecterItem;
    end;
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

function TItemPresenterProxy.GetOnSelect: TNotifyEvent;
begin
  CreatePresenter(FStatus);
  Result := FPresenters[FStatus].OnSelect;
end;

procedure TItemPresenterProxy.Hover;
begin
  CreatePresenter(FStatus);
  FPresenters[FStatus].Hover;
end;

procedure TItemPresenterProxy.OnSelectHandler(ASender: TObject);
begin
  CreatePresenter(FStatus);
  FPresenters[FStatus].OnSelect(ASender);
end;

procedure TItemPresenterProxy.Select;
begin
  CreatePresenter(FStatus);
  FPresenters[FStatus].Select;
end;

procedure TItemPresenterProxy.SetOnSelect(AHandler: TNotifyEvent);
begin
  CreatePresenter(FStatus);
  FPresenters[FStatus].OnSelect := AHandler;
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

