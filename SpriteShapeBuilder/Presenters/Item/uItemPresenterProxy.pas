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
    function CreatePresenter(const AType: TSSBStatus): IItemPresenter;
 { procedure OnSelectHandler(ASender: TObject);
  procedure SetOnSelect(AHandler: TNotifyEvent); override;
  function GetOnSelect: TNotifyEvent; override;
  function GetOnCapture: TNotifyEvent;
  function GetOnUnCapture: TNotifyEvent;
  procedure SetOnCapture(const Value: TNotifyEvent);
  procedure SetOnUnCapture(const Value: TNotifyEvent);
  function GetOnHover: TNotifyEvent;
  procedure SetOnHover(const Value: TNotifyEvent); }
    procedure SetOnMouseDown(AHandler: TNotifyEvent); override;
    function GetOnMouseDown: TNotifyEvent; override;
    procedure SetOnMouseUp(AHandler: TNotifyEvent); override;
    function GetOnMouseUp: TNotifyEvent; override;
    procedure SetOnMouseMove(AHandler: TNotifyEvent); override;
    function GetOnMouseMove: TNotifyEvent; override;
    function GetInstance: TItemBasePresenter;
  public
    procedure Delete; override;
    procedure MouseDown; override;
    procedure MouseUp; override;
    procedure MouseMove; override;

    property Status: TSSBStatus read FStatus write FStatus;
    property Instance: TItemBasePresenter read GetInstance; // Give object of Status
    constructor Create(const AView: IItemView; const AStatus: TSSBStatus = sPicture);
    destructor Destroy; override;
end;

implementation

uses
  uItemImagerPresenter, uItemObjecterPresenter;

  constructor TItemPresenterProxy.Create(const AView: IItemView; const AStatus: TSSBStatus);
begin
  FStatus := AStatus;
  FItemView := AView;
end;

function TItemPresenterProxy.CreatePresenter(
  const AType: TSSBStatus): IItemPresenter;
var
  vImagerItem: TImagerItemPresenter;
  vObjecterItem: TItemObjecterPresenter;
begin
  if FPresenters[AType] <> nil then
    Exit;

  case AType of
    sPicture:
    begin
      vImagerItem := TImagerItemPresenter.Create(FItemView, FModel);
      FPresenters[AType] := vImagerItem;
    end;
    sObject:
    begin
      vObjecterItem := TItemObjecterPresenter.Create(FItemView, FModel);
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

{procedure TItemPresenterProxy.EndDrag;
begin
  if Assigned(FPresenters[FStatus]) then
    FPresenters[FStatus].EndDrag;
end;        }

{function TItemPresenterProxy.GetOnHover: TNotifyEvent;
begin
  CreatePresenter(FStatus);
  Result := FPresenters[FStatus].OnHover;
end;                 }

function TItemPresenterProxy.GetOnMouseDown: TNotifyEvent;
begin
  CreatePresenter(FStatus);
  Result := FPresenters[FStatus].OnMouseDown;
end;

function TItemPresenterProxy.GetOnMouseMove: TNotifyEvent;
begin
  CreatePresenter(FStatus);
  Result := FPresenters[FStatus].OnMouseMove;
end;

function TItemPresenterProxy.GetOnMouseUp: TNotifyEvent;
begin
  CreatePresenter(FStatus);
  Result := FPresenters[FStatus].OnMouseUp;
end;

function TItemPresenterProxy.GetInstance: TItemBasePresenter;
begin
  CreatePresenter(FStatus);
  Result := FPresenters[FStatus];
end;

{function TItemPresenterProxy.GetOnCapture: TNotifyEvent;
begin
  CreatePresenter(FStatus);
  Result := FPresenters[FStatus].OnCapture;
end;

function TItemPresenterProxy.GetOnSelect: TNotifyEvent;
begin
  CreatePresenter(FStatus);
  Result := FPresenters[FStatus].OnSelect;
end;

function TItemPresenterProxy.GetOnUnCapture: TNotifyEvent;
begin
  CreatePresenter(FStatus);
  Result := FPresenters[FStatus].OnUnCapture;
end;

procedure TItemPresenterProxy.Hover;
begin
  CreatePresenter(FStatus);
  FPresenters[FStatus].Hover;
end;          }

procedure TItemPresenterProxy.MouseDown;
begin
  inherited;
  CreatePresenter(FStatus);
  FPresenters[FStatus].MouseDown;
end;

procedure TItemPresenterProxy.MouseMove;
begin
  inherited;
  CreatePresenter(FStatus);
  FPresenters[FStatus].MouseMove;
end;

procedure TItemPresenterProxy.MouseUp;
begin
  inherited;
  CreatePresenter(FStatus);
  FPresenters[FStatus].MouseUp;
end;

{procedure TItemPresenterProxy.OnSelectHandler(ASender: TObject);
begin
  CreatePresenter(FStatus);
  FPresenters[FStatus].OnSelect(ASender);
end;

procedure TItemPresenterProxy.Select;
begin
  CreatePresenter(FStatus);
  FPresenters[FStatus].Select;
end;

procedure TItemPresenterProxy.SetOnCapture(const Value: TNotifyEvent);
begin
  CreatePresenter(FStatus);
  FPresenters[FStatus].OnCapture := Value;
end;

procedure TItemPresenterProxy.SetOnHover(const Value: TNotifyEvent);
begin
  CreatePresenter(FStatus);
  FPresenters[FStatus].OnHover := Value;
end;  }

procedure TItemPresenterProxy.SetOnMouseDown(AHandler: TNotifyEvent);
begin
  inherited;
  CreatePresenter(FStatus);
  FPresenters[FStatus].OnMouseDown := AHandler;
end;

procedure TItemPresenterProxy.SetOnMouseMove(AHandler: TNotifyEvent);
begin
  inherited;
  CreatePresenter(FStatus);
  FPresenters[FStatus].OnMouseMove := AHandler;
end;

procedure TItemPresenterProxy.SetOnMouseUp(AHandler: TNotifyEvent);
begin
  inherited;
  CreatePresenter(FStatus);
  FPresenters[FStatus].OnMouseUp := AHandler;
end;

{procedure TItemPresenterProxy.SetOnSelect(AHandler: TNotifyEvent);
begin
  CreatePresenter(FStatus);
  FPresenters[FStatus].OnSelect := AHandler;
end;

procedure TItemPresenterProxy.SetOnUnCapture(const Value: TNotifyEvent);
begin
  CreatePresenter(FStatus);
  FPresenters[FStatus].OnUnCapture := Value;
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
end;     }

end.

