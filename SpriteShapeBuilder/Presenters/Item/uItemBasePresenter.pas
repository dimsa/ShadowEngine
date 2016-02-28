unit uItemBasePresenter;

interface

uses
  System.Classes,
  uIItemPresenter, uIItemPresenterEvent, uMVPFrameWork, uIItemView;

type
  TItemBasePresenter = class abstract(TPresenter, IItemPresenter, IPresenterEvent)
  protected
    FView: IItemView;
    FOnSelect, FOnCapture, FOnUncapture: TNotifyEvent;
    procedure SetOnSelect(AHandler: TNotifyEvent); virtual;
    function GetOnSelect: TNotifyEvent; virtual;
    procedure SetOnCapture(AHandler: TNotifyEvent); virtual;
    function GetOnCapture: TNotifyEvent; virtual;
    procedure SetOnUnCapture(AHandler: TNotifyEvent); virtual;
    function GetOnUnCapture: TNotifyEvent; virtual;
  public
    property OnSelect: TNotifyEvent read GetOnSelect write SetOnSelect;
    property OnCapture: TNotifyEvent read GetOnCapture write SetOnCapture;
    property OnUnCapture: TNotifyEvent read GetOnCapture write SetOnCapture;

    procedure Select; virtual; abstract;
    procedure Capture; virtual; abstract;
    procedure UnCapture; virtual; abstract;
    procedure Hover; virtual; abstract;
    procedure StartDrag; virtual; abstract;
    procedure EndDrag; virtual; abstract;
    procedure Delete; virtual; abstract;

    constructor Create(const AItemView: IItemView);
  end;

implementation

{ TBaseItemPresenter }

constructor TItemBasePresenter.Create(const AItemView: IItemView);
begin
  FView := AItemView;
end;

function TItemBasePresenter.GetOnCapture: TNotifyEvent;
begin
  Result := FOnCapture;
end;

function TItemBasePresenter.GetOnSelect: TNotifyEvent;
begin
  Result := FOnSelect;
end;

function TItemBasePresenter.GetOnUnCapture: TNotifyEvent;
begin
  Result := FOnUnCapture;
end;

procedure TItemBasePresenter.SetOnCapture(AHandler: TNotifyEvent);
begin
  FOnCapture := AHandler;
end;

procedure TItemBasePresenter.SetOnSelect(AHandler: TNotifyEvent);
begin
  FOnSelect := AHandler;
end;

procedure TItemBasePresenter.SetOnUnCapture(AHandler: TNotifyEvent);
begin
  FOnUncapture := AHandler;
end;

end.
