unit uBaseItemPresenter;

interface

uses
  System.Classes,
  uIItemPresenter, uIPresenterEvent;

type
  TBaseItemPresenter = class abstract(TInterfacedObject, IItemPresenter, IPresenterEvent)
  protected
    FOnSelect, FOnCapture, FOnUncapture: TNotifyEvent;
    procedure SetOnSelect(AHandler: TNotifyEvent);
    function GetOnSelect: TNotifyEvent;
    procedure SetOnCapture(AHandler: TNotifyEvent);
    function GetOnCapture: TNotifyEvent;
    procedure SetOnUnCapture(AHandler: TNotifyEvent);
    function GetOnUnCapture: TNotifyEvent;
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


  end;

implementation

{ TBaseItemPresenter }

function TBaseItemPresenter.GetOnCapture: TNotifyEvent;
begin
  Result := FOnCapture;
end;

function TBaseItemPresenter.GetOnSelect: TNotifyEvent;
begin
  Result := FOnSelect;
end;

function TBaseItemPresenter.GetOnUnCapture: TNotifyEvent;
begin
  Result := FOnUnCapture;
end;

procedure TBaseItemPresenter.SetOnCapture(AHandler: TNotifyEvent);
begin
  FOnCapture := AHandler;
end;

procedure TBaseItemPresenter.SetOnSelect(AHandler: TNotifyEvent);
begin
  FOnSelect := AHandler;
end;

procedure TBaseItemPresenter.SetOnUnCapture(AHandler: TNotifyEvent);
begin
  FOnUncapture := AHandler;
end;

end.
