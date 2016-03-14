unit uBasePresenterIncapsulator;

interface

uses
  System.Types,
  uMVPFrameWork, uIView, uSSBModels;

type
  TBasePresenterIncapsulator = class(TPresenter)
  strict private
    FModel: TSSBModel;
    FElementStart: TRect;
    FMouseStartPoint: TPoint;
    FIsMouseDowned: Boolean;
    function GetView: IMainView;
    procedure SetIsMouseDowned(const Value: Boolean);
  protected
    procedure SetElementStart(const ARect: TRect); virtual;
    property View: IMainView read GetView;
    property ElementStart: TRect read FElementStart;
    property MouseStart: TPoint read FMouseStartPoint;
    property IsMouseDowned: Boolean read FIsMouseDowned write SetIsMouseDowned;
    property Model: TSSBModel read FModel;

    constructor Create(AView: IView; AModel: TSSBModel); virtual;
    destructor Destroy; override;
  end;

implementation

{ TBasePresenterIncapsulator }

constructor TBasePresenterIncapsulator.Create(AView: IView; AModel: TSSBModel);
begin
  FView := AView;
  FModel := AModel;
end;

destructor TBasePresenterIncapsulator.Destroy;
begin
  FModel.Free;
  inherited;;
end;

function TBasePresenterIncapsulator.GetView: IMainView;
begin
  Result := IMainView(FView);
end;

procedure TBasePresenterIncapsulator.SetElementStart(const ARect: TRect);
begin
  FElementStart := ARect;
end;

procedure TBasePresenterIncapsulator.SetIsMouseDowned(const Value: Boolean);
begin
  FIsMouseDowned := Value;
  FMouseStartPoint := View.GetMousePos;
end;

end.
