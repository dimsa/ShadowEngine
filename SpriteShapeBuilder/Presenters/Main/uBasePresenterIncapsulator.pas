unit uBasePresenterIncapsulator;

interface

uses
  System.Types,
  uMVPFrameWork, uIView, uMainModel;

type
  TBasePresenterIncapsulator = class(TPresenter)
  strict private
    FModel: TSSBModel;
    FElementStart: TRect;
    FMouseStartPoint: TPoint;
    FIsMouseDowned: Boolean;
    function GetView: IWorkSpaceView;
    procedure SetIsMouseDowned(const Value: Boolean);
  protected
    procedure SetElementStart(const ARect: TRect); virtual;
    property View: IWorkSpaceView read GetView;
    property ElementStart: TRect read FElementStart;
    property MouseStart: TPoint read FMouseStartPoint;
    property IsMouseDowned: Boolean read FIsMouseDowned write SetIsMouseDowned;
    property Model: TSSBModel read FModel;

    constructor Create(AView: IWorkSpaceView; AModel: TSSBModel); virtual;
    destructor Destroy; override;
  end;

implementation

{ TBasePresenterIncapsulator }

constructor TBasePresenterIncapsulator.Create(AView: IWorkSpaceView; AModel: TSSBModel);
begin
  FView := AView;
  FModel := AModel;
end;

destructor TBasePresenterIncapsulator.Destroy;
begin
  FModel.Free;
  inherited;;
end;

function TBasePresenterIncapsulator.GetView: IWorkSpaceView;
begin
  Result := IWorkSpaceView(FView);
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
