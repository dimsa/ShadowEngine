unit uMVPFramework;

interface

uses
  System.Classes;

type
  IView = interface

  end;

  TModel = class(TInterfacedObject)
  protected
    FUpdateHandler: TNotifyEvent;
    procedure EmptyHandler(ASender: TObject);
  public
    property UpdateHander: TNotifyEvent read FUpdateHandler write FUpdateHandler;
    procedure RaiseUpdateEvent;
    constructor Create; overload; virtual;
    constructor Create(const AUpdateHandler: TNotifyEvent); overload; virtual;
    destructor Destroy; override;
  end;

  TPresenter = class(TInterfacedObject)
  protected
    FView: IView;
  public
    procedure OnModelUpdate(ASender: TObject); virtual;
    constructor Create(const AView: IView); virtual;
    destructor Destroy; override;
  end;

implementation

{ TPresenter }

constructor TPresenter.Create(const AView: IView);
begin
  FView := AView;
end;

destructor TPresenter.Destroy;
begin
  FView := nil;
  inherited;
end;

procedure TPresenter.OnModelUpdate(ASender: TObject);
begin

end;

{ IModel }

constructor TModel.Create(const AUpdateHandler: TNotifyEvent);
begin
  FUpdateHandler := AUpdateHandler;
end;

constructor TModel.Create;
begin
  FUpdateHandler := EmptyHandler;
end;

destructor TModel.Destroy;
begin
  FUpdateHandler := nil;
  inherited;
end;

procedure TModel.EmptyHandler(ASender: TObject);
begin

end;

procedure TModel.RaiseUpdateEvent;
begin
  if Assigned(FUpdateHandler) then
    FUpdateHandler(Self);
end;

end.
