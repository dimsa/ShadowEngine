unit uMVPFramework;

interface

uses
  System.Classes;

type
  IView = interface

  end;

  TModel = class
  protected
    FUpdateHandler: TNotifyEvent;
    procedure EmptyHandler(ASender: TObject);
  public
    property UpdateHander: TNotifyEvent read FUpdateHandler write FUpdateHandler;
    procedure RaiseUpdateEvent;
    constructor Create(const AUpdateHandler: TNotifyEvent); overload; virtual;
    constructor Create; overload;
    destructor Destroy; override;
  end;

  TPresenter = class(TInterfacedObject, IInterface)
  protected
    FView: IView;
  public
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
