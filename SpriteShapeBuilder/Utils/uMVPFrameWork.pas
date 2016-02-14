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
  public
    constructor Create(const AUpdateHandler: TNotifyEvent); virtual;
    destructor Destroy; override;
  end;

  TPresenter = class
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

destructor TModel.Destroy;
begin
  FUpdateHandler := nil;
  inherited;
end;

end.
