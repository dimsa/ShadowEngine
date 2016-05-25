unit uNamedTableView;

interface

uses
  System.Generics.Collections,
  uMVPFrameWork, uITableView, uNamedOptionsForm, uItemBasePresenter;

type
  TTableView = class(TInterfacedObject, ITableView, IView)
  private
    FOptionsForm: TNamedOptionsForm;
    FPresenter: TItemBasePresenter;
    function GetPresenter: TItemBasePresenter;
    procedure SetPresenter(AValue: TItemBasePresenter);
    procedure OnApply(ASender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    property Presenter: TItemBasePresenter read GetPresenter write SetPresenter;
    procedure ShowParams(const AParams: TDictionary<string,string>);
    function TakeParams: TDictionary<string,string>;
  end;

implementation

{ TTableView }

constructor TTableView.Create;
begin
  FOptionsForm := TNamedOptionsForm.Create(nil);
  FOptionsForm.ValuesApplied := OnApply;
end;

destructor TTableView.Destroy;
begin
  Presenter := nil;
  FOptionsForm.Close;
  FOptionsForm.Release;
  inherited;
end;

function TTableView.GetPresenter: TItemBasePresenter;
begin
  Result := FPresenter;
end;

procedure TTableView.OnApply(ASender: TObject);
begin
  FPresenter.SaveOptions;
end;

procedure TTableView.SetPresenter(AValue: TItemBasePresenter);
begin
  FPresenter := AValue;
end;

procedure TTableView.ShowParams(const AParams: TDictionary<string, string>);
begin
  FOptionsForm.Show(AParams);
end;

function TTableView.TakeParams: TDictionary<string, string>;
begin
  Result := FOptionsForm.Params;
end;

end.
