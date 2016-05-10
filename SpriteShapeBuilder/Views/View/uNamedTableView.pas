unit uNamedTableView;

interface

uses
  System.Generics.Collections,
  uMVPFrameWork, uITableView, uNamedOptionsForm, uIItemPresenter;

type
  TTableView = class(TInterfacedObject, ITableView, IView)
  private
    FOptionsForm: TNamedOptionsForm;
    FPresenter: IItemPresenter;
    function GetPresenter: IItemPresenter;
    procedure SetPresenter(AValue: IItemPresenter);
    procedure OnApply(ASender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    property Presenter: IItemPresenter read GetPresenter write SetPresenter;
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
  FOptionsForm.Free;
  inherited;
end;

function TTableView.GetPresenter: IItemPresenter;
begin
  Result := FPresenter;
end;

procedure TTableView.OnApply(ASender: TObject);
begin
  FPresenter.SaveOptions;
end;

procedure TTableView.SetPresenter(AValue: IItemPresenter);
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
