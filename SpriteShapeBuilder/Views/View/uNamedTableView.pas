unit uNamedTableView;

interface

uses
  System.Generics.Collections, uClasses,
  uMVPFrameWork, uITableView, uNamedOptionsForm;

type
  TTableView = class(TInterfacedObject, ITableView, IView)
  private
    FOptionsForm: TNamedOptionsForm;
    FOnTakeParams: TEvent<TDictionary<string,string>>;
    procedure OnApply(ASender: TObject);
    procedure SetOnTakeParams(const Value: TEvent<TDictionary<string,string>>);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ShowParams(const AParams: TDictionary<string,string>);
    property OnTakeParams: TEvent<TDictionary<string,string>> read FOnTakeParams write SetOnTakeParams;
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
  FOptionsForm.Close;
  FOptionsForm.Release;
  inherited;
end;

procedure TTableView.OnApply(ASender: TObject);
begin
  if Assigned(FOnTakeParams) then
    FOnTakeParams(Self, FOptionsForm.Params);
end;

procedure TTableView.SetOnTakeParams(const Value: TEvent<TDictionary<string,string>>);
begin
  FOnTakeParams := Value;
end;

procedure TTableView.ShowParams(const AParams: TDictionary<string, string>);
begin
  FOptionsForm.Show(AParams);
end;

end.
