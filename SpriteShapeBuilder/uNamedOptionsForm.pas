unit uNamedOptionsForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  System.Generics.Collections,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.Layouts, FMX.Grid;

type
  TNamedOptionsForm = class(TForm)
    Grd: TStringGrid;
    ElementPointsLbl: TLabel;
    ApplyBtn: TButton;
    CancelBtn: TButton;
    ParamName: TStringColumn;
    ParamValue: TStringColumn;
    ElementNameEdt: TEdit;
    ElementNameLbl: TLabel;
    procedure CancelBtnClick(Sender: TObject);
    procedure ApplyBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FOnApply: TNotifyEvent;
    FParams: TDictionary<string, string>;
    function GetOnApply: TNotifyEvent;
    procedure SetOnApply(const Value: TNotifyEvent);
  public
    property ValuesApplied: TNotifyEvent read GetOnApply write SetOnApply;
    property Params: TDictionary<string, string> read FParams;
    procedure Show(AParams: TDictionary<string, string>);
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TNamedOptionsForm.ApplyBtnClick(Sender: TObject);
var
  i: Integer;
begin
  if Assigned(FOnApply) then
  begin
   for i := 0 to FParams.Count - 1 do
     FParams[Grd.Cells[0, i]] :=  Grd.Cells[1, i];

    FOnApply(Self);
    FParams.Clear;
  end;

  Close;
end;

procedure TNamedOptionsForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

procedure TNamedOptionsForm.FormCreate(Sender: TObject);
begin
  FParams := TDictionary<string, string>.Create;
end;

procedure TNamedOptionsForm.FormDestroy(Sender: TObject);
begin
  FParams.Free;
end;

function TNamedOptionsForm.GetOnApply: TNotifyEvent;
begin
  Result := FOnApply;
end;

procedure TNamedOptionsForm.SetOnApply(const Value: TNotifyEvent);
begin
  FOnApply := Value;
end;

procedure TNamedOptionsForm.Show(AParams: TDictionary<string, string>);
var
  vS: string;
  i: Integer;
begin
  inherited Show;
  FParams.Clear;
  for vS in AParams.Keys do
    FParams.Add(vS, AParams[vS]);

  i := 0;
  Grd.RowCount := FParams.Count;

  for vS in FParams.Keys do
  begin
    GRD.Cells[0, i] := vS;
    Grd.Cells[1, i] := FParams[vS];
    i := i + 1;
  end;

end;

end.
