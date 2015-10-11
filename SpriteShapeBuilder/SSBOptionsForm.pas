unit SSBOptionsForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Rtti,
  FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.Layouts, FMX.Grid;

type
  TOptionsForm = class(TForm)
    ElementsPointGrd: TStringGrid;
    ElementNameEdt: TEdit;
    ElementPointsLbl: TLabel;
    ElementPathEdt: TEdit;
    ElementPathLbl: TLabel;
    ApplyBtn: TButton;
    CancelBtn: TButton;
    ElementNameLbl: TLabel;
    procedure CancelBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.fmx}

procedure TOptionsForm.CancelBtnClick(Sender: TObject);
begin
  Close;
end;

end.
