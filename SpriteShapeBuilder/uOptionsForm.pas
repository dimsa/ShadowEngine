unit uOptionsForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, System.Rtti, FMX.Grid, FMX.Layouts,
  FMX.Grid.Style, FMX.ScrollBox;

type
  TOptionsForm = class(TForm)
    CancelBtn: TButton;
    ApplyBtn: TButton;
    ElementPointsLbl: TLabel;
    Grd: TStringGrid;
    ParamName: TStringColumn;
    ParamValue: TStringColumn;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  OptionsForm: TOptionsForm;

implementation

{$R *.fmx}

end.
