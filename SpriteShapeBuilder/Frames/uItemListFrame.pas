unit uItemListFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.Controls.Presentation, FMX.Edit, FMX.Objects, FMX.ListView;

type
  TItemListFrame = class(TFrame)
    ObjectList: TListView;
    SearchRect: TRectangle;
    SearchEdit: TEdit;
    SearchBtn: TSpeedButton;
    ListEditorRect: TRectangle;
    AddItemBtn: TSpeedButton;
    DelItemBtn: TSpeedButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
