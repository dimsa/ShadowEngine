unit uItemListFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.Controls.Presentation, FMX.Edit, FMX.Objects, FMX.ListView,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Fmx.Bind.GenData, Data.Bind.Components,
  Data.Bind.ObjectScope;

type
  TItemListFrame = class(TFrame)
    SearchRect: TRectangle;
    SearchEdit: TEdit;
    SearchBtn: TSpeedButton;
    ListEditorRect: TRectangle;
    AddItemBtn: TSpeedButton;
    DelItemBtn: TSpeedButton;
    BindingsList: TBindingsList;
    PrototypeBindSource: TPrototypeBindSource;
    ListView: TListView;
    procedure AddItemBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TItemListFrame.AddItemBtnClick(Sender: TObject);
var
  vItem: TListViewItem;
begin
  with ListView.Items.Add do
  begin
   Data['Name'] := 'Имя продукта';
   Data['Bitmap'] := TBitmap.Create;
  end;
end;

end.
