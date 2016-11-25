unit uRenditionConstructorFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, FMX.Edit, FMX.Objects, FMX.Controls.Presentation, uItemListFrame;

type
  TRenditionConstructorFrame = class(TFrame)
    ToolPanel: TPanel;
    ObjectPropertiesRect: TRectangle;
    ObjectPropertyHeader: TLabel;
    RenditionImg: TImage;
    ItemListFrame: TItemListFrame;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
