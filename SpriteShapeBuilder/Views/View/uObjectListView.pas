unit uObjectListView;

interface

uses
  FMX.Forms,
  uIObjectListItemView, uIObjectListView;

type
  TObjectListView = class(TInterfacedObject, IObjectListView)
  private
    FFrame: TFrame;
  public
    function GetSelectedItem: IObjectListItemView;
    function RemSelectedItem: IObjectListItemView;
    function AddItem: IObjectListItemView;
    function Items: IEnumerable<IObjectListItemView>;
    constructor Create(const AFrame: TFrame);
  end;

implementation

{ TObjectListView }

function TObjectListView.AddItem: IObjectListItemView;
begin

end;

constructor TObjectListView.Create(const AFrame: TFrame);
begin

end;

function TObjectListView.GetSelectedItem: IObjectListItemView;
begin

end;

function TObjectListView.Items: IEnumerable<IObjectListItemView>;
begin

end;

function TObjectListView.RemSelectedItem: IObjectListItemView;
begin

end;

end.
