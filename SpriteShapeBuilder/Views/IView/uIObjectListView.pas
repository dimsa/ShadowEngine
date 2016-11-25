unit uIObjectListView;

interface

uses
  uIObjectListItemView, System.Generics.Collections;

type
  IObjectListView = interface
  ['{C86FB703-7B9D-4889-99FC-8A1CBA7F4C49}']
    function GetSelectedItem: IObjectListItemView;
    function RemSelectedItem: IObjectListItemView;
    function AddItem: IObjectListItemView;
    function Items: IEnumerable<IObjectListItemView>;
  end;

implementation

end.
