unit uIObjectListItemView;

interface

uses
  FMX.Graphics;

type
  IObjectListItemView = interface
  ['{8B0E293D-AA6F-4A5A-9768-CB8C088BE1D9}']
    procedure SetName(AValue: string);
    function GetName: string;
    procedure AssignBitmap(ABitmap: TBitmap);
  end;

implementation

end.
