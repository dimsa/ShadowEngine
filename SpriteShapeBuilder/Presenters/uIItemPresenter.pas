unit uIItemPresenter;

interface

uses
  uSSBTypes;

type
  IItemPresenter = interface
    procedure Select;
    procedure StartDrag;
    procedure EndDrag;
    procedure Delete;
    function GetOnSelect: TItemSelectEvent;
    procedure SetOnSelect(AValue: TItemSelectEvent);
    property OnSelect: TItemSelectEvent read GetOnSelect write SetOnSelect;
  end;

implementation

end.
