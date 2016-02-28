unit uIItemPresenter;

interface

uses
  uSSBTypes;

type
  IItemPresenter = interface
    procedure Select;
    procedure Capture;
    procedure UnCapture;
    procedure Hover;
    procedure StartDrag;
    procedure EndDrag;
    procedure Delete;
  end;

implementation

end.
