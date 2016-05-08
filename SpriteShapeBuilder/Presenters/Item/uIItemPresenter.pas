unit uIItemPresenter;

interface

uses
  uSSBTypes;

type
  IItemPresenter = interface
    ['{DBA1F2DC-D339-4239-BF4E-CC522C6B86EF}']
    procedure Delete;
    procedure MouseDown;
    procedure ShowOptions;
    procedure SaveOptions;
    procedure MouseUp;
    procedure MouseMove;
  end;

implementation

end.
