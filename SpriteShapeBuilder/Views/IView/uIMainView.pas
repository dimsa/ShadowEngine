unit uIMainView;

interface

uses
  System.Types,
  uSSBTypes;

type
  IMainView = interface
    function GetStatus: TSSBStatus;
    procedure SetStatus(const AStatus: TSSBStatus);
    function FilenameFromDlg(out AFileName: string): boolean;
    function ClientToScreenPoint(const APoint: TPoint): TPoint;
  end;

implementation

end.
