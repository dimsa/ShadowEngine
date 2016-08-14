unit uIWorkSpaceView;

interface

uses
  FMX.Objects, System.Types, FMX.Graphics, FMX.Types, System.UITypes,
  System.Generics.Collections,
  uSSBTypes, uIItemView, uMVPFrameWork, uITableView;

type
  IWorkSpaceView = interface(IView)
    ['{4A49079D-0CC3-4390-91DC-480467A01B3F}']
    function AddElement: IItemView;
    procedure RemoveElement(const AElement: IItemView);
    procedure SelectElement(const AElement: IItemView);
    function GetMousePos: TPoint;
    function GetScale: Single;
    procedure ClearAndFreeImg;
    procedure SetBackground(const AImg: TImage);
    function AddTableView: ITableView;
    function FilenameFromDlg(out AFileName: string): boolean;
    procedure ChangeCursor(const ACursor: TCursor);
  end;

implementation

end.
