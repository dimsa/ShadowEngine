unit uIView;

interface

uses
  FMX.Objects, System.Types, FMX.Graphics, FMX.Types, System.UITypes,
  uSSBTypes, uIItemView, uMVPFrameWork;

type
  IMainView = interface(IView)
    ['{4A49079D-0CC3-4390-91DC-480467A01B3F}']
    function AddElement: IItemView;
    procedure RemoveElement(const AElement: IItemView);
    procedure SelectElement(const AElement: IItemView);
    function GetMousePos: TPoint;
    procedure ClearAndFreeImg;
    procedure SetBackground(const AImg: TImage);
    function FilenameFromDlg: string;
    procedure ChangeCursor(const ACursor: TCursor);
  end;

implementation

end.
