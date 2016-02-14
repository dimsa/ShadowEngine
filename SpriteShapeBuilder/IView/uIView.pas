unit uIView;

interface

uses
  FMX.Objects, System.Types, FMX.Graphics, FMX.Types,
  uSSBTypes, uIItemView, uMVPFrameWork;

type
  IMainView = interface(IView)
    function AddElement: IItemView;
    procedure RemoveElement(const AElement: IItemView);
    procedure SelectElement(const AElement: IItemView);
    function GetMousePos: TPoint;
    procedure ClearAndFreeImg;
    procedure SetBackground(const AImg: TImage);
    function FilenameFromDlg: string;
    procedure ChangeImageMouseDownHandler(const AHandler: TMouseEvent);
    procedure ChangeImageMouseUpHandler(const AHandler: TMouseEvent);
    procedure ChangeImageMouseMoveHandler(const AHandler: TMouseMoveEvent);
  end;

implementation

end.
