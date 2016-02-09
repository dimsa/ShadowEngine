unit uIView;

interface

uses
  FMX.Objects, System.Types, FMX.Dialogs, FMX.Graphics, FMX.Controls, FMX.Types,
  uSSBTypes;

type
  ISSBViewElement = interface
    procedure AssignBitmap(ABmp: TBitmap);
    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);
    function GetHeight: Integer;
    procedure SetHeight(AValue: Integer);
    function GetTop: Integer;
    procedure SetTop(AValue: Integer);
    function GetLeft: Integer;
    procedure SetLeft(AValue: Integer);

    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Top: Integer read GetTop write SetTop;
    property Left: Integer read GetLeft write SetLeft;
  end;

  ISSBView = interface
    function AddElement: ISSBViewElement;
    procedure RemoveElement(const AElement: ISSBViewElement);
    procedure SelectElement(const AElement: ISSBViewElement);
    function GetMousePos: TPoint;
    function ElementUnderMouse: ISSBViewElement;
    procedure ClearAndFreeImg;
    procedure SetBackground(const AImg: TImage);
    property MousePos: TPoint read GetMousePos;
    function FilenameFromDlg: string;
    procedure ChangeImageMouseDownHandler(const AHandler: TMouseEvent);
    procedure ChangeImageMouseUpHandler(const AHandler: TMouseEvent);
    procedure ChangeImageMouseMoveHandler(const AHandler: TMouseMoveEvent);
  end;

implementation

end.
