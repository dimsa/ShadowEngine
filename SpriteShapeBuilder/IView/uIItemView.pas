unit uIItemView;

interface

uses
  FMX.Graphics, uIItemPresenter;

type
  IItemView = interface
    procedure AssignBitmap(ABmp: TBitmap);
    function GetWidth: Integer;
    procedure SetWidth(AValue: Integer);
    function GetHeight: Integer;
    procedure SetHeight(AValue: Integer);
    function GetTop: Integer;
    procedure SetTop(AValue: Integer);
    function GetLeft: Integer;
    procedure SetLeft(AValue: Integer);
    function GetPresenter: IItemPresenter;
    procedure SetPresenter(AValue: IItemPresenter);


    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property Top: Integer read GetTop write SetTop;
    property Left: Integer read GetLeft write SetLeft;

    property Presenter: IItemPresenter read GetPresenter write SetPresenter;
  end;

implementation

end.
