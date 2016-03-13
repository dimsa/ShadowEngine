unit uIItemPresenterEvent;

interface

uses
  System.Classes;

type
  IPresenterEvent = interface
    function GetOnMouseDown: TNotifyEvent;
    procedure SetOnMouseDown(AHandler: TNotifyEvent);
    function GetOnMouseUp: TNotifyEvent;
    procedure SetOnMouseUp(AHandler: TNotifyEvent);
    function GetOnMouseMove: TNotifyEvent;
    procedure SetOnMouseMove(AHandler: TNotifyEvent);

    property OnMouseDown: TNotifyEvent read GetOnMouseDown write SetOnMouseDown;
    property OnMouseUp: TNotifyEvent read GetOnMouseUp write SetOnMouseUp;
    property OnMouseMove: TNotifyEvent read GetOnMouseMove write SetOnMouseMove;
  end;

implementation

end.
