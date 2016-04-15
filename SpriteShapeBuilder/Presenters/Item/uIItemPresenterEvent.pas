unit uIItemPresenterEvent;

interface

uses
  System.Classes;

type
  IPresenterEvent = interface
    ['{EB8751E5-2A39-4696-9A52-A65A8F8420AC}']
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
