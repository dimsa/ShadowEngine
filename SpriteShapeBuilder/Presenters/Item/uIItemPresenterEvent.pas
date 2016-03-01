unit uIItemPresenterEvent;

interface

uses
  System.Classes;

type
  IPresenterEvent = interface
   {   procedure SetOnSelect(AHandler: TNotifyEvent);
      function GetOnSelect: TNotifyEvent;
      procedure SetOnCapture(AHandler: TNotifyEvent);
      function GetOnCapture: TNotifyEvent;
      procedure SetOnUnCapture(AHandler: TNotifyEvent);
      function GetOnUnCapture: TNotifyEvent;
      procedure SetOnHover(AHandler: TNotifyEvent);
      function GetOnHover: TNotifyEvent;
      property OnSelect: TNotifyEvent read GetOnSelect write SetOnSelect;
      property OnCapture: TNotifyEvent read GetOnCapture write SetOnCapture;
      property OnUnCapture: TNotifyEvent read GetOnCapture write SetOnCapture;
      property OnHover: TNotifyEvent read GetOnHover write SetOnHover;    }

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
