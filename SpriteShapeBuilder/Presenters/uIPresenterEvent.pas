unit uIPresenterEvent;

interface

uses
  System.Classes;

type
  IPresenterEvent = interface
      procedure SetOnSelect(AHandler: TNotifyEvent);
      function GetOnSelect: TNotifyEvent;
      procedure SetOnCapture(AHandler: TNotifyEvent);
      function GetOnCapture: TNotifyEvent;
      procedure SetOnUnCapture(AHandler: TNotifyEvent);
      function GetOnUnCapture: TNotifyEvent;
      property OnSelect: TNotifyEvent read GetOnSelect write SetOnSelect;
      property OnCapture: TNotifyEvent read GetOnCapture write SetOnCapture;
      property OnUnCapture: TNotifyEvent read GetOnCapture write SetOnCapture;
  end;

implementation

end.
