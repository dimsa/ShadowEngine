unit uIItemPresenterEvent;

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
      procedure SetOnHover(AHandler: TNotifyEvent);
      function GetOnHover: TNotifyEvent;
      property OnSelect: TNotifyEvent read GetOnSelect write SetOnSelect;
      property OnCapture: TNotifyEvent read GetOnCapture write SetOnCapture;
      property OnUnCapture: TNotifyEvent read GetOnCapture write SetOnCapture;
      property OnHover: TNotifyEvent read GetOnHover write SetOnHover;
  end;

implementation

end.
