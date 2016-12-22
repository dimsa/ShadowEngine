unit uISoObject;

interface

uses
  uSoTypes, uSoPosition, uSoProperty;

type
  ISoObject = interface
    function GetProperty(APropertyName: string): TSoProperty;
    function GetHeight: Single;
    function GetWidth: Single;
    function GetPosition: TSoPosition;

    property Position: TSoPosition read GetPosition;
    property Width: Single read GetWidth;
    property Height: Single read GetHeight;
    property Properties[APropertyName: string]: TSoProperty read GetProperty; default;

    function HasProperty(const APropertyName: string): Boolean;
    function AddProperty(const AName: string): TSoProperty;

    procedure AddDestroyHandler(const AHandler: TNotifyEvent);
    procedure RemoveDestroyHandler(const AHandler: TNotifyEvent);
    procedure SetPositionSilent(const AX, AY: Single; const ARotate: Single);

    procedure Kill;
  end;

implementation

end.
