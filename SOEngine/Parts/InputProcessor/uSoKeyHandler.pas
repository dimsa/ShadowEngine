unit uSoKeyHandler;

interface

uses
  FMX.Types, System.Classes,
  uSoObject, uSoBasePart;

type
  TSoKeyHandler = class(TSoBasePart)
  private
    FOnKeyDown, FOnKeyUp: TKeyEvent;
    FEnabled: Boolean;
    procedure SetOnKeyDown(const Value: TKeyEvent);
    procedure SetOnKeyUp(const Value: TKeyEvent);
    procedure EmptyKeyHandler(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure KeyDown(Key: Word; KeyChar: Char; Shift: TShiftState);
    procedure KeyUp(Key: Word; KeyChar: Char; Shift: TShiftState);
  public
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnKeyDown: TKeyEvent read FOnKeyDown write SetOnKeyDown; // May be move it to constructor?
    property OnKeyUp: TKeyEvent read FOnKeyUp write SetOnKeyUp; // May be move it to constructor?
    constructor Create(const ASubject: TSoObject); override;
    destructor Destroy; override;
  end;

implementation

{ TEngine2DKeyboardProcessor }

constructor TSoKeyHandler.Create(const ASubject: TSoObject);
begin
  inherited Create(ASubject);

  FOnKeyDown := EmptyKeyHandler;
  FOnKeyUp := EmptyKeyHandler;
end;


destructor TSoKeyHandler.Destroy;
begin

  inherited;
end;

procedure TSoKeyHandler.EmptyKeyHandler(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin

end;

procedure TSoKeyHandler.KeyDown(Key: Word; KeyChar: Char; Shift: TShiftState);
begin
  FOnKeyDown(FSubject, Key, KeyChar, Shift);
end;

procedure TSoKeyHandler.KeyUp(Key: Word; KeyChar: Char; Shift: TShiftState);
begin
  FOnKeyUp(FSubject, Key, KeyChar, Shift);
end;

procedure TSoKeyHandler.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TSoKeyHandler.SetOnKeyDown(const Value: TKeyEvent);
begin
  FOnKeyDown := Value;
end;

procedure TSoKeyHandler.SetOnKeyUp(const Value: TKeyEvent);
begin
  FOnKeyUp := Value;
end;

end.
