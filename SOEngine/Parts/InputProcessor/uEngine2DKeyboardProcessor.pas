unit uEngine2DKeyboardProcessor;

interface

uses
  FMX.Types, System.Classes,
  uSoContainer;

type
  TEngine2DKeyboardProcessor = class
  private
    FSubject: TSoContainer;
    FOnKeyDown, FOnKeyUp: TKeyEvent;
    FEnabled: Boolean;
    procedure SetOnKeyDown(const Value: TKeyEvent);
    procedure SetOnKeyUp(const Value: TKeyEvent);
    procedure EmptyKeyHandler(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure SetEnabled(const Value: Boolean);
  public
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnKeyDown: TKeyEvent read FOnKeyDown write SetOnKeyDown; // May be move it to constructor?
    property OnKeyUp: TKeyEvent read FOnKeyUp write SetOnKeyUp; // May be move it to constructor?
    procedure KeyDown(Key: Word; KeyChar: Char; Shift: TShiftState);
    procedure KeyUp(Key: Word; KeyChar: Char; Shift: TShiftState);
    constructor Create(const ASubject: TSoContainer);
    destructor Destroy; override;
  end;

implementation

{ TEngine2DKeyboardProcessor }

constructor TEngine2DKeyboardProcessor.Create(const ASubject: TSoContainer);
begin
  FSubject := ASubject;
  FOnKeyDown := EmptyKeyHandler;
  FOnKeyUp := EmptyKeyHandler;
end;


destructor TEngine2DKeyboardProcessor.Destroy;
begin
  FSubject := nil;
  inherited;
end;

procedure TEngine2DKeyboardProcessor.EmptyKeyHandler(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin

end;

procedure TEngine2DKeyboardProcessor.KeyDown(Key: Word; KeyChar: Char; Shift: TShiftState);
begin
  FOnKeyDown(FSubject, Key, KeyChar, Shift);
end;

procedure TEngine2DKeyboardProcessor.KeyUp(Key: Word; KeyChar: Char; Shift: TShiftState);
begin
  FOnKeyUp(FSubject, Key, KeyChar, Shift);
end;

procedure TEngine2DKeyboardProcessor.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TEngine2DKeyboardProcessor.SetOnKeyDown(const Value: TKeyEvent);
begin
  FOnKeyDown := Value;
end;

procedure TEngine2DKeyboardProcessor.SetOnKeyUp(const Value: TKeyEvent);
begin
  FOnKeyUp := Value;
end;

end.
