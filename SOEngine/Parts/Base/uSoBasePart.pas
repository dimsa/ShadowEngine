unit uSoBasePart;

interface

uses
  System.Classes,
  uSoObject;

type
  TSoBasePart = class abstract
  private
    FOnDestroy: TNotifyEvent;
    FEnabled: Boolean;
  protected
    FSubject: TSoObject;
    procedure OnSubjectDestroy(ASender: TObject); virtual;
    procedure SetEnabled(AValue: Boolean); virtual;
  public
    property Subject: TSoObject read FSubject;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    constructor Create(const ASubject: TSoObject); virtual;
    destructor Destroy; override;
  end;

implementation

{ TSoBasePart }

constructor TSoBasePart.Create(const ASubject: TSoObject);
begin
  FSubject := ASubject;
  FSubject.AddDestroyHandler(OnSubjectDestroy);
  FEnabled := True;
end;

destructor TSoBasePart.Destroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);

  FSubject := nil;
  inherited;
end;

procedure TSoBasePart.OnSubjectDestroy(ASender: TObject);
begin
  FSubject.RemoveDestroyHandler(OnSubjectDestroy);
  Free;
end;

procedure TSoBasePart.SetEnabled(AValue: Boolean);
begin
  FEnabled := AValue;
end;

end.
