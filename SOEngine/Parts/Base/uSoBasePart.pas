unit uSoBasePart;

interface

uses
  System.Classes,
  uSoContainer;

type
  TSoBasePart = class abstract
  private
    FOnDestroy: TNotifyEvent;
    FEnabled: Boolean;
  protected
    FSubject: TSoContainer;
    procedure OnSubjectDestroy(ASender: TObject); virtual;
    procedure SetEnabled(AValue: Boolean); virtual;
  public
    property Subject: TSoContainer read FSubject;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    constructor Create(const ASubject: TSoContainer); virtual;
    destructor Destroy; override;
  end;

implementation

{ TSoBasePart }

constructor TSoBasePart.Create(const ASubject: TSoContainer);
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
