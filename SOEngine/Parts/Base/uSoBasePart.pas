unit uSoBasePart;

interface

uses
  System.Classes,
  uSoObject, uCommonClasses;

type
  TSoBasePart = class abstract
  private
    FOnDestroyHandlers: TNotifyEventList;
    FEnabled: Boolean;
  protected
    FSubject: TSoObject;
    procedure OnSubjectDestroy(ASender: TObject); virtual;
    procedure SetEnabled(AValue: Boolean); virtual;
  public
    property Subject: TSoObject read FSubject;
    property Enabled: Boolean read FEnabled write SetEnabled;
    procedure AddDestroyHandler(const AHandler: TNotifyEvent);
    procedure RemoveDestroyHandler(const AHandler: TNotifyEvent);
    constructor Create(const ASubject: TSoObject); virtual;
    destructor Destroy; override;
  end;

implementation

{ TSoBasePart }

procedure TSoBasePart.AddDestroyHandler(const AHandler: TNotifyEvent);
begin
  FOnDestroyHandlers.Add(AHandler);
end;

constructor TSoBasePart.Create(const ASubject: TSoObject);
begin
  FSubject := ASubject;
  FSubject.AddDestroyHandler(OnSubjectDestroy);
  FEnabled := True;
  FOnDestroyHandlers := TNotifyEventList.Create;
end;

destructor TSoBasePart.Destroy;
begin
  FOnDestroyHandlers.RaiseEvent(Self);

  FOnDestroyHandlers.Free;

  FSubject := nil;
  inherited;
end;

procedure TSoBasePart.OnSubjectDestroy(ASender: TObject);
begin
  FSubject.RemoveDestroyHandler(OnSubjectDestroy);
  Free;
end;

procedure TSoBasePart.RemoveDestroyHandler(const AHandler: TNotifyEvent);
begin
  FOnDestroyHandlers.Remove(AHandler);
end;

procedure TSoBasePart.SetEnabled(AValue: Boolean);
begin
  FEnabled := AValue;
end;

end.
