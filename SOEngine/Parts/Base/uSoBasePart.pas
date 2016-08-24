unit uSoBasePart;

interface

uses
  uSoContainer;

type
  TSoBasePart = class abstract
  protected
    FSubject: TSoContainer;
    procedure OnSubjectDestroy(ASender: TObject); virtual;
  public
    constructor Create(const ASubject: TSoContainer); virtual;
    destructor Destroy; override;
  end;

implementation

{ TSoBasePart }

constructor TSoBasePart.Create(const ASubject: TSoContainer);
begin
  FSubject := ASubject;
  FSubject.AddDestroyHandler(OnSubjectDestroy);
end;

destructor TSoBasePart.Destroy;
begin
  FSubject := nil;
  inherited;
end;

procedure TSoBasePart.OnSubjectDestroy(ASender: TObject);
begin
  FSubject.RemoveDestroyHandler(OnSubjectDestroy);
  Free;
end;

end.
