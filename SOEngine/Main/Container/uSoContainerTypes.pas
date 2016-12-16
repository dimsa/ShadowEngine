unit uSoContainerTypes;

interface

uses
  uSoObject, uSoBasePart;

type

  TOnAddContainerEventArgs = record
    Subject: TSoObject;
    BasePart: TSoBasePart;
    constructor Create(const ASubject: TSoObject; ABasePart: TSoBasePart);
  end;

implementation

{ TOnAddContainerEventArgs }

constructor TOnAddContainerEventArgs.Create(const ASubject: TSoObject; ABasePart: TSoBasePart);
begin
  Subject := ASubject;
  BasePart := ABasePart;
end;

end.
