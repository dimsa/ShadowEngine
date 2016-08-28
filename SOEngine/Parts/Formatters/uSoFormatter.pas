unit uSoFormatter;

interface

uses
  FMX.Types, System.Classes,
  uSoContainer, uSoBasePart;

type
  TSoFormatter = class(TSoBasePart)
  public
    procedure Format;
    constructor Create(const ASubject: TSoContainer); override;
    destructor Destroy; override;
  end;

implementation

{ TSoFormatter }

constructor TSoFormatter.Create(const ASubject: TSoContainer);
begin
  inherited;

end;

destructor TSoFormatter.Destroy;
begin

  inherited;
end;

procedure TSoFormatter.Format;
begin

end;

end.
