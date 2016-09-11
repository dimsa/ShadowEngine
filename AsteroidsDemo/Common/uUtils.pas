unit uUtils;

interface

uses
  uEasyDevice;

function ResourcePath(const AFileName: string): string;

implementation

function ResourcePath(const AFileName: string): string;
begin
  Result := UniPath('../../../../art/' + AFileName);
end;

end.
