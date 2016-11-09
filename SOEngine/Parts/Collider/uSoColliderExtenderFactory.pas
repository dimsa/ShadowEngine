unit uSoColliderExtenderFactory;

interface

uses
  uSoTypes,
  uSoColliderOptions, uSoColliderExtender, uSoBox2DExtender;

type
  TSoColliderExtenderFactory = class
  private
    FOptions: TSoColliderOptions;
  public
    constructor Create(const AOptions: TSoColliderOptions);
    function Produce: TSoColliderExtender;
  end;

implementation

{ TSoColliderExtenderFactory }

constructor TSoColliderExtenderFactory.Create(
  const AOptions: TSoColliderOptions);
begin
  FOptions := AOptions;
end;

function TSoColliderExtenderFactory.Produce: TSoColliderExtender;
begin
  if FOptions.IsUsingBox2D then
    Exit(TSoBox2DExtender.Create(FOptions));
  raise Exception.Create('Can not produce ' + Self.ClassName);
end;

end.
