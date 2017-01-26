unit uSoTemplateLoader;

interface

uses
  System.JSON;

type
  // Delegates for Loading from SEJSon or SeCss or etc files
  TDelegateLoadFromJson = procedure(const AClassName: string; AJson: TJsonObject) of object;
  TDelegateLoadFromFile = procedure(const AClassName: string; AFileName: string) of object;

  ISoTemplateLoader = interface
    procedure AddJsonTemplate(const AClassName: string; AJson: TJsonObject);
    procedure AddTemplateFromFile(const AClassName: string; AFileName: string);
  end;

  TSoTemplateLoader = class(TInterfacedObject, ISoTemplateLoader)
  private
    FDelegateLoadFromJson: TDelegateLoadFromJson;
    FDelegateLoadFromFile: TDelegateLoadFromFile;
  public
    procedure AddJsonTemplate(const AClassName: string; AJson: TJsonObject);
    procedure AddTemplateFromFile(const AClassName: string; AFileName: string);

    constructor Create(const ALoadFromJson: TDelegateLoadFromJson; const ALoadFromFile: TDelegateLoadFromFile);
  end;

implementation

{ TTemplateLoader }

procedure TSoTemplateLoader.AddJsonTemplate(const AClassName: string; AJson: TJsonObject);
begin
  FDelegateLoadFromJson(AClassName, AJson);
end;

procedure TSoTemplateLoader.AddTemplateFromFile(const AClassName: string; AFileName: string);
begin
  FDelegateLoadFromFile(AClassName, AFileName);
end;

constructor TSoTemplateLoader.Create(const ALoadFromJson: TDelegateLoadFromJson; const ALoadFromFile: TDelegateLoadFromFile);
begin
  FDelegateLoadFromFile := ALoadFromFile;
  FDelegateLoadFromJson := ALoadFromJson;
end;

end.
