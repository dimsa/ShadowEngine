unit uTemplateLoader;

interface

uses
  System.JSON;

type
  // Delegates for Loading from SEJSon or SeCss or etc files
  TDelegateLoadFromJson = procedure(const AClassName: string; AJson: TJsonObject);
  TDelegateLoadFromFile = procedure(const AClassName: string; AFileName: string);

  ITemplateLoader = interface
    procedure AddJsonTemplate(const AClassName: string; AJson: TJsonObject);
    procedure AddTemplateFromFile(const AClassName: string; AFileName: string);
  end;

  TTemplateLoader = class(TInterfacedObject, ITemplateLoader)
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

procedure TTemplateLoader.AddJsonTemplate(const AClassName: string; AJson: TJsonObject);
begin
  FDelegateLoadFromJson(AClassName, AJson);
end;

procedure TTemplateLoader.AddTemplateFromFile(const AClassName: string; AFileName: string);
begin
  FDelegateLoadFromFile(AClassName, AFileName);
end;

constructor TTemplateLoader.Create(const ALoadFromJson: TDelegateLoadFromJson; const ALoadFromFile: TDelegateLoadFromFile);
begin
  FDelegateLoadFromFile := ALoadFromFile;
  FDelegateLoadFromJson := ALoadFromJson;
end;

end.
