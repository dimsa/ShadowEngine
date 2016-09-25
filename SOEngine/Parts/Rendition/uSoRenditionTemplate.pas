unit uSoRenditionTemplate;

interface

uses
  System.Generics.Collections,
  uE2DRendition, System.JSON;

type
  TSoRenditionTemplate = class
  public
    function Instantiate: TEngine2DRendition; virtual; abstract;
  end;

  TSoSpriteTemplate = class(TSoRenditionTemplate)
  private
    FJson: TJSONObject;
    FResourceList: TList<Integer>;
  public
    function Instantiate: TEngine2DRendition; override;
    constructor Create(const AJson: TJSONObject);
  end;

  TSoShapeTemplate = class(TSoRenditionTemplate)
  private
    FJson: TJSONObject;
  public
    function Instantiate: TEngine2DRendition; override;
    constructor Create(const AJson: TJSONObject);
  end;

  TSoTextTemplate = class(TSoRenditionTemplate)
  private
    FJson: TJSONObject;
  public
    function Instantiate: TEngine2DRendition; override;
    constructor Create(const AJson: TJSONObject);
  end;

implementation

{ TSoTextTemplate }

constructor TSoTextTemplate.Create(const AJson: TJSONObject);
begin

end;

function TSoTextTemplate.Instantiate: TEngine2DRendition;
begin

end;

{ TSoShapeTemplate }

constructor TSoShapeTemplate.Create(const AJson: TJSONObject);
begin

end;

function TSoShapeTemplate.Instantiate: TEngine2DRendition;
begin

end;

{ TSoSpriteTemplate }

constructor TSoSpriteTemplate.Create(const AJson: TJSONObject);
begin

end;

function TSoSpriteTemplate.Instantiate: TEngine2DRendition;
begin

end;

end.
