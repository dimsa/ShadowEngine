unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  uSoEngine;

type
  TAsteroidsVsYou = class(TForm)
    MainImg: TImage;
    procedure FormCreate(Sender: TObject);
  private
    FEngine: TSoEngine;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AsteroidsVsYou: TAsteroidsVsYou;

implementation

{$R *.fmx}

procedure TAsteroidsVsYou.FormCreate(Sender: TObject);
begin
  FEngine := TSoEngine.Create(MainImg);
  FEngine.Start;
end;

end.
