unit uSSBPresenters;

interface

uses
  System.Classes, System.UITypes, System.Types, System.SysUtils, System.Generics.Collections,
  FMX.StdCtrls, FMX.Controls, FMX.Graphics, FMX.Objects, FMX.Dialogs, FMX.Types,
  uSSBModels, uClasses, uEasyDevice, uIView, uIItemView, uMVPFrameWork;


type
  TMainPresenter = class(TPresenter)
  private
    function GetView: IMainView;
  protected
    property View: IMainView read GetView;
  public
    constructor Create(AView: IMainView); virtual;
    procedure Init; virtual;
    destructor Destroy; override;
  end;

  TSSBObjecterPresenter = class(TMainPresenter)
  public
    procedure SelObj;
    procedure AddObj;
    procedure DelObj;
    procedure EditObj;
    procedure ImgAutoConvert;
  end;

  TSSBShaperPresenter = class(TMainPresenter)
  public
    procedure SelObj;
    procedure AddShape;
    procedure DelShape;
    procedure SelShape;
    procedure EditShape;
  end;

implementation

{ TSSBPresenter }

constructor TMainPresenter.Create(AView: IMainView);
begin
  FView := AView;
end;

destructor TMainPresenter.Destroy;
begin
  FView := nil;
  inherited;
end;

function TMainPresenter.GetView: IMainView;
begin
  Result := IMainView(FView);
end;

procedure TMainPresenter.Init;
begin

end;

{ TSSBObjecterPresenter }

procedure TSSBObjecterPresenter.AddObj;
begin

end;

procedure TSSBObjecterPresenter.DelObj;
begin

end;

procedure TSSBObjecterPresenter.EditObj;
begin

end;

procedure TSSBObjecterPresenter.ImgAutoConvert;
begin

end;

procedure TSSBObjecterPresenter.SelObj;
begin

end;

{ TSSBShaperPresenter }

procedure TSSBShaperPresenter.AddShape;
begin

end;

procedure TSSBShaperPresenter.DelShape;
begin

end;

procedure TSSBShaperPresenter.EditShape;
begin

end;

procedure TSSBShaperPresenter.SelObj;
begin

end;

procedure TSSBShaperPresenter.SelShape;
begin

end;

end.
