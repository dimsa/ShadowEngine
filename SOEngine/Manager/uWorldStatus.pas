unit uWorldStatus;

interface

uses
  uSoModel, uSoTypes;

type

TSoModelFriend = class(TSoModel);

TWorldStatus = class
private
  FWidth, FHeight: Single;
//  PWidth, PHeight: PInteger;
  FRect: TRectObject;
  FModel: TSoModelFriend;
{    function GetHeight: Integer;
    function GetWidth: Integer;}
public
  procedure Resize;
  property Width: Single read FWidth;
  property Height: Single read FHeight;

  constructor Create(const AModel: TSoModel; const ARect: TRectObject);
end;


implementation

{ TWorldStatus }

constructor TWorldStatus.Create(const AModel: TSoModel; const ARect: TRectObject);
begin
  FModel := TSoModelFriend(AModel);

  FRect := ARect;
  {PWidth := AWidth;
  PHeight := AHeight;}

  Resize;
end;

{function TWorldStatus.GetHeight: Integer;
begin
  Result := FSize.Height;
end;

function TWorldStatus.GetWidth: Integer;
begin
  Result := FSize.Width;
end;   }

procedure TWorldStatus.Resize;
begin
  FWidth := FRect.Width;// PWidth^;
  FHeight := FRect.Height;// PHeight^;
end;

end.
