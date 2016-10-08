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
  FSize: TSizeObject;
  FModel: TSoModelFriend;
{    function GetHeight: Integer;
    function GetWidth: Integer;}
public
  procedure Resize;
  property Width: Single read FWidth;
  property Height: Single read FHeight;

  constructor Create(const AModel: TSoModel; const ASize: TSizeObject);
end;


implementation

{ TWorldStatus }

constructor TWorldStatus.Create(const AModel: TSoModel; const ASize: TSizeObject);
begin
  FModel := TSoModelFriend(AModel);

  FSize := ASize;
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
  FWidth := FSize.Width;// PWidth^;
  FHeight := FSize.Height;// PHeight^;
end;

end.
