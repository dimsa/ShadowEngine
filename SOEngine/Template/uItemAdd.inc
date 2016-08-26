  Inc(FAddedObjects);
  if AName = '' then
    vName := 'genname'+IntToStr(FAddedObjects)
  else
    vName := AName;

  if FList.IsHere(AItem) then
  begin
    raise Exception.Create('You are trying to add ' + AItem.ClassName + ' with Name that already Exists in ' + Self.ClassName);
    Exit;
  end;

  AItem.OnDestroy := OnItemDestroy;

  inherited Add(AItem, AName);
