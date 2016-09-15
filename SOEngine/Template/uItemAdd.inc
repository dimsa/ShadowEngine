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

  if Assigned(FOnAdd) then
  begin
    FOnAdd(Self, TOnAddContainerEventArgs.Create(TSoBasePart(AItem).Subject, AItem) );
  end;

  AItem.AddDestroyHandler(OnItemDestroy);

  inherited Add(AItem, AName);
