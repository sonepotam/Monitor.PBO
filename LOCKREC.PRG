function lockit( fileName, recNo)
if !file( fileName)
   ? "File not found..."
else
  use ( fileName) shared readonly new
  if netErr()
     ? "Can't open file " + fileName
  else
     go val( recno)
     rLock()
     ? "Locked.Press a key..."
     inkey( 0)
     unlock
     close
  endif
endif
return NIL
