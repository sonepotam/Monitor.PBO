/***
*   
*  Browse.prg
*
*  Database browse function
*
*  Copyright (c) 1990-1993, Computer Associates International Inc.
*  All rights reserved.
*
*  Compile: /n
*
*/

#include "inkey.ch"
#include "setcurs.ch"
#include "clippExt.ch"


// This code block will toggle insert mode and cursor
static bInsToggle := {|| SetCursor( if( ReadInsert( !ReadInsert() ), ;
                                        SC_NORMAL, SC_INSERT ))      }


/***
*
*  Browse( [nTop], [nLeft], [nBottom], [nRight] )
*
*	View, add, change, delete
*
*/
function browseFiz( y1, x1, oB, aH, aC, bIdle, bApp, bDel, fk,userDefineKeys, astr)
local n, lMore, cScrSave, lAppend, lKillAppend,;
      nKey, nCursSave, lGotKey, bKeyBlock, get, str, i, arr, searchStr,;
      seekStr, saveStr, savePos, rV



str       := space( 40)
get       := getNew( 2, 3 + len( aStr[ 1]))
get:block := block( str)
get:colorSpec := setGetColor()
get:setFocus()
get:display()



	// init
	lAppend := lKillAppend := .F.
	nCursSave := SetCursor(0)
        
	// while ( !oB:stabilize() ) ; end
        // oB: goTop()
        oB: refreshAll(): forceStable()
        sayCliFooting( oB:nBottom + 2, oB:nLeft)

	if ( LastRec() == 0 )
		// empty file..force append mode
		nKey := K_DOWN
		lGotKey := .t.
	else
		lGotKey := .f.
	end

	lMore := .t.
	while (lMore)
      get:display()

        //
        // подвал
        //
        sayCliFooting( oB:nBottom + 2, oB:nLeft)
        // @ 2, x1 + 1 say aStr[ 1]
        @ 2, x1 + 1 say aStr[ CLIENTS ->( indexOrd())]

      if wStBrwsTop()[ 2]
         oB:refreshAll()
         oB:forceStable()
         wStBrwsTop()[ 2] := .F.
         wStBrwsTop()[ 3] := .F.
      endif

      if wStBrwsTop()[ 3]
         oB:refreshCurrent()
         oB:forceStable()
         wStBrwsTop()[ 3] := .F.
      endif


    if ( !lGotKey )
			// stabilization will be interrupted by any keystroke
			while ( !oB:stabilize() )
				if ( (nKey := Inkey()) != 0 )
					lGotKey := .t.
					exit
				end
			end
		end

		if ( !lGotKey )
			// the TBrowse object is stable
			if ( oB:hitBottom )
				if ( !lAppend .or. Recno() != LastRec() + 1 )
					if ( lAppend )
						// continue appending..restore color to current row
						oB:refreshCurrent()
						while ( !oB:stabilize() ) ; end

						// ensure bottom of file without refresh
						go bottom
					else
						// begin append mode
					//	lAppend := .t.

						// turn the cursor on
					//	SetCursor( if(ReadInsert(), SC_INSERT, SC_NORMAL) )
					end

					// move to next row and stabilize to set rowPos
					// oB:down()
					while ( !oB:stabilize() ) ; end

				end
			end

			// display status and stabilize again for correct cursor pos
			// Statline(oB, lAppend)
                        // sayString()
			// while ( !oB:stabilize() ) ; end
                        oB:forceStable()
                        sayCliFooting( oB:nBottom + 2, oB:nLeft)

			// idle
      get:display()
      nKey := Inkey0( fk)

			if ( (bKeyBlock := SetKey(nKey)) != NIL )
				// run SET KEY block
				Eval(bKeyBlock, ProcName(1), ProcLine(1), "")
				loop	// NOTE
			end
		else
			// reset for next loop
			lGotKey := .f.
		end

		do case
       //
       // вставка для FIZ
       //
       case nkey == K_BS .and. !empty( get: buffer )
            get:backSpace()
            cliSeek( rTrim( get:buffer))
            oB:refreshAll()
            oB:forceStable()
       case nkey == K_DEL
            get:Delete()
               if CLIENTS ->( indexOrd()) == 2
                  arr := ctoa( rTrim( get:buffer), " ")
                  searchStr := ""
                  for i := 1 to len( arr) do searchStr += padR( arr[ i], 20, " ") + " "
                  cliSeek( rTrim( r_upper( searchStr)))
               else
                  cliSeek( rTrim( get:buffer))
               endif
            oB:refreshAll()
            oB:forceStable()
       case nkey == K_INS
            if yesNo( "Вы действительно хотите ввести нового клиента ?")
               cliNewClient()               
              oB:refreshAll()
            endif

   case ( nKey == K_DOWN ) 
         for i := 1 to 30 do get:backSpace()
         oB:down() /*
         if ( lAppend)
            oB:hitBottom := .t.
	 else
	    oB:down()
	 end*/

		case ( nKey == K_UP )
   for i := 1 to 30 do get:backSpace()
   if ( lAppend )
				lKillAppend := .t.
			else
				oB:up()
			end

		case ( nKey == K_PGDN )
//   get: buffer := " "
   for i := 1 to 30 do get:backSpace()
			if ( lAppend )
				oB:hitBottom := .t.
			else
				oB:pageDown()
			end

		case ( nKey == K_PGUP )
   for i := 1 to 30 do get:backSpace()
			if ( lAppend )
				lKillAppend := .t.
			else
				oB:pageUp()
			end

		case ( nKey == K_CTRL_PGUP )
   for i := 1 to 30 do get:backSpace()
   if ( lAppend )
				lKillAppend := .t.
			else
				oB:goTop()
			end

		case ( nKey == K_CTRL_PGDN )
   for i := 1 to 30 do get:backSpace()
   if ( lAppend )
    lKillAppend := .t.   
			else
				oB:goBottom()
			end

		case ( nKey == K_RIGHT )
			oB:right()

		case ( nKey == K_LEFT )
			oB:left()

		case ( nKey == K_HOME )
			oB:home()

		case ( nKey == K_END )
			oB:end()

		case ( nKey == K_CTRL_LEFT )
			oB:panLeft()

		case ( nKey == K_CTRL_RIGHT )
			oB:panRight()

		case ( nKey == K_CTRL_HOME )
			oB:panHome()

		case ( nKey == K_CTRL_END )
			oB:panEnd()

		case ( nKey == K_ESC )
			// exit browse
			lMore := .f.
    case fKeysProc( fk, nKey)
         for i := 1 to 30 do get:backSpace()

    case nkey >= 32 .AND. nkey <= 255 .AND. ;
         empty( aScan( userDefineKeys, {|x| x[ 1] == nkey}))
         rV      := .F.
         seekStr := r_upper( get: buffer )
         savePos := CLIENTS -> ( recNo())
         if set( _SET_INSERT)
           seekStr := left( seekStr, get: pos - 1 ) + r_upper( chr( nKey )) +;
                      right( seekStr, len( seekStr ) - get: pos + 1 )
         else
           seekStr := left( seekStr, get: pos - 1 ) + r_upper( chr( nKey )) +;
                      right( seekStr, len( seekStr ) - get: pos )

         endif
           if CLIENTS ->( indexOrd()) == 2
              arr := ctoa( rTrim( seekStr ), " ")
              searchStr := ""
              for i := 1 to len( arr) do searchStr += padR( arr[ i], 20, " ") + " "
              rV := cliSeek( rTrim( r_upper( searchStr)))
            else
              rV := cliSeek( rTrim( seekStr ))
            endif
          if rV
            if set( _SET_INSERT)
              get:Insert( r_upper( chr( nkey)))
            else
              get:OverStrike( r_upper( chr( nkey)))
            endif
            saycliFooting( oB:nBottom + 2, oB:nLeft)
          else
            beep()
            CLIENTS -> ( dbGoTo( savePos ))
          endIf
          get: display()
          oB: refreshAll()
          oB: forceStable()


    otherwise
         if( i := aScan( userDefineKeys, {|x| x[ 1] == nkey})) >0
            if CLIENTS ->( mRLock())
               CLIENTS ->( baseSavePos())
               eval( userDefineKeys[ i][ 2])
               CLIENTS ->( baseRestPos())
               CLIENTS ->( netUnLock())
               for i := 1 to 30 do get:backSpace()
            else
               messageB( "С этим клиентом уже pаботает дpугой опеpатоp")
            endif
         endif
         browseHandle( nkey, oB, aH, aC, bApp, bDel)
         sayCliFooting( oB:nBottom + 2, oB:nLeft)

       end

		if ( lKillAppend )
			// turn off append mode
			lKillAppend := .f.
			lAppend := .f.

			// refresh respecting any change in index order
			SetCursor(0)
		end
	end

	// restore
	SetCursor(nCursSave)
get:killFocus()

return (.t.)



/***
*
*	DoGet()
*
*	Edit the current field
*
*/
static func DoGet( oB, lAppend )

local bInsSave, lScoreSave, lExitSave
local oCol, oGet, nKey, cExpr, xEval
local lFresh, nCursSave, mGetVar
local cForCond

	// make sure the display is correct
	oB:hitTop := .f.
	Statline(oB, lAppend)
	while ( !oB:stabilize() ) ; end

	// save state
	lScoreSave := Set(_SET_SCOREBOARD, .f.)
	lExitSave := Set(_SET_EXIT, .t.)

	// set insert key to toggle insert mode and cursor
	bInsSave := SetKey(K_INS, bInsToggle)

	// turn the cursor on
	nCursSave := SetCursor( if(ReadInsert(), SC_INSERT, SC_NORMAL) )

	// get the controlling index key
	cExpr := IndexKey(0)
	if ( !Empty(cExpr) )
		// expand key expression for later comparison
		xEval := &cExpr
	end

	// get column object from browse
	oCol := oB:getColumn(oB:colPos)

	// use temp for safety
	mGetVar := Eval(oCol:block)

	// create a corresponding GET with ambiguous set/get block
	oGet := GetNew(Row(), Col(),									;
				   {|x| if(PCount() == 0, mGetVar, mGetVar := x)},	;
				   "mGetVar",, oB:colorSpec)

	// refresh flag
	lFresh := .f.

	// read it
	if ( ReadModal( {oGet} ) )
		// new data has been entered
		if ( lAppend .and. Recno() == LastRec() + 1 )
			// new record confirmed
			APPEND BLANK
		end

		// replace with new data
		Eval(oCol:block, mGetVar)

      // test for dropping out of a conditional index
      if ( !lAppend .AND. !empty( cForCond := ordFor( IndexOrd() )))
         if !( &( cForCond ))
            dbGoTop()
         endif
      endif

      // test for change in index order
      if ( !lAppend .and. !Empty(cExpr) )
         if ( xEval != &cExpr )
            // change in index key eval
            lFresh := .t.
         end
      end

	end

	if ( lFresh )
		// record in new indexed order
		FreshOrder(oB)

		// no other action
		nKey := 0
	else
		// refresh the current row only
		oB:refreshCurrent()

		// certain keys move cursor after edit if no refresh
		nKey := ExitKey(lAppend)
	end

	if ( lAppend )
		// maintain special row color
		oB:colorRect({oB:rowPos,1,oB:rowPos,oB:colCount}, {2,2})
	end

	// restore state
	SetCursor(nCursSave)
	Set(_SET_SCOREBOARD, lScoreSave)
	Set(_SET_EXIT, lExitSave)
	SetKey(K_INS, bInsSave)

return (nKey)



/***
*
*	ExitKey()
*
*	Determine the follow-up action after editing a field
*
*/
static func ExitKey(lAppend)

local nKey

	nKey := LastKey()
	if ( nKey == K_PGDN )
		// move down if not append mode
		if ( lAppend )
			nKey := 0
		else
			nKey := K_DOWN
		end

	elseif ( nKey == K_PGUP )
		// move up if not append mode
		if ( lAppend )
			nKey := 0
		else
			nKey := K_UP
		end

	elseif ( nKey == K_RETURN .or. (nKey >= 32 .and. nKey <= 255) )
		// return key or type out..move right
		nKey := K_RIGHT

	elseif ( nKey != K_UP .and. nKey != K_DOWN )
		// no other action
		nKey := 0
	end

return (nKey)



/***
*
*	FreshOrder()
*
*	Refresh respecting any change in index order
*
*/
static func FreshOrder(oB)

local nRec

	nRec := Recno()
	oB:refreshAll()

	// stabilize to see if TBrowse moves the record pointer
	while ( !oB:stabilize() ) ; end

	if ( nRec != LastRec() + 1 )
		// record pointer may move if bof is on screen
		while ( Recno() != nRec .AND. !BOF() )
			// falls through unless record is closer to bof than before
			oB:up()
			while ( !oB:stabilize() ) ; end
		end
	end

return (NIL)



/***
*
*	Statline()
*
*	display status at coordinates relative to TBrowse object
*
*/
static func Statline(oB, lAppend)

local nTop, nRight

	nTop := oB:nTop - 1
	nRight := oB:nRight

	@ nTop, nRight - 27 say "Record "
	if ( LastRec() == 0 .and. !lAppend )
		// file is empty
		@ nTop, nRight - 20 say "<none>               "
	elseif ( Recno() == LastRec() + 1 )
		// no record number if eof
		@ nTop, nRight - 40 say "         "
		@ nTop, nRight - 20 say "                <new>"
	else
		// normal record..display Recno()/LastRec() and Deleted()
		@ nTop, nRight - 40 say If(Deleted(), "<Deleted>", "         ")
		@ nTop, nRight - 20 say Pad(Ltrim(Str(Recno())) + "/" +;
									Ltrim(Str(LastRec())), 16) +;
								If(oB:hitTop, "<bof>", "     ")
	end

return (NIL)



/***
*
*  Skipped( n )
*
*	Skip thru database and return the
*	actual number of records skipped
*
*/
static func Skipped( nRequest, lAppend )

local nCount

	nCount := 0
	if ( LastRec() != 0 )
		if ( nRequest == 0 )
			skip 0

		elseif ( nRequest > 0 .and. Recno() != LastRec() + 1 )
			// forward
			while ( nCount < nRequest )
				skip 1
				if ( Eof() )
					if ( lAppend )
						// eof record allowed if append mode
						nCount++
					else
						// back to last actual record
						skip -1
					end

					exit
				end

				nCount++
			end

		elseif ( nRequest < 0 )
			// backward
			while ( nCount > nRequest )
				skip -1
				if ( Bof() )
					exit
				end

				nCount--
			end
		end
	end

return (nCount)


// eof browse.prg

