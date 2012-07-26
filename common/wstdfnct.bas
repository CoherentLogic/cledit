
#lang "fblite"

DECLARE FUNCTION WordMatch (txt AS STRING, pattern AS STRING, match AS STRING) AS INTEGER
DECLARE FUNCTION Split (txt AS STRING, Delim AS STRING, WordArray() AS STRING) AS INTEGER
DECLARE FUNCTION nDirExists (DirName AS STRING) AS INTEGER
DECLARE FUNCTION nExists (FileName AS STRING) AS INTEGER
DECLARE FUNCTION nFigurePercent (Given AS DOUBLE, Max AS DOUBLE) AS DOUBLE
DECLARE FUNCTION nYesOrNo (Message AS STRING) AS INTEGER
DECLARE FUNCTION szDecrypt (InData AS STRING) AS STRING
DECLARE FUNCTION szDOSDriveLetter (MountPath AS STRING) AS STRING
DECLARE FUNCTION szEncrypt (InData AS STRING) AS STRING
DECLARE FUNCTION szGetRootDir (Path AS STRING, File AS STRING) AS STRING
DECLARE FUNCTION szKillChar (InText AS STRING, TrimChar AS STRING, ReplaceWith AS STRING) AS STRING
DECLARE FUNCTION szStringLeft (InText AS STRING, CharFilter AS STRING) AS STRING
DECLARE FUNCTION szStringRight (InText AS STRING, CharFilter AS STRING) AS STRING
DECLARE FUNCTION szStrReplace! ()
DECLARE FUNCTION szTrimText (InText AS STRING) AS STRING

'WSTDFNCT.BAS
' WillisWare Standard Functions/Subroutines

SUB ChangeDir (DestD AS STRING)
    
END SUB

 SUB ComLine (NumArgs, Args$(), MaxArgs)
 CONST true = -1, FALSE = 0
 
     NumArgs = 0: In = FALSE
     ' Get the command line using the COMMAND$ function
     Cl$ = COMMAND$
     L = LEN(Cl$)
     ' Go through the command line a character at a time
     FOR i = 1 TO L
          c$ = MID$(Cl$, i, 1)
          ' Test for character being a blank or a tab
          IF (c$ <> " " AND c$ <> CHR$(9)) THEN
          ' Neither blank nor tab; test if you're already inside
          ' an argument
               IF NOT In THEN
               ' You've found the start of a new argument
                    ' Test for too many arguments
                      IF NumArgs = MaxArgs THEN EXIT FOR
                      NumArgs = NumArgs + 1
                      In = true
               END IF
               ' Add the character to the current argument
               Args$(NumArgs) = Args$(NumArgs) + LCASE$(c$)
          ELSE
          ' Found a blank or a tab.
               ' Set "Not in an argument" flag to FALSE
               In = FALSE
          END IF
     NEXT i
 
 END SUB

FUNCTION nDirExists (DirName AS STRING) AS INTEGER
    DIM OldDir AS STRING
    ON LOCAL ERROR GOTO ErrHandler
    
    'Save the current directory
    OldDir = CURDIR$

    'Attempt to change to the directory
    CHDIR DirName

    'Check the error code
    IF ERR > 0 THEN
        nDirExists = FALSE
    ELSE
        nDirExists = true
    END IF
    CHDIR "\" + OldDir

    EXIT FUNCTION
ErrHandler:
    RESUME NEXT
END FUNCTION

FUNCTION nExists (FileName AS STRING) AS INTEGER
    DIM nExistsFile AS INTEGER
    ON LOCAL ERROR GOTO neErrHandler

    'Attempt to open the file
    nExistsFile = FREEFILE
    OPEN FileName FOR INPUT AS #nExistsFile

    'Check the error code
    IF ERR > 0 THEN
        nExists = 0
    ELSE
        nExists = -1
    END IF

    'Close the file
    CLOSE #nExistsFile
    EXIT FUNCTION
neErrHandler:
    RESUME NEXT

END FUNCTION

FUNCTION nFigurePercent (Given AS DOUBLE, Max AS DOUBLE) AS DOUBLE
    ON LOCAL ERROR GOTO nfpErrHandler
    DIM GivenMod1 AS DOUBLE
    'Percentage Algorithm
    GivenMod1 = 100 * Given
    nFigurePercent = INT(GivenMod1 / Max)
    EXIT FUNCTION
nfpErrHandler:
    RESUME NEXT
END FUNCTION

FUNCTION nYesOrNo (Message AS STRING) AS INTEGER
    DIM KeyName AS STRING
    PRINT Message + " (Y/N) "
    DO
        KeyName = INKEY$
    LOOP UNTIL LCASE$(KeyName) = "y" OR LCASE$(KeyName) = "n"

    IF LCASE$(KeyName) = "y" THEN
        nYesOrNo = true
    ELSE
        nYesOrNo = FALSE
    END IF
END FUNCTION

SUB ProgressBar (X AS INTEGER, Y AS INTEGER, ForeColor AS INTEGER, BackColor AS INTEGER, Value AS INTEGER)
    DIM leftOver AS INTEGER

    Value = Value / 2
    leftOver = 50 - Value

    LOCATE Y, X: COLOR ForeColor, BackColor
    PRINT STRING$(Value, 219); STRING$(leftOver, 176);
    LOCATE Y + 1, X + 23, 0: PRINT szTrimText(STR$(INT(Value * 2))); "%   ";

END SUB

SUB SetVideoMode (VMode AS STRING)

END SUB

FUNCTION Split (txt AS STRING, Delim AS STRING, WordArray() AS STRING) AS INTEGER
    DIM wLen AS INTEGER, i AS INTEGER, wordCount AS INTEGER
    DIM buildUp AS STRING, c AS STRING

    wLen = LEN(txt)
    
    FOR i = 1 TO wLen
        c = MID$(txt, i, 1)
        SELECT CASE c
            CASE Delim
                wordCount = wordCount + 1
                REDIM PRESERVE WordArray(wordCount) AS STRING
                WordArray(wordCount) = buildUp
                buildUp = ""
            CASE ELSE
                buildUp = buildUp + c
        END SELECT
        IF i = wLen THEN
            wordCount = wordCount + 1
            REDIM PRESERVE WordArray(wordCount) AS STRING
            WordArray(wordCount) = buildUp
        END IF
    NEXT i
    

    Split = wordCount
END FUNCTION

FUNCTION szDecrypt (InData AS STRING) AS STRING
    FOR i = 1 TO LEN(InData)

        X$ = MID$(InData, i, 1)
        Y% = ASC(X$) + 1
        BU$ = BU$ + CHR$(Y%)
    NEXT
    szDecrypt = BU$
END FUNCTION

FUNCTION szDOSDriveLetter (MountPath AS STRING) AS STRING
    szDOSDriveLetter = "" ' ProfileRead$("C:\XOS\USER\CONFIG\DRIVES", "DriveMappings", MountPath)
END FUNCTION

FUNCTION szEncrypt (InData AS STRING) AS STRING

    FOR i = 1 TO LEN(InData)

        X$ = MID$(InData, i, 1)
        Y% = ASC(X$) - 1
        BU$ = BU$ + CHR$(Y%)
    NEXT

    szEncrypt = BU$

END FUNCTION

FUNCTION szGetRootDir (Path AS STRING, File AS STRING) AS STRING
    IF RIGHT$(Path, 1) = "\" THEN
        szGetRootDir = Path + File
    ELSE
        szGetRootDir = Path + "\" + File
    END IF
END FUNCTION

DEFINT A-Z
FUNCTION szKillChar (InText AS STRING, TrimChar AS STRING, ReplaceWith AS STRING) AS STRING

    FOR Iteration = 1 TO LEN(InText)
        'Get one character from the text
        Temp$ = MID$(InText, Iteration, 1)

        'If the character is the same as
        'TrimChar, exclude it. Otherwise,
        'include it in Whole$
        IF NOT UCASE$(Temp$) = UCASE$(TrimChar) THEN
            Whole$ = Whole$ + Temp$
        ELSE
            Whole$ = Whole$ + ReplaceWith
        END IF
    NEXT Iteration

    szKillChar = Whole$
END FUNCTION

FUNCTION szStringLeft (InText AS STRING, CharFilter AS STRING) AS STRING

    IF INSTR(InText, CharFilter) > 0 THEN
        nCharPos% = INSTR(InText, CharFilter)
        Temp$ = MID$(InText, 1, nCharPos%)
        szStringLeft = Temp$
    ELSE
        szStringLeft = InText
    END IF

END FUNCTION

DEFSNG A-Z
FUNCTION szStringRight (InText AS STRING, CharFilter AS STRING) AS STRING

    IF NOT INSTR(InText, CharFilter) > 0 THEN
        szStringRight = InText
        EXIT FUNCTION
    END IF

    'Get the characters
    FOR Iteration% = LEN(InText) TO 1 STEP -1

        Temp$ = MID$(InText, Iteration%, 1)
        
        IF Temp$ = CharFilter THEN
            FOR Iteration2% = LEN(Whole$) TO 1 STEP -1

                Temp$ = MID$(Whole$, Iteration2%, 1)
                Result$ = Result$ + Temp$

            NEXT Iteration2%

            szStringRight = Result$
            EXIT FUNCTION
        ELSE
            Whole$ = Whole$ + Temp$
        END IF
    NEXT Iteration%

    'Reverse the string before returning the result

END FUNCTION

FUNCTION szStrReplace ()
        szStrReplace = 0
END FUNCTION

FUNCTION szTrimText (InText AS STRING) AS STRING
    szTrimText = LTRIM$(RTRIM$(InText))
END FUNCTION

FUNCTION WordMatch (txt AS STRING, pattern AS STRING, match AS STRING) AS INTEGER
    CONST true = -1
    CONST FALSE = 0
    DIM txtWords() AS STRING
    DIM wordsMatched() AS INTEGER
    DIM patWords() AS STRING
    DIM txtWC AS INTEGER, patWC AS INTEGER
    DIM dMatch AS INTEGER

    DIM i AS INTEGER
    
    txtWC = Split(txt, " ", txtWords())
    patWC = Split(pattern, " ", patWords())

    REDIM wordsMatched(patWC) AS INTEGER

    ' fail match if pattern is longer than text
    IF LEN(pattern) > LEN(txt) THEN
        dMatch = FALSE
        GOTO EndFunc
    END IF

    FOR i = 1 TO patWC
        IF i > txtWC THEN EXIT FOR
        IF patWords(i) = "@" THEN
            wordsMatched(i) = true
            match = szTrimText(txtWords(i))
            GOTO EndLoop
        END IF
        IF patWords(i) = txtWords(i) THEN
            wordsMatched(i) = true
            GOTO EndLoop
        END IF
        IF INSTR(patWords(i), "*") > 0 THEN
            endSearch% = INSTR(patWords(i), "*") - 1
            IF LEFT$(txtWords(i), endSearch%) = LEFT$(patWords(i), endSearch%) THEN
                wordsMatched(i) = true
                GOTO EndLoop
            ELSE
                wordsMatched(i) = FALSE
            END IF
        END IF
EndLoop:
    NEXT i

    FOR i = 1 TO patWC
        IF wordsMatched(i) = FALSE THEN
            dMatch = FALSE
            GOTO EndFunc
        END IF
    NEXT i

    dMatch = true
EndFunc:
    
    WordMatch = dMatch

END FUNCTION

