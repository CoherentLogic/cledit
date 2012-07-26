' ----------------------------------------
' FILE:         NTED.BAS
' DESCRIPTION:  NTED 1.1 MAIN SOURCE FILE
' ORIG. AUTHOR: JOHN WILLIS
' LAST EDIT BY: JOHN WILLIS
' ----------------------------------------
' Copyright Work of John Willis. All Rights Reserved
'
' THIS FILE CONTAINS CONFIDENTIAL INFORMATION
' WHICH IS THE EXCLUSIVE PROPERTY OF JOHN WILLIS.
' ACCESS TO THIS SOURCE CODE IS LIMITED TO CO-
' OPERATIVE DEVELOPERS OF THE XOS PRODUCT.
'
' >>> EDIT HISTORY
'
'
' 12/17/97      Testing for reliability. Program must be able
'               to work on a small, low-end system
'               Changing interface slightly to reflect the XOS Plus
'               Version 10 changes. Commenting code.
'
' 12/18/97      Finishing up the code comments
'
' 07/16/2012    Replace JWSD branding with CLD branding,
'               fix braindead fixed-buffer limitations.
'               Add a save feature (what the hell?).
'               Add the snarf bucket and copy.
'               Read XOS environment variable instead of
'               assuming that XOS is installed in C:\XOS.
'
' 07/17/2012    Add insert and append functions.
'               Add paste function.

'>>> includes
'CLD standard functions
'$INCLUDE: '..\COMMON\WSTDFNCT.BI'
'profile string support
'$INCLUDE: '..\COMMON\PROFILE.BI'
'TRUE/FALSE defs
'$INCLUDE: 'CONSTANT.BI'

'>>> function declares
DECLARE FUNCTION nSearchLinesForText (SearchP AS STRING) AS INTEGER
DECLARE FUNCTION PadLeft (intext AS STRING, padchar AS STRING, count AS INTEGER) AS STRING
DECLARE SUB UpdateScreen ()
DECLARE SUB ExpandBuffer (NumLines AS INTEGER)
DECLARE SUB ShiftLinesDown (FromLine AS INTEGER, NumLines AS INTEGER)

DIM SHARED XOSBase AS STRING            'XOS base install location
DIM SHARED EditBuffer() AS STRING         'the editing buffer which contains
                                          'the lines of the current file.
DIM SHARED CopyBuffer() AS STRING       'the copy buffer which acts as a
                                          'temporary storage space for data
                                          'being moved or copied.
DIM SHARED CopyBufferLines AS INTEGER

COMMON SHARED CLine AS INTEGER              'the current editor line
COMMON SHARED StartLine AS INTEGER          'the starting line for the
                                            'current operation

COMMON SHARED EndLine AS INTEGER            'the ending line for the
                                            'current operation

COMMON SHARED CCommand AS STRING            'the name of the current
                                            'operation

COMMON SHARED RawCommand AS STRING          'the whole command, with parms
COMMON SHARED AllParams AS STRING           'the command parameters

COMMON SHARED InFile AS STRING              'the input filename
COMMON SHARED NewFile AS INTEGER            'TRUE/FALSE (is this a new file?)
COMMON SHARED LineIndex AS INTEGER          'line currently being read from
                                            'InFile$
COMMON SHARED LineCount AS INTEGER          'total number of lines in buffer

COMMON SHARED StartDisplay AS INTEGER
StartDisplay = 1

DIM i AS INTEGER
ON ERROR RESUME NEXT
XOSBase = ENVIRON$("XOS")
CLS

                                            'check if a file was specified
IF COMMAND$ = "" THEN                       '(if filename not specified)
    PRINT "File name must be specified"     '-print message
    PRINT                                   '-print blank line
    END                                     '-terminate editor
ELSE                                        '(if filename WAS specified)
    InFile = COMMAND$                       '-use command line arguments
                                            ' for input filename
                                            '-check if the file already exists
    IF nExists(InFile) = TRUE THEN          '(if file exists)
        NewFile = FALSE                     '-set new file flag to FALSE
    ELSE                                    '(if file does not exist)
        NewFile = TRUE                      '-set new file flag to TRUE
    END IF
END IF

IF NewFile = TRUE THEN                      '(if new file flag is true)
    '                                       '
    'TODO: add code for making a new file   '
    '                                       '
ELSE                                        '(if new file flag is false)
                                            '-start loading file
    LineIndex = 0                           '-set current line to 0 (BOF)
    OPEN InFile FOR INPUT AS #1             '-open the input file for reading
    DO WHILE NOT EOF(1)                     '-loop until EOF reached on file 1
        LineIndex = LineIndex + 1           '-increment line number by 1
        
        REDIM PRESERVE EditBuffer(LineIndex) AS STRING

        LINE INPUT #1, EditBuffer(LineIndex)'-read one line into the buffer
    LOOP
    CLOSE #1                                '-close the input file
    LineCount = LineIndex                   '-set the linecount to lineindex
END IF

CLine = 1                                   '-set current line to 1
PromptArea:                                 '-label for returning to prompt

UpdateScreen
COLOR 13
VIEW PRINT 24 TO 24
LOCATE 24, 1: COLOR 13: PRINT "*"; : COLOR 7, 0: LINE INPUT " "; RawCommand
VIEW PRINT 1 TO 23: LOCATE 1, 1

                                            ' putting whole command in
                                            ' RawCommand
IF RawCommand = "q" THEN                    '(if command is q for quit)
    END                                     '-terminate editor
ELSEIF RawCommand = "v" THEN                '(if command is v for view)
    FOR i = 1 TO LineCount                  '-loop for the number of lines
                                            ' in the edit buffer
        PRINT i; ": "; EditBuffer(i)        '-display the line # with line
    NEXT
    GOTO PromptArea                         '-return to prompt
ELSEIF RawCommand = "s" THEN
    OPEN InFile FOR OUTPUT AS #1

    FOR i = 1 TO LineCount
        PRINT #1, EditBuffer(i)
    NEXT i

    CLOSE #1

    GOTO PromptArea
ELSEIF RawCommand = "show snarf" THEN
    FOR i = 0 TO CopyBufferLines - 1
        PRINT LTRIM$(RTRIM$(STR$(i + 1))); ":  "; CopyBuffer(i)
    NEXT
    GOTO PromptArea
ELSEIF LEFT$(RawCommand, 5) = "paste" THEN
    pasteTarget% = VAL(MID$(RawCommand, 7))

    ShiftLinesDown pasteTarget%, CopyBufferLines

    FOR i = 0 TO CopyBufferLines - 1
        EditBuffer(pasteTarget%) = CopyBuffer(i)
        pasteTarget% = pasteTarget% + 1
    NEXT

    GOTO PromptArea
ELSEIF RawCommand = "a" OR RawCommand = "append" THEN
    PRINT "Enter the lines you would like to append."
    PRINT "When you are finished, enter a period on a line by itself."
    DO
        LineCount = LineCount + 1
        REDIM PRESERVE EditBuffer(LineCount) AS STRING
        PRINT szTrimText(STR$(LineCount)); ":  ";
        LINE INPUT "", EditBuffer(LineCount)
    LOOP UNTIL EditBuffer(LineCount) = "."
    LineCount = LineCount - 1
    REDIM PRESERVE EditBuffer(LineCount) AS STRING
    GOTO PromptArea
END IF
                                            '-parse the raw input
IF INSTR(RawCommand$, " ") > 0 THEN         '(if space exists in command)
                                            '-set line range (ladd$) to
                                            ' the part of the command which
                                            ' comes before the space
    LAdd$ = LEFT$(RawCommand$, INSTR(RawCommand$, " ") - 1)
ELSE                                        '(if space does NOT exist)
    PRINT "Invalid command format"          '-display an error message
END IF

IF LEN(LAdd$) = 1 THEN                      '(if the line range is one char)
    SELECT CASE LAdd$                       '-choose options from the range
        CASE "%"                            '(if the range is % (%=all lines))
            StartLine = 1                   '-set start line to 1
            EndLine = LineCount             '-set end line to last line
        CASE "d"                            '(if the command is for DOS shell)
            SHELL                           '-open a DOS prompt
        CASE ELSE                           '(if the command is none above)
    IF NOT INSTR(LAdd$, ",") > 0 THEN       ' (if the range isn't start,end)
            StartLine = VAL(LAdd$)          ' -set startline and endline
            EndLine = VAL(LAdd$)            '  to the same line (ladd$)
            GOTO DoCommand                  ' -jump to command processor
    END IF

    END SELECT
ELSE                                        '(if the range is > 1 char)
    IF NOT INSTR(LAdd$, ",") > 0 THEN       ' (if the range isn't start,end)
            StartLine = VAL(LAdd$)          ' -set startline and endline
            EndLine = VAL(LAdd$)            '  to the same line (ladd$)
            GOTO DoCommand                  ' -jump to command processor
    END IF
    LeftOfC$ = LEFT$(LAdd$, INSTR(LAdd$, ",") - 1) 'parse input so that
                                                   'the leftofc$ var contains
                                                   'text to left of comma
    RightOfC$ = MID$(LAdd$, INSTR(LAdd$, ",") + 1) 'same for the post-comma
    SELECT CASE LeftOfC$                    '-obtain start line from LeftOfC$
        CASE "."                            '(if LeftOfC is . for current line)
            StartLine = CLine               '-set startline to current line
        CASE "f"                            '(if leftofc is f for first line
            StartLine = 1                   '-set startline to 1
        CASE "m"                            '(if leftofc is m for mid line)
            StartLine = LineCount / 2       '-set startline to mid of buffer
        CASE ELSE                           '(if none of the above are true)
            IF LEFT$(LeftOfC$, 1) = "/" THEN '(if a text-search argument)
                                             '-set search data to
                                             ' portion of parm left of "/"
                SearchParm$ = MID$(LeftOfC$, 2, INSTR(2, LeftOfC$, "/") - 2)
                                            '-obtain line by searcing with
                                            ' nSearchLinesForText
                StartLine = nSearchLinesForText(SearchParm$)
            ELSE                            '(is NOT a text-search argument)
                StartLine = VAL(LeftOfC$)   'assume startline is a literal
                                            'line number
            END IF

    END SELECT
                                            '(comments on lines 150-171
                                            ' apply to most of lines 177-196)
    SELECT CASE RightOfC$
        CASE "."
            EndLine = CLine
        CASE "f"
            EndLine = 1
        CASE "m"
            EndLine = LineCount / 2
        CASE "$"
            EndLine = LineCount
        CASE ELSE
            IF LEFT$(RightOfC$, 1) = "/" THEN
                SearchParm$ = MID$(RightOfC$, 2, INSTR(2, RightOfC$, "/") - 2)
                EndLine = nSearchLinesForText(SearchParm$)
            ELSEIF LEFT$(RightOfC$, 1) = "+" THEN
                EndLine = StartLine + VAL(MID$(RightOfC$, 2))

            ELSE
                EndLine = VAL(RightOfC$)
            END IF
    END SELECT
END IF
DoCommand:                          
CCommand = MID$(RawCommand, INSTR(RawCommand, " ") + 1)
'PRINT "ccomand:"; CCommand
IF INSTR(CCommand, ":") > 0 THEN
                                            'obtain operation to perform
                                            'on lines startline to
                                            'endline
    CCommand = LEFT$(CCommand, INSTR(CCommand, ":") - 1)
                                            'obtain parameters to operation
    AllParams = MID$(RawCommand, INSTR(RawCommand, ":") + 1)
END IF

FOR CLine = StartLine TO EndLine            'execute command on lines
                                            'startline-endline,
                                            'where CLine=current line
                                            '(for new commands)
    SELECT CASE CCommand                    '-choose command
        CASE "i", "insert"

        CASE "plugin", "user", "filter"     '(command is one of the following:
                                            ' plugin, user, or filter)
                                            '-open a file to use as temp.
                                            ' storage for lines being changed
                                            ' by an external filter
            OPEN XOSBase + "\LINEDATA.TMP" FOR OUTPUT AS #5
            PRINT #5, EditBuffer(CLine)     '-put the current line
                                            ' in the temporary file
            CLOSE #5                        '-close the temp. file
            SHELL XOSBase + "\PLUGIN\" + AllParams '-run the specified filter
                                               ' on the temp. file
                                            '-reopen the temp. file
                                            ' but read it. It should
                                            ' now contain the data
                                            ' as modified by the filter
            OPEN XOSBase + "\LINEDATA.TMP" FOR INPUT AS #5
            LINE INPUT #5, EditBuffer(CLine)'-obtain modified data
                                            ' from the temp. file
            CLOSE #5                        '-close the temp. file for good
        CASE "ln"                           '(cmd=ln for obtaining line #s
                                            ' usually used with /text/
                                            ' start/end ranges to display
                                            ' the actual line #s)
            IF CLine = EndLine THEN         '(if current line is last line)
                PRINT LTRIM$(RTRIM$(STR$(StartLine))); "-"; LTRIM$(RTRIM$(STR$(EndLine)))   '-display the range
            END IF
        CASE "p", "print"    '(cmd is for displaying the range)
                                            '-display the cur. line # w/line
            PRINT szTrimText(STR$(CLine)); ":  "; EditBuffer(CLine)
        CASE "ab", "add-begin", "prepend"   '(cmd adds its params to start of
                                            ' each line in range)
            EditBuffer(CLine) = AllParams + EditBuffer(CLine)
        CASE "nl", "numerate-lines"         '(add line #s to lines in range)
            EditBuffer(CLine) = szTrimText(STR$(CLine)) + " " + EditBuffer(CLine)
        CASE "ll", "linelook", "linesearch" '(search range for occurences
                                            ' of text contained in params)
                                            '-set FPos% to first occurence
                                            ' of AllParams in current line
            FPos% = INSTR(EditBuffer(CLine), AllParams)
            IF FPos% > 0 THEN               '(if the string exists)
                                            '-display the line # w/line
                PRINT szTrimText(STR$(CLine)); ":  "; LEFT$(EditBuffer(CLine), FPos% - 1);
                COLOR 15                    '-set color to bright white
                PRINT AllParams;            '-display the search string
                COLOR 7                     '-set color to lt. gray
                PRINT MID$(EditBuffer(CLine), FPos% + LEN(AllParams))

            END IF
        CASE "grab"
            'do this stuff once, at the beginning
            IF CLine = StartLine THEN
                CopyBufferLines = (EndLine - StartLine) + 1
                REDIM PRESERVE CopyBuffer(CopyBufferLines) AS STRING
                CurrentBufIndex = 0
                CopyBuffer(CurrentBufIndex) = EditBuffer(CLine)
                CurrentBufIndex = CurrentBufIndex + 1
            'do this stuff for every other line
            ELSEIF CLine > StartLine AND CLine < EndLine THEN
                CopyBuffer(CurrentBufIndex) = EditBuffer(CLine)
                CurrentBufIndex = CurrentBufIndex + 1
            ELSEIF CLine = EndLine THEN
                CopyBuffer(CurrentBufIndex) = EditBuffer(CLine)
            END IF

            IF CLine = EndLine THEN
                PRINT "Grabbed "; LTRIM$(RTRIM$(STR$(CopyBufferLines))); " lines into the snarf bucket."
            END IF
        CASE "c", "change", "replace"       '(if command is find/replace)
                                            '-set search text to the
                                            ' part of AllParams which
                                            ' falls before the space
            SearchText$ = LEFT$(AllParams, INSTR(AllParams, " ") - 1)
                                            '-set the replacement text to the
                                            ' part of AllParams which falls
                                            ' AFTER the space
            RepText$ = MID$(AllParams, INSTR(AllParams, " ") + 1)
            
            SearchData$ = SearchText$
            ReplaceWith$ = RepText$
Research:                                   'label to jump to to look for the
                                            'next occurence in the line
                                            '(if the search data doesn't
                                            ' exist)
            IF INSTR(EditBuffer(CLine), SearchData$) = 0 THEN
                IF CLine >= EndLine THEN    '-(if current line is last line)
                    GOTO PromptArea         ' -return to command prompt
                END IF
                CLine = CLine + 1           '-otherwise, increment the
                                            ' current line
                GOTO Research               '-re search the current line
                                            ' for the string
            END IF

                                            '-append a tilde to the line
                                            ' as an EOL character
            InputData$ = EditBuffer(CLine) + " ~"

            
            fIndex% = 0                     '-set the find index to 0
            FOR i = 1 TO LEN(InputData$)    '-loop for every char in
                                            ' input data

                                            '-get the next character
                CChar$ = MID$(InputData$, i, 1)

                'If it is a space, make a word out of what is already there
                IF CChar$ = " " THEN
                    NextWord$ = BuildUp$
                IF NextWord$ = SearchData$ THEN  '-found an occurence of the data
                    fIndex% = fIndex% + 1   '-increment the find index
                    'PRINT "Found occurence"; fIndex%; "of the data."
                                            '-determine the pos. of the string
                    FoundPos% = INSTR(InputData$, SearchData$)
                                            '-determine length of the string
                    WordLen% = LEN(SearchData$)
                                            '-get the text to the left of
                                            ' the string we just found...
                    LeftText$ = LEFT$(InputData$, FoundPos% - 1)
                                            ' ...and the text to the right
                                            ' of the string we just found
                    RightText$ = MID$(InputData$, FoundPos% + WordLen% + 1)
                                            '-put the replacement text
                                            ' after the text to left of
                                            ' the search string and before
                                            ' the text to the right of it
                    InputData$ = LeftText$ + ReplaceWith$ + " " + RightText$
                    
                                            '(if no more occurences exist)
                    IF NOT INSTR(InputData$, SearchData$) > 0 THEN
                                            '-remove the tilde
                        EditBuffer(CLine) = LEFT$(InputData$, LEN(InputData$) - 2)

                        InputData$ = ""     '-clear the input data
                                            '(if the last line was reached)
                        IF CLine = EndLine THEN
                            GOTO PromptArea '-return to NTED prompt
                        ELSE                '(otherwise...)
                            CLine = CLine + 1 '-increment line number
                            GOTO Research     '-search the line once more
                        END IF
                    ELSE                    '(otherwise...)
                                            '-remove the tilde
                        EditBuffer(CLine) = LEFT$(InputData$, LEN(InputData$) - 2)
                        GOTO Research       '-and search the line once more
                    END IF
                    
                                            '-this line has no real function
                                            ' except to alleviate dependency
                                            ' problems
                    NextPos% = NextPos% + FoundPos% + WordLen%
                END IF
                BuildUp$ = ""               '-clear characters, parse next word
            ELSE                            '(otherwise...)
                BuildUp$ = BuildUp$ + CChar$'-continue building current word
            END IF
        NEXT


        CASE "w"                            '(if cmd is w for write, save)
            IF AllParams <> "" THEN         '-(if parameters exist)
                OutFile$ = AllParams        ' -set output filename to params
            ELSE                            '(otherwise...)
                OutFile$ = InFile           '-set output filename = input filename
            END IF
            OPEN OutFile$ FOR APPEND AS #1  '-open the output file for appending
            PRINT #1, EditBuffer(CLine)     '-append current line to output file
            CLOSE #1                        '-close the output file
        CASE "e", "edit"                    '(if cmd is e or edit for edit line)
                                            '-display line # with the cur. line
            PRINT szTrimText(STR$(CLine)); ":  "; EditBuffer(CLine)
                                            '-display the "edit #:" prompt
            PRINT szTrimText(STR$(CLine)); ":  ";
                                            '-read new line from input into
                                            ' current line buffer
            LINE INPUT ""; EditBuffer(CLine)
    END SELECT
NEXT


GOTO PromptArea                             '-return to command prompt
                                            '-end of source file
'>>> END OF MODULE NTED.BAS <<<'

SUB ExpandBuffer (NumLines AS INTEGER)
    
    LineCount = LineCount + NumLines
    REDIM PRESERVE EditBuffer(LineCount) AS STRING

END SUB

FUNCTION nSearchLinesForText (SearchP AS STRING) AS INTEGER
    FOR i = 1 TO 5000
        IF INSTR(EditBuffer(i), SearchP) > 0 THEN
            nSearchLinesForText = i
            EXIT FUNCTION
        END IF
    NEXT
END FUNCTION

FUNCTION PadLeft (intext AS STRING, padchar AS STRING, count AS INTEGER) AS STRING
    DIM itl AS INTEGER
    DIM padsize AS INTEGER
    itl = LEN(intext)
    padsize = count - itl
    PadLeft = STRING$(padsize, padchar) + intext
END FUNCTION

SUB ShiftLinesDown (FromLine AS INTEGER, NumLines AS INTEGER)
    DIM i AS INTEGER
    DIM OriginalBufferSize AS INTEGER

    DIM ShiftBuffer() AS STRING
    DIM ShiftBufSize AS INTEGER
    DIM NewLocLine AS INTEGER
    ShiftBufSize = LineCount - FromLine + 1

    REDIM ShiftBuffer(ShiftBufSize) AS STRING
    DIM bufIdx AS INTEGER
    bufIdx = 1

    FOR i = FromLine TO LineCount
        ShiftBuffer(bufIdx) = EditBuffer(i)
        EditBuffer(i) = ""
        bufIdx = bufIdx + 1
    NEXT i

    'FOR i = 1 TO ShiftBufSize
    '    PRINT szTrimText(STR$(i)); ":  "; ShiftBuffer(i)
    'NEXT i

    OriginalBufferSize = LineCount
    ExpandBuffer NumLines

    NewLocLine = FromLine + NumLines

    bufIdx = 1
    FOR i = NewLocLine TO LineCount
        EditBuffer(i) = ShiftBuffer(bufIdx)
        bufIdx = bufIdx + 1
    NEXT i

END SUB

SUB ShrinkBuffer (NumLines AS INTEGER)
    LineCount = LineCount - NumLines
    REDIM PRESERVE EditBuffer(LineCount) AS STRING
END SUB

SUB UpdateScreen ()
    VIEW PRINT 25 TO 25
    COLOR 15, 1
    CLS
    LOCATE 25, 1
    PRINT InFile$; " "; CHR$(179); " EDIT: "; szTrimText(STR$(LineCount)); "L ";
    PRINT CHR$(179); " COPY: "; szTrimText(STR$(CopyBufferLines)); "L ";
    PRINT CHR$(179); " DISPLAY: "; szTrimText(STR$(StartDisplay)); "-";
    PRINT szTrimText(STR$(StartDisplay + 22));
    VIEW PRINT 1 TO 24: COLOR 7, 0

    FOR i = StartDisplay TO StartDisplay + 22
        PRINT STRING$(80, " ");
        LOCATE CSRLIN - 1, 1
        IF i <= LineCount THEN COLOR 10, 0 ELSE COLOR 12, 0

        PRINT PadLeft(szTrimText(STR$(i)), "0", 5); ":  ";
        COLOR 7, 0
        IF i <= LineCount THEN PRINT EditBuffer(i) ELSE PRINT ""

    NEXT
    
END SUB

