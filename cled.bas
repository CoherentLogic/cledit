'
' cled.bas
'  Coherent Logic Editor
'
' Copyright (C) 1997, 2012
'  Coherent Logic Development LLC
'
'


#lang "fblite"

#define CLED_VERSION 0.02

#ifdef __FB_DOS__
 #include "common\wstdfnct.bi"
 #include "common\constant.bi"
 
 #define C_VERT_BAR CHR$(179)
 #define C_LINE_CONT CHR$(16)

 #define CLED_PLATFORM "i386/msdos/djgpp"
#endif

#ifdef __FB_WIN32__
 #include "common\wstdfnct.bi"
 #include "command\constant.bi"

 #define C_VERT_BAR "|"
 #define C_LINE_CONT "+"

 #define CLED_PLATFORM "i386/win32/mingw"
#endif

#ifdef __FB_LINUX__
 #include "common/wstdfnct.bi"
 #include "common/constant.bi"

 #define C_VERT_BAR "|"
 #define C_LINE_CONT "+"

 #define CLED_PLATFORM "i386/linux/gcc"
#endif


'>>> function declares
DECLARE FUNCTION nSearchLinesForText (SearchP AS STRING) AS INTEGER
DECLARE FUNCTION Split (txt AS STRING, Delim AS STRING, WordArray() AS STRING) AS INTEGER
DECLARE SUB ShiftLinesUp (FromLine AS INTEGER, NumLines AS INTEGER)
DECLARE SUB ShrinkBuffer (NumLines AS INTEGER)
DECLARE SUB JoinLines (SourceLine AS INTEGER, TargetLine AS INTEGER)
DECLARE SUB BreakLine (BLine AS INTEGER, BCol AS INTEGER)
DECLARE SUB DelCharLeft (DLine AS INTEGER, DCol AS INTEGER)
DECLARE SUB InsertChar (ILine AS INTEGER, ICol AS INTEGER, IChar AS STRING)
DECLARE SUB VisualMode ()
DECLARE FUNCTION PadLeft (intext AS STRING, padchar AS STRING, count AS INTEGER) AS STRING
DECLARE SUB UpdateScreen ()
DECLARE SUB ExpandBuffer (NumLines AS INTEGER)
DECLARE SUB ShiftLinesDown (FromLine AS INTEGER, NumLines AS INTEGER)
DECLARE SUB IndexJump ()
DECLARE FUNCTION WordMatch (txt AS STRING, pattern AS STRING, match AS STRING) AS INTEGER
DECLARE SUB ProgressBar (X AS INTEGER, Y AS INTEGER, ForeColor AS INTEGER, BackColor AS INTEGER, Value AS INTEGER)
DECLARE SUB ApplyXRefRules (Silent AS INTEGER)
DECLARE SUB CheckIndexing ()
DECLARE FUNCTION FileExtension (txt AS STRING) AS STRING
DECLARE FUNCTION FileType (txt AS STRING) AS STRING
DECLARE FUNCTION FirstNonWhitespace (txt AS STRING) AS INTEGER
DECLARE SUB WaitOutput ()
DECLARE SUB ExitCLED ()
DECLARE SUB SetTabs ()
DECLARE FUNCTION InSelection (l AS INTEGER, c AS INTEGER) AS INTEGER
DECLARE SUB Message (MTxt AS STRING)
DECLARE SUB DelCharRight (DLine AS INTEGER, DCol AS INTEGER)
DECLARE SUB HelpCommandMode ()
DECLARE SUB HelpVisualMode ()
DECLARE SUB HelpInit ()
DECLARE SUB Help ()
DECLARE SUB Cut ()
DECLARE SUB Copy ()
DECLARE SUB Paste ()
DECLARE SUB Find ()
DECLARE SUB FindReplace ()
declare sub updateConsole() 
declare sub rulerLine()


type ConsoleTag
    Width as integer
    Height as integer
    StatusLine as integer
    EditTop as integer
    EditBottom as integer
    MessageLine as integer
    RulerLine as integer
    EditLeft as integer
    EditRight as integer
    MaxLines as integer
    MaxLineWidth as integer
    CommandModeLine as integer
end type


TYPE SelectionBlock
    StartLine AS INTEGER
    StartCol AS INTEGER
    EndLine AS INTEGER
    EndCol AS INTEGER
END TYPE

TYPE XRefRule
    RuleName AS STRING * 30
    RuleCode AS STRING * 30
END TYPE

TYPE IndexEntryTag
    IndexName AS STRING * 30
    MatchName AS STRING * 30
    LineNumber AS INTEGER
END TYPE

dim shared Cons as ConsoleTag

DIM SHARED IndexEntries() AS IndexEntryTag
DIM SHARED indexEntryCount AS INTEGER

DIM SHARED XRefRules() AS XRefRule
DIM SHARED XRefRuleCount AS INTEGER

DIM SHARED LastLocation AS INTEGER

DIM SHARED EditFileType AS STRING

COMMON SHARED VLine AS INTEGER
COMMON SHARED VCol AS INTEGER
COMMON SHARED LastVLine AS INTEGER
COMMON SHARED LastVCol AS INTEGER
COMMON SHARED BufferLine AS INTEGER

COMMON SHARED SelBlock AS SelectionBlock
COMMON SHARED SelActive AS INTEGER

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
COMMON SHARED TabStops AS INTEGER



TabStops = 4
StartDisplay = 1
SelActive = FALSE
indexEntryCount = 0

DIM i AS INTEGER
ON ERROR GOTO ErrHandler
XOSBase = ENVIRON$("XOS")
CLS

VCol = 1
VLine = 1
BufferLine = 1

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
    LineCount = 1
    StartDisplay = 1
    REDIM PRESERVE EditBuffer(LineCount) AS STRING
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
    IF INSTR(InFile, ".") <> 0 THEN
        bak$ = LEFT$(InFile, INSTR(InFile, ".") - 1)
        bak$ = bak$ + ".CBK"
    ELSE
        bak$ = bak$ + ".CBK"
    END IF
    
    OPEN bak$ FOR OUTPUT AS #2
    FOR i = 1 TO LineCount
        PRINT #2, EditBuffer(i)
    NEXT i
    CLOSE #2


    EditFileType = FileType(InFile)
    IF EditFileType = "" THEN EditFileType = "ASCII"
    CheckIndexing

END IF

CLine = 1                                   '-set current line to 1
PromptArea:                                 '-label for returning to prompt

UpdateScreen
VisualMode
VIEW PRINT Cons.CommandModeLine TO Cons.CommandModeLine
COLOR 15, 1: CLS
PRINT "COMMAND MODE";
VIEW PRINT Cons.CommandModeLine + 1 TO Cons.MessageLine - 1
COLOR 7, 0
CLS

LOCATE Cons.CommandModeLine, 1:  LINE INPUT "* ", RawCommand

                                            ' putting whole command in
                                            ' RawCommand
if RawCommand = "q" then                    '(if command is q for quit)
    ExitCLED                                     '-terminate editor
elseif RawCommand = "version" then
     print "CLED Version "; CLED_VERSION; "   (";CLED_PLATFORM;" ";__FB_SIGNATURE__;")"
     print " Copyright (C) 2012 Coherent Logic Development LLC"
     print ""
     print ""
     print "Licensed Work. Program property of Coherent Logic Development LLC."
     print "Unauthorized usage, distribution, reverse engineering, decompiling,"
     print "or modification of this software program is strictly prohibited"
     print "and will be prosecuted to the fullest extent of the law."
     print ""
     print "See the file LICENSE.TXT, included with this distribution, for details"
     print "pertaining to the terms of legal usage for this software."
     WaitOutput
elseif RawCommand = "v" then                '(if command is v for view)
    for i = 1 TO LineCount                  '-loop for the number of lines
                                            ' in the edit buffer
        print i; ": "; EditBuffer(i)        '-display the line # with line
    next
    goto PromptArea                         '-return to prompt
elseif RawCommand = "s" THEN
    open InFile for output as #1

    for i = 1 TO LineCount
        print #1, EditBuffer(i)
    next i

    close #1

    goto PromptArea
elseif left(RawCommand, 4) = "page" THEN
    pageCmd$ = MID$(RawCommand, 6)
    SELECT CASE pageCmd$
        CASE "up"
            StartDisplay = StartDisplay - Cons.MaxLines
            IF StartDisplay < 1 THEN
                view print Cons.Height to Cons.Height
                locate Cons.Height, 1: COLOR 12, 0
                PRINT "ALREADY AT TOP OF FILE";
                BEEP
                SLEEP 1
                LOCATE Cons.Height, 1
                PRINT "                      ";
                StartDisplay = 1
            END IF
        CASE "down"
            StartDisplay = StartDisplay + Cons.MaxLines
        CASE "top"
            StartDisplay = 1
        CASE "bottom"
            StartDisplay = LineCount - Cons.MaxLines - 1
    END SELECT
    GOTO PromptArea
ELSEIF LEFT$(RawCommand, 4) = "line" THEN
    lineCmd$ = MID$(RawCommand, 6)
    SELECT CASE lineCmd$
        CASE "up"
            StartDisplay = StartDisplay - 1
        CASE "down"
            StartDisplay = StartDisplay + 1
    END SELECT
    GOTO PromptArea
ELSEIF RawCommand = "show copy" THEN
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
ELSEIF RawCommand = "visual" THEN
    VisualMode
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

        CASE "="     '(command is one of the following:
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
        CASE "range"                           '(cmd=ln for obtaining line #s
                                            ' usually used with /text/
                                            ' start/end ranges to display
                                            ' the actual line #s)
            IF CLine = EndLine THEN         '(if current line is last line)
                Message LTRIM$(RTRIM$(STR$(StartLine))) + "-" + LTRIM$(RTRIM$(STR$(EndLine))) + " (" + szTrimText(STR$((EndLine - StartLine) + 1)) + ")"
                
            END IF

        CASE "p", "print"    '(cmd is for displaying the range)
                                            '-display the cur. line # w/line
            PRINT szTrimText(STR$(CLine)); ":  "; EditBuffer(CLine)
            IF CLine = EndLine THEN
                WaitOutput
            END IF

        CASE "pre", "prepend"   '(cmd adds its params to start of
                                            ' each line in range)
            EditBuffer(CLine) = AllParams + EditBuffer(CLine)
        CASE "nl", "numerate-lines"         '(add line #s to lines in range)
            EditBuffer(CLine) = szTrimText(STR$(CLine)) + " " + EditBuffer(CLine)
        CASE "ls", "linesearch" '(search range for occurences
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

            IF CLine = EndLine THEN WaitOutput
        CASE "c", "co", "copy"
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
                Message "Copied " + LTRIM$(RTRIM$(STR$(CopyBufferLines))) + " lines"
            END IF
        CASE "r", "replace"       '(if command is find/replace)
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


        CASE "w", "write"                           '(if cmd is w for write, save)
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
        CASE "g", "go"
            StartDisplay = CLine
    END SELECT
NEXT



GOTO PromptArea                             '-return to command prompt
                                            '-end of source file
END

ErrHandler:
'    VIEW PRINT 1 TO 25
'    COLOR 12, 0
'    PRINT "ERROR: "; ERROR$(ERR)
'    SLEEP
    RESUME NEXT

SUB ApplyXRefRules (Silent AS INTEGER)
    DIM i AS INTEGER, j AS INTEGER

    DIM RuleWords() AS STRING
    DIM RuleWordCount AS INTEGER
    DIM pctDone AS INTEGER
    DIM ctxt AS STRING
    DIM pbLoc AS INTEGER

    DIM dMatchName AS STRING

    DIM LineWords() AS STRING
    DIM LineWordCount AS INTEGER

    IF Silent = FALSE THEN
        COLOR 15, 1: CLS
        pbLoc = (80 / 2) - (50 / 2)
    END IF

    indexEntryCount = 0
    REDIM IndexEntries(indexEntryCount) AS IndexEntryTag
   
    FOR i = 1 TO XRefRuleCount
        FOR j = 1 TO LineCount

            IF Silent = FALSE THEN
                ctxt = "Applying cross-reference rule " + RTRIM$(XRefRules(i).RuleName)
                COLOR 15, 1
                
                LOCATE 12, 1
                PRINT STRING$(80, " ");

                LOCATE 12, (80 / 2) - (LEN(ctxt) / 2): PRINT ctxt;

                pctDone = nFigurePercent(CDBL(j), CDBL(LineCount))
                ProgressBar pbLoc, 15, 15, 1, pctDone
            END IF

            IF WordMatch(szTrimText(EditBuffer(j)), szTrimText(XRefRules(i).RuleCode), dMatchName) = TRUE THEN
                indexEntryCount = indexEntryCount + 1
                REDIM PRESERVE IndexEntries(indexEntryCount) AS IndexEntryTag

                IndexEntries(indexEntryCount).IndexName = XRefRules(i).RuleName
                IndexEntries(indexEntryCount).MatchName = dMatchName
                IndexEntries(indexEntryCount).LineNumber = j
            END IF
        NEXT
    NEXT i

    IF Silent = FALSE THEN
        COLOR 7, 0
        CLS
    END IF

END SUB

SUB BreakLine (BLine AS INTEGER, BCol AS INTEGER)
    DIM tmp AS STRING
    DIM postBrk AS STRING
    DIM preBrk AS STRING

    tmp = EditBuffer(BLine)

    preBrk = LEFT$(EditBuffer(BLine), BCol - 1)
    postBrk = MID$(EditBuffer(BLine), BCol)

    ShiftLinesDown BLine + 1, 1

    EditBuffer(BLine + 1) = postBrk
    EditBuffer(BLine) = preBrk
    
END SUB

SUB CheckIndexing ()
    DIM cexFile AS STRING
    DIM fileNum AS INTEGER
    DIM identStr AS STRING
    DIM fType AS STRING
    DIM RuleCount AS INTEGER
    DIM tmpRule AS STRING

    cexFile = XOSBase + "\" + FileExtension(InFile) + ".CEX"

    IF nExists(cexFile) THEN
        fileNum = FREEFILE
        OPEN cexFile FOR INPUT AS #fileNum
        LINE INPUT #fileNum, identStr
        fType = LEFT$(identStr, INSTR(identStr, ":") - 1)
        RuleCount = VAL(MID$(identStr, INSTR(identStr, ":") + 1))
    END IF

    IF RuleCount > 0 THEN
        XRefRuleCount = RuleCount

        REDIM XRefRules(XRefRuleCount) AS XRefRule

        FOR i = 1 TO RuleCount
            LINE INPUT #fileNum, tmpRule
            XRefRules(i).RuleName = LEFT$(tmpRule, INSTR(tmpRule, ":") - 1)
            XRefRules(i).RuleCode = MID$(tmpRule, INSTR(tmpRule, ":") + 1)
        NEXT i
        
        ApplyXRefRules FALSE
    END IF

END SUB

SUB Copy ()

END SUB

SUB Cut ()

END SUB

SUB DelCharLeft (DLine AS INTEGER, DCol AS INTEGER)
    DIM tmp AS STRING

    ' get the line starting at the insertion point
    tmp = MID$(EditBuffer(DLine), DCol)
    
    
    'MID$(EditBuffer(DLine), DCol - 1, 1) = " "
    MID$(EditBuffer(DLine), DCol - 1) = tmp
    'MID$(EditBuffer(DLine), LEN(EditBuffer(DLine)), 1) = " "
    EditBuffer(DLine) = LEFT$(EditBuffer(DLine), LEN(EditBuffer(DLine)) - 1)

END SUB

SUB DelCharRight (DLine AS INTEGER, DCol AS INTEGER)
    DIM tmp AS STRING

    tmp = MID$(EditBuffer(DLine), DCol + 1)

    MID$(EditBuffer(DLine), DCol) = tmp

    MID$(EditBuffer(DLine), LEN(EditBuffer(DLine)), 1) = " "
    EditBuffer(DLine) = RTRIM$(EditBuffer(DLine))

END SUB

SUB ExitCLED ()
    CLS
    END
END SUB

SUB ExpandBuffer (NumLines AS INTEGER)
    
    LineCount = LineCount + NumLines
    REDIM PRESERVE EditBuffer(LineCount) AS STRING

END SUB

FUNCTION FileExtension (txt AS STRING) AS STRING
    DIM i AS INTEGER
    DIM dotLocation AS INTEGER

    FOR i = LEN(txt) TO 1 STEP -1
        IF MID$(txt, i, 1) = "." THEN
            dotLocation = i
            EXIT FOR
        END IF
    NEXT i
    FileExtension = MID$(txt, i + 1)
END FUNCTION

FUNCTION FileType (txt AS STRING) AS STRING
    DIM cexFile AS STRING
    DIM fileNum AS INTEGER
    DIM identStr AS STRING
    DIM fType AS STRING
    DIM RuleCount AS INTEGER

    cexFile = XOSBase + "\" + FileExtension(txt) + ".CEX"
    
    IF nExists(cexFile) THEN
        fileNum = FREEFILE
        OPEN cexFile FOR INPUT AS #fileNum
        LINE INPUT #fileNum, identStr
        fType = LEFT$(identStr, INSTR(identStr, ":") - 1)
        RuleCount = VAL(MID$(identStr, INSTR(identStr, ":") + 1))
    END IF

    FileType = fType
END FUNCTION

SUB Find ()

END SUB

SUB FindReplace ()

END SUB

FUNCTION FirstNonWhitespace (txt AS STRING) AS INTEGER
    DIM trimLen AS INTEGER
    DIM origLen AS INTEGER

    trimLen = LEN(LTRIM$(txt))
    origLen = LEN(txt)

    FirstNonWhitespace = origLen - trimLen

END FUNCTION

SUB Help ()
    HelpInit
    PRINT
    PRINT
    PRINT "Get help on..."
    PRINT "  1)    Command Mode"
    PRINT "  2)    Visual Mode"
    PRINT
    PRINT "  ESC)  Exit Help"
    PRINT
    PRINT "  ";

    DO
        i$ = INKEY$

        SELECT CASE i$
            CASE "1"
                HelpInit
                HelpCommandMode
            CASE "2"
                HelpInit
                HelpVisualMode
            CASE CHR$(27)
                EXIT SUB
        END SELECT

    LOOP

END SUB

SUB HelpCommandMode ()

LOCATE 6, 3: PRINT "COMMAND FORMAT"
LOCATE 8, 5: PRINT "n1,n2 command:parameter"
LOCATE 9, 6: PRINT " CLED will apply command:parameter"
LOCATE 10, 6: PRINT " to lines n1 through n2."
LOCATE 12, 5: PRINT "n command:parameter"
LOCATE 13, 6: PRINT " CLED will apply command:parameter"
LOCATE 14, 6: PRINT " to line n only."
LOCATE 16, 3: PRINT "n1 & n2 SHORTHAND"
LOCATE 18, 5: PRINT "SYMB     DESCRIPTION          N N1 N2"
LOCATE 19, 5: PRINT "%        ALL LINES            *  -  -"
LOCATE 20, 5: PRINT "f        FIRST LINE           -  *  *"
LOCATE 21, 5: PRINT "m        MIDDLE LINE          -  *  *"
LOCATE 22, 5: PRINT "/TEXT/   LINE INCLUDING TEXT  -  *  *"
LOCATE 23, 5: PRINT "+L       N1 + L LINES         -  -  *"
LOCATE 24, 5: PRINT "$        LAST LINE            -  -  *"


    
END SUB

SUB HelpInit ()
    VIEW PRINT 1 TO 25
    CLS
    COLOR 15, 0

    PRINT
    PRINT CHR$(201); STRING$(12, 205); CHR$(187)
    PRINT CHR$(186); CHR$(219); " CLED HELP "; CHR$(186)
    PRINT CHR$(202); STRING$(12, 205); CHR$(202);
    PRINT STRING$(66, 205);

END SUB

SUB HelpVisualMode ()
END SUB

SUB IndexJump ()
    DIM i AS INTEGER, curIdx AS INTEGER
    DIM selection AS INTEGER

    Message "Updating cross-references..."
    LastLocation = DisplayStart
    ApplyXRefRules TRUE
   
    VIEW PRINT 1 TO Cons.Height
    COLOR 15, 1
    CLS
    LOCATE 3, 2
    PRINT "Select a cross-reference:"
    PRINT
    FOR i = 1 TO XRefRuleCount
        PRINT "    "; szTrimText(STR$(i)); ":  "; XRefRules(i).RuleName
        IF CSRLIN > Cons.Height THEN
            PRINT "---MORE---"
            SLEEP
            CLS
        END IF
    NEXT i
    PRINT ""
    INPUT "    ==> ", selection
    
    CLS
    PRINT "Choose "; RTRIM$(XRefRules(selection).RuleName); " cross-reference:"

    FOR i = 1 TO indexEntryCount
        IF IndexEntries(i).IndexName = XRefRules(selection).RuleName THEN
            PRINT "  "; szTrimText(STR$(i)); ":  "; IndexEntries(i).MatchName
            IF CSRLIN > Cons.Height THEN
                PRINT "---MORE---"
                SLEEP
                CLS
            END IF
        END IF
    NEXT i
    PRINT
    INPUT "  ==>", selection

    StartDisplay = IndexEntries(selection).LineNumber
    COLOR 7, 0
    CLS

END SUB

FUNCTION InSelection (l AS INTEGER, c AS INTEGER) AS INTEGER

    DIM InLineRange AS INTEGER
    DIM InColRange AS INTEGER
    DIM InFirstLine AS INTEGER
    DIM InLastLine AS INTEGER
    DIM ColumnMatters AS INTEGER

    InLineRange = FALSE
    InColRange = FALSE

    IF l >= SelBlock.StartLine AND l <= SelBlock.EndLine THEN
        InLineRange = TRUE
    ELSE
        InLineRange = FALSE
    END IF

    IF l = SelBlock.StartLine THEN InFirstLine = TRUE ELSE InFirstLine = FALSE
    IF l = SelBlock.EndLine THEN InLastLine = TRUE ELSE InLastLine = FALSE

    IF InFirstLine = TRUE OR InLastLine = TRUE THEN
        ColumnMatters = TRUE
    ELSE
        ColumnMatters = FALSE
    END IF
   
    IF ColumnMatters = FALSE THEN
        InColRange = TRUE
    ELSE
        IF InFirstLine = TRUE AND InLastLine = TRUE THEN
            IF c >= SelBlock.StartCol AND c <= SelBlock.EndCol THEN
                InColRange = TRUE
            ELSE
                InColRange = FALSE
            END IF
        ELSEIF InFirstLine = TRUE AND InLastLine = FALSE THEN
            IF c >= SelBlock.StartCol THEN
                InColRange = TRUE
            ELSE
                InColRange = FALSE
            END IF
        ELSEIF InFirstLine = FALSE AND InLastLine = TRUE THEN
            IF c <= SelBlock.EndCol THEN
                InColRange = TRUE
            ELSE
                InColRange = FALSE
            END IF
        END IF
    END IF

    IF InLineRange = TRUE AND InColRange = TRUE THEN
        InSelection = TRUE
    ELSE
        InSelection = FALSE
    END IF

END FUNCTION

SUB InsertChar (ILine AS INTEGER, ICol AS INTEGER, IChar AS STRING)

    DIM tmp AS STRING

    IF ILine > LineCount THEN
        ExpandBuffer ILine - LineCount
    END IF

    tmp = MID$(EditBuffer(ILine), ICol)
    EditBuffer(ILine) = EditBuffer(ILine) + " "

    'IF ICol + 1 < LEN(EditBuffer(ILine)) THEN
        MID$(EditBuffer(ILine), ICol + 1) = tmp
    'END IF

    MID$(EditBuffer(ILine), ICol, 1) = IChar

END SUB

SUB JoinLines (SourceLine AS INTEGER, TargetLine AS INTEGER)
    EditBuffer(TargetLine) = EditBuffer(TargetLine) + EditBuffer(SourceLine)
    ShiftLinesUp SourceLine + 1, 1
END SUB

SUB Message (MTxt AS STRING)
    VIEW PRINT Cons.MessageLine TO Cons.MessageLine
    LOCATE Cons.MessageLine, 1: COLOR 0, 3: PRINT STRING$(Cons.Width, " ");
    LOCATE Cons.MessageLine, 1: COLOR 0, 3: PRINT MTxt;
    BEEP
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

SUB Paste ()

END SUB

SUB SetTabs ()
    VIEW PRINT Cons.MessageLine TO Cons.MessageLine
    LOCATE Cons.MessageLine, 1
    COLOR 12, 0
    PRINT STRING$(80, " ");
    LOCATE Cons.MessageLine, 1
    PRINT "Tabs: "; TabStops; " ";
    INPUT "Set to: ", TabStops
    Message "Tabs set to " + szTrimText(STR$(TabStops))
    UpdateScreen

END SUB

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

SUB ShiftLinesUp (FromLine AS INTEGER, NumLines AS INTEGER)
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

    OriginalBufferSize = LineCount
    ShrinkBuffer NumLines

    NewLocLine = FromLine - NumLines

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

sub rulerLine()
    dim i as integer
    dim curCol as integer

    view print Cons.RulerLine to Cons.RulerLine: COLOR 7, 0
    color 15, 1
    locate Cons.RulerLine, 1
    print " LINE  0";
    curCol = 0
    for i = 1 to Cons.MaxLineWidth + 2
    	if i mod 10 = 0 then
	   if curCol < 9 then
	       curCol += 1
	   else
	       curCol = 1
	   end if
	   print szTrimText(str(curCol));
	else
	   print "-";
        end if
    next
    
end sub

SUB UpdateScreen ()
    dim curDispLine as integer

    updateConsole
    rulerLine

    view print Cons.EditTop TO Cons.EditBottom
    COLOR 7, 0
    cls

    curDispLine = Cons.EditTop

    FOR i = StartDisplay TO StartDisplay + Cons.MaxLines
        
	locate curDispLine, 1
	print string(80, " ");
        locate curDispLine, 1
        IF i <= LineCount THEN COLOR 10, 0 ELSE COLOR 12, 0

        PRINT PadLeft(szTrimText(STR$(i)), "0", 5); ":  ";
        
        COLOR 7, 0
        IF i <= LineCount THEN
            
            IF LEN(EditBuffer(i)) > Cons.MaxLineWidth THEN
                COLOR 7, 0
                PRINT LEFT$(EditBuffer(i), Cons.MaxLineWidth);
                COLOR 12, 0: PRINT C_LINE_CONT;: COLOR 7, 0
            ELSE
                PRINT EditBuffer(i)
            END IF
        ELSE
            IF i = LineCount + 1 THEN
                COLOR 12, 0: PRINT "*** END OF FILE ***": COLOR 7, 0
            ELSE
                PRINT ""
            END IF
        END IF

	curDispLine = curDispLine + 1

    NEXT
    VIEW PRINT Cons.StatusLine TO Cons.StatusLine
    COLOR 15, 1
    CLS
    LOCATE Cons.StatusLine, 1
    PRINT InFile$; " "; C_VERT_BAR; " "; UCASE$(EditFileType); " "; C_VERT_BAR; " EDIT: "; szTrimText(STR$(LineCount)); "L ";
    PRINT C_VERT_BAR; " COPY: "; szTrimText(STR$(CopyBufferLines)); "L ";
    PRINT C_VERT_BAR; " LINE "; szTrimText(STR$((VLine + StartDisplay) - 1)); " "; C_VERT_BAR; " COL "; szTrimText(STR$(VCol));
    VIEW PRINT 1 TO Cons.Height
END SUB

SUB VisualMode ()

    DIM CurChar AS STRING

    dim con as integer, ocon as integer

    DIM K_LEFT AS STRING
    DIM K_RIGHT AS STRING
    DIM K_UP AS STRING
    DIM K_DOWN AS STRING
    DIM K_DEL AS STRING
    DIM K_BS AS STRING
    DIM K_ESC AS STRING
    DIM K_TAB AS STRING
    DIM K_PGUP AS STRING
    DIM K_PGDOWN AS STRING
    DIM K_HOME AS STRING
    DIM K_END AS STRING
    DIM K_INSERT AS STRING
    DIM K_ENTER AS STRING
    DIM K_F1 AS STRING
    DIM K_F2 AS STRING
    DIM K_F3 AS STRING
    DIM K_F4 AS STRING
    DIM K_F5 AS STRING
    DIM K_F6 AS STRING
    DIM K_F7 AS STRING
    DIM K_F8 AS STRING
    DIM K_F9 AS STRING
    DIM K_F10 AS STRING
    DIM K_F11 AS STRING
    DIM K_F12 AS STRING
    DIM K_CTRLC AS STRING
    DIM K_CTRLS AS STRING
    DIM K_CTRLT AS STRING
    DIM K_CTRLQ AS STRING
    DIM K_CTRLW AS STRING
    DIM K_CTRLD AS STRING

    K_LEFT = CHR$(255) + "K"
    K_RIGHT = CHR$(255) + "M"
    K_UP = CHR$(255) + "H"
    K_DOWN = CHR$(255) + "P"
    K_DEL = CHR$(255) + "S"
    K_ESC = CHR$(27)
    K_BS = CHR$(8)
    K_TAB = CHR$(9)
    K_PGUP = CHR$(255) + "I"
    K_PGDOWN = CHR$(255) + "Q"
    K_HOME = CHR$(255) + "G"
    K_END = CHR$(255) + "O"
    K_INSERT = CHR$(255) + "R"
    K_ENTER = CHR$(13)
    K_F1 = CHR$(255) + CHR$(59)
    K_F2 = CHR$(255) + CHR$(60)
    K_F3 = CHR$(255) + CHR$(61)
    K_F4 = CHR$(255) + CHR$(62)
    K_F5 = CHR$(255) + CHR$(63)
    K_F6 = CHR$(255) + CHR$(64)
    K_F7 = CHR$(255) + CHR$(65)
    K_F8 = CHR$(255) + CHR$(66)
    K_F9 = CHR$(255) + CHR$(67)
    K_F10 = CHR$(255) + CHR$(68)
    K_F11 = CHR$(255) + CHR$(133)
    K_F12 = CHR$(255) + CHR$(134)
    K_CTRLC = CHR$(3)
    K_CTRLS = CHR$(19)
    K_CTRLT = CHR$(20)
    K_CTRLQ = CHR$(17)
    K_CTRLW = CHR$(23)

    VLine = 1
    VCol = 1

    DO

	'redraw if the console size has changed
	con = width()
	if con <> ocon then UpdateScreen

        BufferCol = VCol
        BufferLine = (VLine + StartDisplay) - 1

        LOCATE VLine + 1, VCol + 8, 1

        i$ = INKEY$
        SELECT CASE i$
            CASE K_ENTER
                BreakLine BufferLine, VCol
                VLine = VLine + 1
                VCol = 1
                UpdateScreen
            CASE K_LEFT
                IF VCol > 1 THEN VCol = VCol - 1 ELSE BEEP
            CASE K_RIGHT
                IF VCol < Cons.EditRight THEN VCol = VCol + 1 ELSE BEEP
            CASE K_UP
                IF VLine > 1 THEN VLine = VLine - 1

                IF VLine = 1 THEN
                    IF StartDisplay > 1 THEN
                        StartDisplay = StartDisplay - 1
                    ELSE
                        BEEP
                    END IF
                END IF
                UpdateScreen
            CASE K_DOWN
                IF VLine = Cons.MaxLines THEN
                    StartDisplay = StartDisplay + 1
                    UpdateScreen
                ELSE
                    VLine = VLine + 1
                    UpdateScreen
                END IF
            CASE K_CTRLT
                SetTabs
            CASE K_DEL
                IF LEN(EditBuffer(BufferLine)) >= VCol THEN
                    DelCharRight BufferLine, VCol
                ELSE
                    BEEP
                END IF
            CASE K_BS
                IF VCol > 1 THEN
                    IF VCol > LEN(EditBuffer(BufferLine)) THEN
                        VCol = LEN(EditBuffer(BufferLine)) + 1
                    END IF
                    DelCharLeft BufferLine, VCol
                    VCol = VCol - 1
                ELSE
                    postJoinCol% = LEN(EditBuffer(BufferLine - 1)) + 1
                    JoinLines BufferLine, BufferLine - 1
                    VLine = VLine - 1
                    VCol = postJoinCol%
                END IF
                UpdateScreen

            CASE K_TAB
                FOR i = 1 TO TabStops
                    InsertChar BufferLine, VCol, " "
                NEXT
                VCol = VCol + TabStops
            CASE K_PGDOWN
                StartDisplay = StartDisplay + Cons.MaxLines
                
            CASE K_PGUP
                StartDisplay = StartDisplay - Cons.MaxLines
                IF StartDisplay < 1 THEN StartDisplay = 1: BEEP
                UpdateScreen
            CASE K_HOME
                VCol = 1
            CASE K_END
                endLen% = LEN(EditBuffer(BufferLine)) + 1

                IF endLen% < Cons.EditRight THEN
                    VCol = endLen%
                ELSE
                    VCol = Cons.EditRight
                END IF
            CASE K_INSERT
                PRINT "INSERT"
            CASE K_ESC
                EXIT SUB
            CASE K_F1
                Help
            CASE K_F2
                IndexJump
                UpdateScreen
            CASE K_F3
                DisplayStart = LastLocation
                UpdateScreen
            CASE K_F4
                Paste
            CASE K_F5
                UpdateScreen
            CASE K_F6
                Find
            CASE K_F7
                FindReplace
            CASE K_F8
                SelBlock.StartLine = BufferLine
                SelBlock.EndLine = BufferLine
                SelBlock.StartCol = VCol
                SelBlock.EndCol = VCol
                Message "Mark set"
                UpdateScreen
            CASE K_F9
                IF BufferLine >= SelBlock.StartLine AND VCol >= SelBlock.StartCol THEN
                    SelBlock.EndLine = BufferLine
                    SelBlock.EndCol = VCol
                    SelActive = TRUE
                    Message "Block set (" + szTrimText(STR$(SelBlock.StartLine)) + "," + szTrimText(STR$(SelBlock.StartCol)) + ")-(" + szTrimText(STR$(SelBlock.EndLine)) + "," + szTrimText(STR$(SelBlock.EndCol)) + ")"
                    UpdateScreen
                ELSE
                    Message "Cannot end block before its start"
                END IF
            CASE K_CTRLQ
                ExitCLED
            CASE ELSE
                IF i$ <> "" THEN
                    InsertChar BufferLine, VCol, i$
                    VCol = VCol + 1
                    UpdateScreen
                END IF
        END SELECT
        IF i$ <> "" THEN UpdateScreen

	ocon = con
    LOOP
END SUB

SUB WaitOutput ()
    COLOR 12, 0
    LOCATE 24, 1
    PRINT "Press ESC to continue"
    WHILE INKEY$ <> CHR$(27): WEND
    COLOR 7, 0
END SUB


sub updateConsole()
    dim tw as integer

    tw = width()

    Cons.Width = loword(tw)
    Cons.Height = hiword(tw)
    Cons.StatusLine = Cons.Height
    Cons.RulerLine = 1
    Cons.EditTop = Cons.RulerLine + 1
    Cons.EditLeft = 9
    Cons.EditRight = Cons.Width - 1
    Cons.MessageLine = Cons.StatusLine - 1
    Cons.EditBottom = Cons.MessageLine - 1
    Cons.MaxLines = Cons.EditBottom - Cons.EditTop - 1
    Cons.MaxLineWidth = Cons.Width - Cons.EditLeft
    Cons.CommandModeLine = Cons.Height - 16    
   
end sub
