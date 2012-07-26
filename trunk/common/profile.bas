DECLARE FUNCTION ProfileRead$ (IniFile$, IniSection$, IniKey$)
'===============================================
' PROFILE.BAS
' Routines to write and read profile strings
'===============================================

' Notes:
' Add $INCLUDE: 'PROFILE.BI' to the calling module.
' Load PROFILE.BAS into your project list.
'
' Example of use:
'    Last$ = ProfileRead(IniFile$, IniSection$, IniKey$)
'    ProfileWrite IniFile$, IniSection$, IniKey$, ProfileStr$

'$INCLUDE: 'CONSTANT.BI'
'$INCLUDE: '..\COMMON\PROFILE.BI'

FUNCTION ProfileRead$ (IniFile$, IniSection$, IniKey$)
    TmpSection$ = "[" + IniSection$ + "]"
    ON LOCAL ERROR GOTO BadIniReadFile
    FileNum% = FREEFILE
    OPEN IniFile$ FOR INPUT AS #FileNum%
    DO UNTIL EOF(FileNum%)
        LINE INPUT #FileNum%, A$
        IF LEFT$(A$, 1) = "[" THEN
            IF INSTR(A$, TmpSection$) = 1 THEN
                SectionFlag% = TRUE
            ELSE
                SectionFlag% = FALSE
            END IF
        END IF
        IF SectionFlag% = TRUE THEN
            IF INSTR(A$, IniKey$) = 1 THEN
                A$ = MID$(A$, INSTR(A$, "=") + 1)
                ProfileRead$ = LTRIM$(RTRIM$(A$))
                EXIT DO
            END IF
        END IF
    LOOP
            
AllDoneRead:
    CLOSE #FileNum%
    EXIT FUNCTION

BadIniReadFile:
    RESUME AllDoneRead
END FUNCTION

SUB ProfileWrite (IniFile$, IniSection$, IniKey$, ProfileStr$)
    REDIM P$(1)

    TmpSection$ = "[" + IniSection$ + "]"
    FileNum% = FREEFILE

    ON LOCAL ERROR GOTO NoIniWriteFile
    OPEN IniFile$ FOR INPUT AS #FileNum%
    DO UNTIL EOF(FileNum%)
        n% = n% + 1
        REDIM PRESERVE P$(n%)
        LINE INPUT #FileNum%, P$(n%)
    LOOP
    CLOSE #FileNum%

AddToIniFile:
    ON LOCAL ERROR GOTO BadIniWriteFile
    FOR i% = 1 TO n%
        IF LEFT$(P$(i%), 1) = "[" THEN
            IF INSTR(P$(i%), TmpSection$) = 1 THEN
                SectionFlag% = TRUE
                SectionFound% = i%
            ELSE
                SectionFlag% = FALSE
            END IF
        END IF
        IF SectionFlag% = TRUE THEN
            IF INSTR(P$(i%), IniKey$) = 1 THEN
                P$(i%) = IniKey$ + "=" + ProfileStr$
                KeyFound% = TRUE%
                EXIT FOR
            END IF
        END IF
    NEXT i%

    IF SectionFound% = FALSE THEN
        REDIM PRESERVE P$(n% + 3)
        P$(n% + 2) = "[" + IniSection$ + "]"
        P$(n% + 3) = IniKey$ + "=" + ProfileStr$
        n% = n% + 3
    ELSEIF KeyFound% = FALSE THEN
        NewLine% = SectionFound% + 1
        REDIM PRESERVE P$(n% + 1)
        FOR i% = n% TO NewLine% STEP -1
            P$(i% + 1) = P$(i%)
        NEXT i%
        P$(NewLine%) = IniKey$ + "=" + ProfileStr$
        n% = n% + 1
    END IF

    OPEN IniFile$ FOR OUTPUT AS #FileNum%
    FOR i% = 1 TO n%
        PRINT #FileNum%, P$(i%)
    NEXT i%
    CLOSE #FileNum%

AllDoneWrite:
    CLOSE #FileNum%
    EXIT SUB

NoIniWriteFile:
    OPEN IniFile$ FOR OUTPUT AS #FileNum%
    PRINT #FileNum%, SPACE$(1)
    CLOSE #FileNum%
    RESUME AddToIniFile

BadIniWriteFile:
    RESUME AllDoneWrite
END SUB

