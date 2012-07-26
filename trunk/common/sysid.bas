DECLARE FUNCTION MachineID () AS INTEGER
OPTION EXPLICIT

FUNCTION MachineID () AS INTEGER
        DIM x AS INTEGER
        DEF SEG = &HF000

        x = PEEK(&HFFFE)
        MachineID = x
END FUNCTION

