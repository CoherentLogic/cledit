DO
    i$ = INKEY$

    IF i$ <> "" THEN
        SELECT CASE LEN(i$)
            CASE 1
                PRINT "1: "; ASC(i$)
            CASE 2
                c1$ = LEFT$(i$, 1)
                c2$ = RIGHT$(i$, 1)

                PRINT "2: "; ASC(c1$), ASC(c2$)
        END SELECT
    END IF

LOOP

