
dim con as integer
dim w as integer
dim h as integer

dim ocon as integer

ocon = 0

do
	con = width()

	w = loword(con)
	h = hiword(con)

	if ocon <> con then
	   cls
	   print "new size: "; w; " x "; h
        end if

	ocon = con	

loop until inkey <> ""
