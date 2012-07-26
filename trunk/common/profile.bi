'===============================================
' PROFILE.BI
' Include file for accessing PROFILE.BAS
'===============================================

DECLARE FUNCTION ProfileRead$ (IniFile$, IniSection$, IniKey$)
DECLARE SUB ProfileWrite (IniFile$, IniSection$, IniKey$, ProfileStr$)

