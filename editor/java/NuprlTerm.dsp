# Microsoft Developer Studio Project File - Name="NuprlTerm" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Java Virtual Machine Java Project" 0x0809

CFG=NuprlTerm - Java Virtual Machine Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "NuprlTerm.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "NuprlTerm.mak" CFG="NuprlTerm - Java Virtual Machine Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "NuprlTerm - Java Virtual Machine Release" (based on\
 "Java Virtual Machine Java Project")
!MESSAGE "NuprlTerm - Java Virtual Machine Debug" (based on\
 "Java Virtual Machine Java Project")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
JAVA=jvc.exe

!IF  "$(CFG)" == "NuprlTerm - Java Virtual Machine Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir ""
# PROP BASE Intermediate_Dir ""
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ""
# PROP Intermediate_Dir ""
# PROP Target_Dir ""
# ADD BASE JAVA /O
# ADD JAVA /O

!ELSEIF  "$(CFG)" == "NuprlTerm - Java Virtual Machine Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir ""
# PROP BASE Intermediate_Dir ""
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ""
# PROP Intermediate_Dir ""
# PROP Target_Dir ""
# ADD BASE JAVA /g
# ADD JAVA /g

!ENDIF 

# Begin Target

# Name "NuprlTerm - Java Virtual Machine Release"
# Name "NuprlTerm - Java Virtual Machine Debug"
# Begin Source File

SOURCE=.\ActiveApplet.java
# End Source File
# Begin Source File

SOURCE=.\ActiveApplication.java
# End Source File
# Begin Source File

SOURCE=.\BoundTerm.java
# End Source File
# Begin Source File

SOURCE=.\Closure.java
# End Source File
# Begin Source File

SOURCE=.\DebugFlags.java
# End Source File
# Begin Source File

SOURCE=.\DisplayDynamic.java
# End Source File
# Begin Source File

SOURCE=.\DisplayEngine.java
# End Source File
# Begin Source File

SOURCE=.\DisplayTerm.java
# End Source File
# Begin Source File

SOURCE=.\Eval.java
# End Source File
# Begin Source File

SOURCE=.\EvalError.java
# End Source File
# Begin Source File

SOURCE=.\FontBase.java
# End Source File
# Begin Source File

SOURCE=.\FreeVar.java
# End Source File
# Begin Source File

SOURCE=.\IntStack.java
# End Source File
# Begin Source File

SOURCE=.\LevelExp.java
# End Source File
# Begin Source File

SOURCE=.\LevelVar.java
# End Source File
# Begin Source File

SOURCE=.\Lexer.java
# End Source File
# Begin Source File

SOURCE=.\LispExpression.java
# End Source File
# Begin Source File

SOURCE=.\LispParser.java
# End Source File
# Begin Source File

SOURCE=.\LispSExpression.java
# End Source File
# Begin Source File

SOURCE=.\LispSoApply.java
# End Source File
# Begin Source File

SOURCE=.\LispVar.java
# End Source File
# Begin Source File

SOURCE=.\MatchError.java
# End Source File
# Begin Source File

SOURCE=.\Matching.java
# End Source File
# Begin Source File

SOURCE=.\NetscapeApplet.java
# End Source File
# Begin Source File

SOURCE=.\NuprlTerm.java
# End Source File
# Begin Source File

SOURCE=.\Operator.java
# End Source File
# Begin Source File

SOURCE=.\Opname.java
# End Source File
# Begin Source File

SOURCE=.\Param.java
# End Source File
# Begin Source File

SOURCE=.\ParamLevelExp.java
# End Source File
# Begin Source File

SOURCE=.\ParamMatchError.java
# End Source File
# Begin Source File

SOURCE=.\ParamMDiff.java
# End Source File
# Begin Source File

SOURCE=.\ParamMEqual.java
# End Source File
# Begin Source File

SOURCE=.\ParamMeta.java
# End Source File
# Begin Source File

SOURCE=.\ParamMLessThan.java
# End Source File
# Begin Source File

SOURCE=.\ParamMLevel.java
# End Source File
# Begin Source File

SOURCE=.\ParamMNotEqual.java
# End Source File
# Begin Source File

SOURCE=.\ParamMNumber.java
# End Source File
# Begin Source File

SOURCE=.\ParamMPair.java
# End Source File
# Begin Source File

SOURCE=.\ParamMProduct.java
# End Source File
# Begin Source File

SOURCE=.\ParamMQuotient.java
# End Source File
# Begin Source File

SOURCE=.\ParamMRem.java
# End Source File
# Begin Source File

SOURCE=.\ParamMString.java
# End Source File
# Begin Source File

SOURCE=.\ParamMSum.java
# End Source File
# Begin Source File

SOURCE=.\ParamMToken.java
# End Source File
# Begin Source File

SOURCE=.\ParamMVar.java
# End Source File
# Begin Source File

SOURCE=.\ParamNumber.java
# End Source File
# Begin Source File

SOURCE=.\ParamString.java
# End Source File
# Begin Source File

SOURCE=.\ParamToken.java
# End Source File
# Begin Source File

SOURCE=.\ParamVar.java
# End Source File
# Begin Source File

SOURCE=.\QuickSort.java
# End Source File
# Begin Source File

SOURCE=.\Rewrite.java
# End Source File
# Begin Source File

SOURCE=.\Semaphore.java
# End Source File
# Begin Source File

SOURCE=.\SmallScrollGroup.java
# End Source File
# Begin Source File

SOURCE=.\Sort.java
# End Source File
# Begin Source File

SOURCE=.\Subst.java
# End Source File
# Begin Source File

SOURCE=.\SubstParam.java
# End Source File
# Begin Source File

SOURCE=.\SubstSimul.java
# End Source File
# Begin Source File

SOURCE=.\SubstSingle.java
# End Source File
# Begin Source File

SOURCE=.\Term.java
# End Source File
# Begin Source File

SOURCE=.\TermBreak.java
# End Source File
# Begin Source File

SOURCE=.\TermDisplay.java
# End Source File
# Begin Source File

SOURCE=.\TermFont.java
# End Source File
# Begin Source File

SOURCE=.\TermLexer.java
# End Source File
# Begin Source File

SOURCE=.\TermNuprl.java
# End Source File
# Begin Source File

SOURCE=.\TermParser.java
# End Source File
# Begin Source File

SOURCE=.\TermPop.java
# End Source File
# Begin Source File

SOURCE=.\TermPush.java
# End Source File
# Begin Source File

SOURCE=.\TermSoApply.java
# End Source File
# Begin Source File

SOURCE=.\TermSoVar.java
# End Source File
# Begin Source File

SOURCE=.\TermString.java
# End Source File
# Begin Source File

SOURCE=.\TermVar.java
# End Source File
# Begin Source File

SOURCE=.\TermView.java
# End Source File
# Begin Source File

SOURCE=.\TermZone.java
# End Source File
# Begin Source File

SOURCE=.\TextBuffer.java
# End Source File
# Begin Source File

SOURCE=.\TextViewBuffer.java
# End Source File
# Begin Source File

SOURCE=.\Token.java
# End Source File
# End Target
# End Project
