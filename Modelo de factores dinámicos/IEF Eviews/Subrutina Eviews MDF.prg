Subroutine DFM(Group XGrp, Scalar FNum, Scalar VLag, Sample S)
'SCALAR WITH THE NUMBER OF OBSERVED VARIABLES
Scalar XNum = XGrp.@Count
'FINDING SAMPLE FOR BALANCED PANEL
'Creating matrix that preserves NAs with name 'XMat'
Smpl @All
Stomna(XGrp,XMat)
'Finding start and end dates for balanced panel, restricted by sample S
Smpl S
If @Dtoo(@WLeft(@PageSmpl,1)) >= @Max(@Cifirst(XMat)) Then
%BalStart = @WLeft(@PageSmpl,1)
Else
%BalStart = @Otod(@Max(@Cifirst(XMat)))
EndIf
If @Dtoo(@WRight(@PageSmpl,1)) <= @Min(@Cilast(XMat)) Then
%BalEnd = @WRight(@PageSmpl,1)
Else
%BalEnd = @Otod(@Min(@Cilast(XMat)))
EndIf
'CHECKING THAT THERE ARE NO MISSING VALUES WITHIN BALANCED PANEL
Smpl {%BalStart} {%BalEnd}
!i = 0
While !i < XGrp.@Count
!i = !i+1
%SerName = XGrp.@Seriesname(!i)
Series NAtest = @IsNa({%SerName})
If @Sum(NAtest) > 0 Then
%PromptStr = %SerName + " has NAs within the balanced panel."
%PromptStr = %PromptStr + " The variable is removed."
@UiPrompt(%PromptStr)
XGrp.Drop {%SerName}
If XGrp.@Count > 0 Then
Matrix LambdaHat = LambdaHat.@Droprow(!i)
Matrix CovEpsHat = CovEpsHat.@Droprow(!i)
!i = !i-1
Else
'If no variables remain, the subroutine is ended

@UiPrompt("There are no variables left. The subroutine is ended.")
Return
EndIf
EndIf
WEnd
'Recreating scalar with number of variables
XNum = XGrp.@Count
'ESTIMATING BY PC
'Standardizing data over balanced panel
For !i = 1 to XNum
%Series = XGrp.@Seriesname(!i)
Smpl {%BalStart} {%BalEnd}
!Std = @StDev({%Series})
!Mean = @Mean({%Series})
Smpl @All
{%Series} = ({%Series}-!Mean)/!Std
Next
Smpl {%BalStart} {%BalEnd}
'Creating matrix of balanced panel (T times N)
Stom(XGrp,XMatBal)
'Computing sample covariance matrix of x
Sym CovXHat = (@Transpose(XMatBal))*XMatBal/@Rows(XMatBal)
'Computing ordered eigenvalues and associated eigenvectors
Vector EigVals = @Sort(@Eigenvalues(CovXHat),"d")
Vector EigRanks = @Ranks(EigVals,"a","i")
Matrix EigVecs = @Rapplyranks(@Eigenvectors(CovXHat),EigRanks)
'Estimating factors (GHat: N times T) and loadings (LambdaHat: N times R),
'and residual covariance matrix (CovEpsHat: N times N)
Matrix DHat = @Makediagonal(@Subextract(EigVals,1,1,FNum,1))
Matrix PHat = @Subextract(EigVecs,1,1,XNum,FNum)
Matrix GHat = (@Sqrt(@Inverse(DHat)))*(@Transpose(PHat))*(@Transpose(XMatBal))
Matrix LambdaHat = PHat*(@Sqrt(DHat))
'Estimating residual covariance matrix
Matrix CovEpsHat = CovXHat-(LambdaHat*(@Transpose(LambdaHat)))
'Creating factor series, that will be used for constructing states
Group FGrp
For !i = 1 to FNum
Series pc_{!i}
FGrp.Add pc_{!i}
Next
'Placing values in factor series
Matrix TGHat = @Transpose(GHat)
Mtos(TGHat,FGrp)
'Creating list with names of factor series
%Glist = FGrp.@Members

'ESTIMATING VAR ON FACTORS, WITHOUT CONSTANT
Smpl {%BalStart} {%BalEnd}
Var GVar.Ls(noconst) 1 {VLag} {%Glist}
'Placing VAR coefficients in matrix
Matrix AHat = GVar.@Coefmat
'Creating VAR residual covariance matrix
Matrix CovWHat = GVar.@Residcov
'CREATING STATE SPACE OBJECT WITH NAME 'DFMSS'
SSpace DFMSS
'NAMING SIGNAL RESIDUALS AND ASSIGNING THEM PC-ESTIMATED VARIANCES
For !i = 1 to XNum
DFMSS.Append @ename e{!i}
DFMSS.Append @evar Var(e{!i}) = CovEpsHat(!i,!i)
Next
'NAMING STATE RESIDUALS AND ASSIGNING THEM ESTIMATED VAR RESIDUAL
'VARIANCE/COVARIANCES
For !i = 1 to FNum
DFMSS.Append @ename w{!i}
DFMSS.Append @evar Var(w{!i}) = CovWHat(!i,!i)
If FNum > 1 Then
For !j = !i+1 to FNum
DFMSS.Append @evar Cov(w{!i},w{!j}) = CovWHat(!i,!j)
Next
EndIf
Next
'DEFINING THE SIGNAL EQUATIONS
For !i = 1 to XNum
'Making string variable that is filled with signal equations
%Signal = XGrp.@Seriesname(!i)+" ="
For !j = 1 to FNum
%Signal = %Signal + " LambdaHat(" + @Str(!i) + "," + @Str(!j) + ")*SV"
%Signal = %Signal + @Str(!j) + "_0 +"
Next
'Adding error and appending signal equations to state space object
%Signal = %Signal + " e" + @Str(!i)
DFMSS.Append @Signal {%Signal}
Next
'DEFINING THE R (= NUMBER OF FACTORS) FIRST STATE EQUATIONS
For !i = 1 to FNum
'Making string variable that is filled with state equation
%State = "SV" + @Str(!i) + "_0 ="
For !a = 1 to FNum
For !j = 1 to VLag

%State = %State + " AHat(" + @Str(!j + VLag*(!a-1)) + "," + @Str(!i)
%State = %State + ")*SV" + @Str(!a) + "_" + @Str(!j-1) + "(-1) +"
Next
Next
'Adding error and appending state equations to state space object
%State = %State + " w" + @Str(!i)
DFMSS.Append @State {%State}
Next
'DEFINING THE REMAINING STATE EQUATIONS, WITHOUT ERRORS
For !i = 1 to FNum
For !j = 1 to VLag-1
%State = "SV" + @Str(!i) + "_" + @Str(!j) + " = SV" + @Str(!i) + "_"
%State = %State + @Str(!j-1) + "(-1)"
DFMSS.Append @State {%State}
Next
Next
'SETTING UP SMOOTHER
Smpl S
DFMSS.ml
DFMSS.Makestates(t=smooth) *
'DELETING USED OBJECTS
Delete FNum XNum EigVals EigRanks EigVecs GHat  GVar FGrp PHat
Delete DHat CovXHat XMat XMatBal NAtest pc_*
EndSub

call DFM (g, 4,1, j)
