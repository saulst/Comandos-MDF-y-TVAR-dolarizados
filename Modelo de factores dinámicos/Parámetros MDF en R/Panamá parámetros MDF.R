library(tseries)
library(readxl)
library("fUnitRoots")
library(clusterSim)
library(corrplot)
library(psych)
library("POET")
library(dynfactoR)
library(forecast)
library("fGarch")
library(xlsx)
library(tidyverse)
library(readxl)
library(zoo)
library(lubridate)
library(gridExtra)
library(grid)
library(janitor)
library(tsDyn)
library(gt)
library(broom)
library(xlsx)
library(seasonal)



INDICE_PANAMA <- read_excel("D:/Saúl/TESIS/BASE DE DATOS DOLARIADOS/PANAMA/INDICE PANAMA/INDICE PANAMA.xlsx")



mora<-INDICE_PANAMA$mora
imae<-INDICE_PANAMA$imae
bvpsi<- INDICE_PANAMA$bvpsi
ta<-INDICE_PANAMA$`tasa activa`
tp<-INDICE_PANAMA$`tasa pasiva`
c.d<-INDICE_PANAMA$`CREDITO/DEPOSITOS`
ted<-INDICE_PANAMA$`ted spread`
liq<-INDICE_PANAMA$liq
wti<-INDICE_PANAMA$WTI
embi<-INDICE_PANAMA$embi
#roa<-INDICE_PANAMA$roa
#roe<-INDICE_PANAMA$roe
ut_roa<-INDICE_PANAMA$UT_ROA
ut_roe<-INDICE_PANAMA$UT_ROE


ted=ts(ted,frequency=12,start=c(2007,1))
wti=ts(wti, frequency=12, start=c(2007,1))
c.d=ts(c.d, frequency=12, start=c(2007,1))
embi=ts(embi, frequency=12, start=c(2007,1))
tp=ts(tp,frequency=12, start=c(2007,1))
imae=ts(imae, frequency=12,start=c(2007,1))
ta=ts(ta, frequency=12, start=c(2007,1))
liq=ts(liq, frequency=12, start=c(2007,1))
mora=ts(mora, frequency=12, start=c(2007,1))
bvpsi=ts(bvpsi, frequency=12, start=c(2007,1))
#roa=ts(roa, frequency=12, start=c(2007,1))
#roe=ts(roe, frequency=12, start=c(2007,1))
ut_roa=ts(ut_roa, frequency=12, start=c(2007,1))
ut_roe=ts(ut_roe, frequency=12, start=c(2007,1))


plot(ted)
plot(wti)
plot(c.d)
plot(decompose(mora))
plot(embi)
plot(tp)
plot(imae)
plot(ta)
plot(liq)
plot(mora)
plot(bvpsi)
plot(ut_roa)
plot(ut_roe)


X<-cbind(ted, wti,c.d,embi,tp,imae,ta,ut_roa,ut_roe,liq,mora,bvpsi)
X=ts(X,frequency=12,start=c(2007,1))

normalizadas<-as.data.frame(scale(X[,c(1,2,3,4,5,6,7,8,9,10,11,12)]))
normalizadas<- ts(normalizadas,frequency=12,start=c(2007,1) )
normalizadas

ted1<-normalizadas[,1]
wti1<-normalizadas[,2]
cr.dep<-normalizadas[,3]
embi1<-normalizadas[,4]
tp1<-normalizadas[,5]
imae1<-normalizadas[,6]
ta1<-normalizadas[,7]
liq1<-normalizadas[,10]
mora1<-normalizadas[,11]
bvpsi1<-normalizadas[,12]
ut_roa1<-normalizadas[,8]
ut_roe1<-normalizadas[,9]




##################
adfTest(ted1) 
ted1.d1=diff(ted1)
adfTest(ted1.d1)## estacionario en priemras diferencias


adfTest(wti1)  

adfTest(cr.dep) 
cr.depd1=diff(cr.dep)
adfTest(cr.depd1) 

adfTest(embi1) ## embi(riesgo pais) es estacionario 

adfTest(tp1) ##  pestacionario

adfTest(imae1) ##imae es estacionario pero tiene tendencia y etacionalidad
imae1.d1<-diff(imae1)
adfTest(imae1.d1)


adfTest(ut_roa1)

adfTest(ut_roe1)
ut_roed1<-diff(ut_roe1)
adfTest(ut_roed1)


adfTest(ta1) #p-value=-1.35 no es estacionaria,, se procede a diferenciar
ta1.d1=diff(ta1)
adfTest(ta1.d1) #tasa activa es estacionaria en primeras diferencias

adfTest(liq1) ##estacionario


adfTest(mora1)
mora1.d1=diff(mora1)
adfTest(mora1.d1) ## mora es estacionario en priemras diferencias



adfTest(bvpsi1)
bvpsi1.d1=diff(bvpsi1)
adfTest(bvpsi1.d1)

Y<- cbind(ted1.d1,wti1, cr.depd1, embi1, tp1, imae1.d1, ta1.d1, 
          ut_roa1, ut_roed1, liq1, mora1.d1, bvpsi1.d1  )

write.xlsx(Y, "variables indice panama.xlsx")
Yomit<-na.omit(Y)
lags.select(Yomit)


######################################
correlacion<-cor(Yomit)
fa.parallel(correlacion, n.obs=length(X), fm="gls", main="Panamá")
## numero de factores 5 y numero de componentes 5

acp<- prcomp(Yomit)
plot(acp, type="l")
summary(acp)
acp

componente1<- apply(acp$rotation[,1]*Y,1,sum)
componente2<-apply(acp$rotation[,2]*Y,1,sum)
componente3<-apply(acp$rotation[,3]*Y,1,sum)
componente4<-apply(acp$rotation[,4]*Y,1,sum)
componente5<-apply(acp$rotation[,5]*Y,1,sum)

componentesprin<-cbind(componente1,componente2,componente3,componente4, componente5)
componentesomit=na.omit(componentesprin)

lag1<-lags.select(componentesomit, lag.max = 5)
lag_pan<-rbind(lag1$AICs, lag1$BICs,lag1$HQs)
row.names(lag_pan)<-c("AIC_PANAMA","BIC_PANAMA","HQ_PANAMA") 
write.xlsx(lag_pan, "Panama rezagos.xlsx")

#### me dice que debo tener 2 rrezagos

#Best AIC:  lag= 2 
#Best BIC:  lag= 1 
#Best HQ :  lag= 2 
