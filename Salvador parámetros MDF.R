library(tseries)
library(readxl)
library("fUnitRoots")
library(clusterSim)
library("nowcasting")
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


base<- read_excel("D:/Saúl/TESIS/BASE DE DATOS DOLARIADOS/EL SALVADOR/INDICE SALVADOR/INDICE SALVADOR.xlsx")%>% clean_names

vector_variables<- c("ted",
                     "wti",
                     "c_d",
                     "embi",
                     "roa",
                     "roe",
                     "imae",
                     "tp", 
                     "ta",
                     "ibes",
                     "liq",
                     "mora")

series <- base %>% 
  select(vector_variables)

normalizadas<- as.data.frame(scale(series))

#######################
plot.ts(normalizadas$ted)
pacf(normalizadas$ted)
acf(normalizadas$ted)
adfTest(normalizadas$ted)

####################
plot.ts(normalizadas$wti)
pacf(normalizadas$wti)
acf(normalizadas$wti)
adfTest(normalizadas$wti)

#####################

plot.ts(normalizadas$c_d)
pacf(normalizadas$c_d)
acf(normalizadas$c_d)
adfTest(normalizadas$c_d)

c_d.d1<-diff(normalizadas$c_d)
adfTest(c_d.d1)


################
plot.ts(normalizadas$embi)
pacf(normalizadas$embi)
acf(normalizadas$embi)
adfTest(normalizadas$embi)


##############

plot.ts(normalizadas$roa)
pacf(normalizadas$roa)
acf(normalizadas$roa)
adfTest(normalizadas$roa)

roa.d1<-diff(normalizadas$roa)
adfTest(roa.d1)

##############
plot.ts(normalizadas$roe)
pacf(normalizadas$roe)
acf(normalizadas$roe)
adfTest(normalizadas$roe)



##############

plot.ts(normalizadas$imae)
pacf(normalizadas$imae)
acf(normalizadas$imae)
adfTest(normalizadas$imae)

############

plot.ts(normalizadas$tp)
pacf(normalizadas$tp)
acf(normalizadas$tp)
adfTest(normalizadas$tp)

tap.d1<-diff(normalizadas$tp)
adfTest(tap.d1)

##############

plot.ts(normalizadas$ta)
pacf(normalizadas$ta)
acf(normalizadas$ta.)
adfTest(normalizadas$ta)

ta.d1<-diff(normalizadas$ta)
adfTest(ta.d1)

###############

plot.ts(normalizadas$mora)
pacf(normalizadas$mora)
acf(normalizadas$mora)
adfTest(normalizadas$mora)

mora.d1<-diff(normalizadas$mora)
adfTest(mora.d1)

############

plot.ts(normalizadas$ibes)
pacf(normalizadas$ibes)
acf(normalizadas$ibes)
adfTest(normalizadas$ibes)

ibes.d1<-diff(normalizadas$ibes)
adfTest(ibes.d1)

##############

plot.ts(normalizadas$liq)
pacf(normalizadas$liq)
acf(normalizadas$liq)
adfTest(normalizadas$liq)

liq.d1<-diff(normalizadas$liq)
adfTest(liq.d1)


###liq, ibes, c_d, roa, tp, ta, mora se tuvo que diferenciar 1 vez

normalizadas2<-select(normalizadas,-liq,-ibes,-roa,-c_d,-tp,-ta,-mora)

normalizadas2<-ts(normalizadas2, start = c(2007,1), frequency = 12)

variables<-cbind( c_d.d1,ibes.d1, liq.d1, mora.d1, roa.d1,tap.d1,ta.d1)
variables<-ts(variables, start=c(2007,1), frequency = 12)

variables2<-cbind(normalizadas2, variables)
variablesomit<-na.omit(variables2)
write.xlsx(variables2, "variables indice salvador.xlsx")
#####################

correlacion<-cor(series)
corrplot(cor(series), order = "hclust", tl.col='black', tl.cex=1)
fa.parallel(variables2, n.obs=length(variables2), fm="gls" ) 
fa.parallel(correlacion, n.obs=length(variables2), fm="gls", main= "El Salvador")
####5 factores y 5 componentes


####################

acp<-prcomp(variablesomit)
plot(acp, type="l")
summary(acp)


componente1<- apply(acp$rotation[,1]*variables2,1,sum)
componente2<-apply(acp$rotation[,2]*variables2,1,sum)
componente3<-apply(acp$rotation[,3]*variables2,1,sum)
componente4<-apply(acp$rotation[,4]*variables2,1,sum)
componente5<-apply(acp$rotation[,5]*variables2,1,sum)



componentesprin<-cbind(componente1,componente2,componente3,componente4)
componentesomit=na.omit(componentesprin)
lags.select(componentesomit, lag.max = 5)

###salvador 2 rezagos y 4 componentes

lag1<- lags.select(componentesomit, lag.max = 5)
lag_salva<-rbind(lag1$AICs, lag1$BICs,lag1$HQs)
row.names(lag_salva)<-c("AIC_SALV","BIC_SALV","HQ_SALV") 
write.xlsx(lag_salva, "Salvador rezagos.xlsx")

#Best AIC:  lag= 2 
#Best BIC:  lag= 2 
#Best HQ :  lag= 2 
