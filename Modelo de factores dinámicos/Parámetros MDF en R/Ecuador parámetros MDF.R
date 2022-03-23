library(tseries)
library(readxl)
library("fUnitRoots")
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


base_ecu <- read_excel("D:/Saúl/TESIS/BASE DE DATOS DOLARIADOS/ECUADOR/BASE INDICE ECUADOR.xlsx")

base_ecu<-base_ecu[-1]
base_ecu=ts(base_ecu,frequency=12,start=c(2007,1))
normales<-scale(base_ecu)

ted<-normales[,1]
wti<-normales[,2]
credito_deposito<-normales[,3]
riesgo_pais<-normales[,4]
roa<-normales[,5]
roe<-normales[,6]
ina<-normales[,7]
tasa_pasiva<-normales[,8]
tasa_activa<-normales[,9]
mora<-normales[,10]
ecuindex<-normales[,11]
liquidez<-normales[,12]

plot(ted, type="l")
plot(wti, type="l")
plot(credito_deposito, type="l")
plot(riesgo_pais, type="l")
plot(roa, type="l")
plot(roe, type="l")
plot(ina, type="l")
plot(tasa_pasiva, type="l")
plot(tasa_activa, type="l")
plot(mora, type="l")
plot(ecuindex, type="l")
plot(liquidez, type="l")


adfTest(ted)

adfTest(wti)

adfTest(credito_deposito)
credito_deposito_d1<-diff(credito_deposito)
adfTest(credito_deposito_d1)

adfTest(riesgo_pais)


adfTest(roa)
roa_d1<-diff(roa)
adfTest(roa_d1)

adfTest(roe)
roe_d1<-diff(roe)
adfTest(roe_d1)


adfTest(ina)

adfTest(tasa_pasiva)
tasa_pasiva_d1<-diff(tasa_pasiva)
adfTest(tasa_pasiva_d1)

adfTest(tasa_activa)
tasa_activa_d1<-diff(tasa_activa)
adfTest(tasa_activa_d1)

adfTest(mora)
mora_d1<-diff(mora)
adfTest(mora_d1)

adfTest(ecuindex)
ecuindex_d1<-diff(ecuindex)
adfTest(ecuindex_d1)

adfTest(liquidez)
liquidez_d1<-diff(liquidez)
adfTest(liquidez_d1)


variables<-cbind(ted, wti, credito_deposito_d1, riesgo_pais, roa_d1, 
                 roe_d1, ina, tasa_pasiva_d1, tasa_activa_d1, mora_d1, ecuindex_d1,liquidez_d1)

write.xlsx(variables, "variables_ecuador_indice.xlsx")
fa.parallel(cor(base_ecu), n.obs=length(variables), fm="gls", main="Ecuador")

#####acp######
variables_omit<-na.omit(variables)

acp<- prcomp(variables_omit)
acp
summary(acp)
plot(acp, type="l")


componente1<- apply(acp$rotation[,1]*variables,1,sum)
componente2<-apply(acp$rotation[,2]*variables,1,sum)
componente3<-apply(acp$rotation[,3]*variables,1,sum)
componente4<-apply(acp$rotation[,4]*variables,1,sum)

componentesprin<-cbind(componente1,componente2,componente3,componente4)
componentesomit=na.omit(componentesprin)

lag1<- lags.select(componentesomit, lag.max = 5)
lag_ecu<-rbind(lag1$AICs, lag1$BICs,lag1$HQs)
row.names(lag_ecu)<-c("AIC_ECU","BIC_ECU","HQ_ECU") 
write.xlsx(lag_ecu, "Ecuador rezagos.xlsx")

#Best AIC:  lag= 2 
#Best BIC:  lag= 1 
#Best HQ :  lag= 1 
