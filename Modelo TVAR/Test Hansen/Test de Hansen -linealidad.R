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

series0 <- read_excel("D:/Saúl/TESIS/BASE DE DATOS DOLARIADOS/Paises unidos.xlsx") %>% 
  clean_names 


vector_variables <- c(
  "pais",
  #"ipc",
  "ipc_a",
  "deuda",
  "m2",
  "ri",
  "cr_pib",
  "ief"
  
)

series <- series0 %>% 
  select(vector_variables)

######seleccion de rezago
rezagos<-function(pais0){
  
  d.pais <- series0 %>% 
    filter(pais == pais0) %>%  # me quedo solo con datos_pais
    select(vector_variables)  %>%  # Me quedo solo las variables del mdoelo
    select(-pais)
  
  lag1<- lags.select(d.pais)
  lag2<-rbind(lag1$AICs, lag1$BICs,lag1$HQs)
  
  
  
}

lag_ecu<-rezagos("Ecuador")
row.names(lag_ecu)<-c("AIC_ECU","BIC_ECU","HQ_ECU") 


lag_salva<-rezagos("Salvador")
row.names(lag_salva)<-c("AIC_SALV","BIC_SALV","HQ_SALV")

lag_pan<-rezagos("Panama")
row.names(lag_pan)<-c("AIC_PAN","BIC_PAN","HQ_PAN")

lag_paises<-rbind(lag_ecu,lag_salva, lag_pan)
write.xlsx(lag_paises, "Rezagos_paises.xlsx")

##ECUADOR
#Best AIC:  lag= 2 
#Best BIC:  lag= 2 
#Best HQ :  lag= 2 

##### SALVADOR
#Best AIC:  lag= 2 
#Best BIC:  lag= 1 
#Best HQ :  lag= 1 

##### PANAMA
#Best AIC:  lag= 1 
#Best BIC:  lag= 1 
#Best HQ :  lag= 1 


######ECUADOR##########

d.pais <- series0 %>% 
  filter(pais == "Ecuador") %>%  # me quedo solo con datos_pais
  select(vector_variables)  %>%  # Me quedo solo las variables del mdoelo
  select(-pais)

lags.select(d.pais)

ecui1 <- TVAR.LRtest(data = d.pais,      # Base de datos (no debe haber vacios)
                     lag = 2,           # Lag para las variables para las variables explicativas
                     thDelay = 1,       # Lag para la variable de umbral
                     mTh = 6,           # Indicador de la columna donde se encuantra la variable de umbral   
                     # nboot = 100,       # numero de repeticiones de bootstrap para calcular el estadistico
                     # hpc = "foreach",   # Paraleización del proceso
                     trim = .1)  

ecui2 <-   TVAR.LRtest(data = d.pais,      # Base de datos (no debe haber vacios)
                       lag = 2,           # Lag para las variables para las variables explicativas
                       thDelay = 1,       # Lag para la variable de umbral
                       mTh = 6,           # Indicador de la columna donde se encuantra la variable de umbral   
                       # nboot = 100,       # numero de repeticiones de bootstrap para calcular el estadistico
                       # hpc = "foreach",   # Paraleización del proceso
                       trim = .1,
                       test = "2vs3") 

ecui1
ecui2




######ELSALVADOR##########

d.pais <- series0 %>% 
  filter(pais == "Salvador") %>%  # me quedo solo con datos_pais
  select(vector_variables)  %>%  # Me quedo solo las variables del mdoelo
  select(-pais)

lags.select(d.pais)
sali1 <- TVAR.LRtest(data = d.pais,      # Base de datos (no debe haber vacios)
                     lag = 1,           # Lag para las variables para las variables explicativas
                     thDelay = 1,       # Lag para la variable de umbral
                     mTh = 6,           # Indicador de la columna donde se encuantra la variable de umbral   
                     # nboot = 100,       # numero de repeticiones de bootstrap para calcular el estadistico
                     # hpc = "foreach",   # Paraleización del proceso
                     trim = .1)  

sali2 <-   TVAR.LRtest(data = d.pais,      # Base de datos (no debe haber vacios)
                       lag = 1,           # Lag para las variables para las variables explicativas
                       thDelay = 1,       # Lag para la variable de umbral
                       mTh = 6,           # Indicador de la columna donde se encuantra la variable de umbral   
                       # nboot = 100,       # numero de repeticiones de bootstrap para calcular el estadistico
                       # hpc = "foreach",   # Paraleización del proceso
                       trim = .1,
                       test = "2vs3") 

sali1
sali2




######PANAMÁ##########

vector_variables <- c(
  "pais",
  #"ipc",
  "ipc_a",
  "deuda",
  "m2",
  #"ri",
  "cr_pib",
  "ief"
  
)
plot.ts(d.pais$ief)

d.pais <- series0 %>% 
  filter(pais == "Panama") %>%  # me quedo solo con datos_pais
  select(vector_variables)  %>%  # Me quedo solo las variables del mdoelo
  select(-pais)

lags.select(d.pais)
pani1 <- TVAR.LRtest(data = d.pais,      # Base de datos (no debe haber vacios)
                     lag = 1,           # Lag para las variables para las variables explicativas
                     thDelay = 1,       # Lag para la variable de umbral
                     mTh = 5,           # Indicador de la columna donde se encuantra la variable de umbral   
                     # nboot = 100,       # numero de repeticiones de bootstrap para calcular el estadistico
                     # hpc = "foreach",   # Paraleización del proceso
                     trim = .1)  

pani2 <-   TVAR.LRtest(data = d.pais,      # Base de datos (no debe haber vacios)
                       lag = 1,           # Lag para las variables para las variables explicativas
                       thDelay = 1,       # Lag para la variable de umbral
                       mTh = 5,           # Indicador de la columna donde se encuantra la variable de umbral   
                       # nboot = 100,       # numero de repeticiones de bootstrap para calcular el estadistico
                       # hpc = "foreach",   # Paraleización del proceso
                       trim = .1,
                       test = "2vs3") 

pani1
pani2




model <- TVAR(d.pais,
              lag = 1,
              nthresh = 1,
              thDelay = 1,
              mTh =5,           # Posición de la variable que determina el regimen
              trim = 0.1,
              dummyToBothRegimes = T,  # Si el modelo debe devolver las dummies del regimen
              trick = "mapply",
              plot = T)
model$nobs_regimes
