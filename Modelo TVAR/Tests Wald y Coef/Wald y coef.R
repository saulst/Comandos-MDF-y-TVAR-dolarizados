library(tsDyn)
library(tvarGIRF)
library(tidyverse)
library(readxl)
# library(zoo)
library(lubridate)
library(gridExtra)
# library(grid)
library(janitor)
library(tsDyn)
# library(gt)
# library(broom)
library(aod)
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

walds2 <- function(pais0,variable){
  
  datos_pais <- series0 %>% 
    filter(pais == pais0) %>%  # me quedo solo con datos_pais
    select(vector_variables)  %>%  # Me quedo solo las variables del mdoelo
    select(-pais)
  
  
  datos_pais %>% names
  
  modelo_1 <- TVAR(data = datos_pais,
                   lag = 2,
                   nthresh = 1,
                   thDelay = 1,
                   mTh = variable,           # Posición de la variable que determina el regimen
                   trim = 0.1,
                   dummyToBothRegimes = T,  # Si el modelo debe devolver las dummies del regimen
                   trick = "mapply"
  )
  
  summary(modelo_1)
  
  
  modelos_tres <- c(tsDyn::getTh(modelo_1),modelo_1$model.specific$allgammas) %>% 
    map(~{
      
      safely(TVAR)(
        data = datos_pais,
        lag = 2,
        nthresh = 1,
        thDelay = 1,
        mTh = variable,           # Posición de la variable que determina el regimen
        trim = 0.1,
        dummyToBothRegimes = T,  # Si el modelo debe devolver las dummies del regimen
        trick = "mapply",
        gamma = .x)
      # 
      # .x
      
      # aa <- tsDyn::getTh(modelo$result)
      
      
      
    })
  
  
  var <-   modelos_tres %>% 
    map("result") %>% 
    discard(~is.null(.x))
  
 
  # aa <- var[[1]]
  
  # bb<- summary(aa)
  
  # browser()
  
  modelos_tres <- var %>% 
    # %>% 
    map(~{
      
      
      resumen <- summary(.x)
      
      var_cov <- resumen$VarCov
      
      var_cov_wald <- var_cov[[1]]
      
      coeff_wald <- c(
        coefficients(.x)[[1]],
        coefficients(.x)[[2]]
      )
      
      # browser()
      
      safely(wald.test)(Sigma = var_cov_wald,
                        b = coeff_wald,
                        L = cbind(0,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1,+1
                        ) )
      
    }
    
    )
  
  
  
  
  
  walds_pre <- modelos_tres  %>% 
    map("result") %>% 
    map("result") %>% 
    discard(~is.null(.x)) %>% 
    map(unlist) %>% 
    reduce(rbind) %>% 
    as_tibble
  
  
  sup <- walds_pre[which.max(walds_pre$chi2.chi2),]
  
  avg <- tibble(chi2.chi2 = mean(walds_pre$chi2.chi2),chi2.P = mean(walds_pre$chi2.P))
  
  real <- walds_pre[1,]
  
  tabla_final <- tibble(
    Country  = pais0,
    `Treshold Variable` = names(datos_pais)[variable],
    `Estimated treshold` = tsDyn::getTh(modelo_1),
    `Best Wald` = str_c(round(real$chi2.chi2,2)," (",round(real$chi2.P,2),")"),
    `Sup Wald` = str_c(round(sup$chi2.chi2,2)," (",round(sup$chi2.P,2),")"),
    `Avg Wald` = str_c(round(avg$chi2.chi2,2)," (",round(avg$chi2.P,2),")")
    
    
  ) 
  
  tabla_final
}


tabla_pais2 <- function(pais){
  
  map2(rep(pais,6),1:6,~walds2(.x,.y)) %>% 
    reduce(bind_rows)
}

walds2("Ecuador", 6)


write.xlsx(tabla_pais2("Ecuador"), "walds ecuador.xlsx")


walds1 <- function(pais0,variable){
  
  datos_pais <- series0 %>% 
    filter(pais == pais0) %>%  # me quedo solo con datos_pais
    select(vector_variables)  %>%  # Me quedo solo las variables del mdoelo
    select(-pais)
  
  # El test de linearidad nos dio los siguiotes parametros:
  
  # 
  # lag = 1 Lag para las variables para las variables explicativas
  # thDelay = 1 Lag para la variable de umbral
  # mTh = tres regimenes: "gdp_growth","liquid_assets_to_short_term_liabilities","inflation_regime_ipc"
  # trick = "mapply" Es un metodo computacional para acelerar la estimación
  
  # 1. Modelo, regimen: gdp -------------------------------------------------
  
  datos_pais %>% names
  
  modelo_1 <- TVAR(data = datos_pais,
                   lag = 1,
                   nthresh = 1,
                   thDelay = 1,
                   mTh = variable,           # Posición de la variable que determina el regimen
                   trim = 0.1,
                   dummyToBothRegimes = T,  # Si el modelo debe devolver las dummies del regimen
                   trick = "mapply"
  )
  
  summary(modelo_1)
  
  
  modelos_tres <- c(tsDyn::getTh(modelo_1),modelo_1$model.specific$allgammas) %>% 
    map(~{
      
      safely(TVAR)(
        data = datos_pais,
        lag = 1,
        nthresh = 1,
        thDelay = 1,
        mTh = variable,           # Posición de la variable que determina el regimen
        trim = 0.1,
        dummyToBothRegimes = T,  # Si el modelo debe devolver las dummies del regimen
        trick = "mapply",
        gamma = .x)
      # 
      # .x
      
      # aa <- tsDyn::getTh(modelo$result)
      
      
      
    })
  
  
  var <-   modelos_tres %>% 
    map("result") %>% 
    discard(~is.null(.x))
  
  
  # aa <- var[[1]]
  
  # bb<- summary(aa)
  
  # browser()
  
  modelos_tres <- var %>% 
    # %>% 
    map(~{
      
      
      resumen <- summary(.x)
      
      var_cov <- resumen$VarCov
      
      var_cov_wald <- var_cov[[1]]
      
      coeff_wald <- c(
        coefficients(.x)[[1]],
        coefficients(.x)[[2]]
      )
      
      # browser()
      
      safely(wald.test)(Sigma = var_cov_wald,
                        b = coeff_wald,
                        L = cbind(0,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1,
                                  0,+1,+1,+1,+1,+1,+1) )
    }
    
    )
  
  
  
  
  
  walds_pre <- modelos_tres  %>% 
    map("result") %>% 
    map("result") %>% 
    discard(~is.null(.x)) %>% 
    map(unlist) %>% 
    reduce(rbind) %>% 
    as_tibble
  
  
  sup <- walds_pre[which.max(walds_pre$chi2.chi2),]
  
  avg <- tibble(chi2.chi2 = mean(walds_pre$chi2.chi2),chi2.P = mean(walds_pre$chi2.P))
  
  real <- walds_pre[1,]
  
  tabla_final <- tibble(
    Country  = pais0,
    `Treshold Variable` = names(datos_pais)[variable],
    `Estimated treshold` = tsDyn::getTh(modelo_1),
    `Best Wald` = str_c(round(real$chi2.chi2,2)," (",round(real$chi2.P,2),")"),
    `Sup Wald` = str_c(round(sup$chi2.chi2,2)," (",round(sup$chi2.P,2),")"),
    `Avg Wald` = str_c(round(avg$chi2.chi2,2)," (",round(avg$chi2.P,2),")")
    
    
  ) 
  
  tabla_final
}

tabla_pais1 <- function(pais){
  
  map2(rep(pais,6),1:6,~walds1(.x,.y)) %>% 
    reduce(bind_rows)
}




######################COEFICIENTES###########


matriz_coeficientes2 <- function(pais0,variable){
  
  datos_pais <- series0 %>% 
    filter(pais == pais0) %>%  # me quedo solo con datos_pais
    select(vector_variables)  %>%  # Me quedo solo las variables del mdoelo
    select(-pais)
  
  # El test de linearidad nos dio los siguiotes parametros:
  
  
  datos_pais %>% names
  
  modelo_1 <- TVAR(data = datos_pais,
                   lag = 2,
                   nthresh = 1,
                   thDelay = 1,
                   mTh = variable,           # Posición de la variable que determina el regimen
                   trim = 0.1,
                   dummyToBothRegimes = T,  # Si el modelo debe devolver las dummies del regimen
                   trick = "mapply"
  )
  
  
  resumen <- summary(modelo_1)
  
  
  
  list(
    coefficients(modelo_1)[[1]],
    resumen$Pvalues[[1]],
    coefficients(modelo_1)[[2]],
    resumen$Pvalues[[2]]
  ) %>% 
    map(t) %>% 
    map(~{
      tibble(Parameter = rownames(.x)) %>% 
        bind_cols(as_tibble(.x)) %>% 
        gather("Ecuation","Value",-Parameter)
    }) %>% 
    reduce(right_join,by = c("Parameter","Ecuation")) %>% 
    mutate_if(is.numeric,round,digits = 4) %>% 
    rename_at(c("Value.x" ,"Value.y" ,"Value.x.x" ,"Value.y.y"),
              ~c("Parametro Regimen Bajo","P-valor Regimen Bajo",
                 "Parametro Regimen Alto","P-valor Regimen Alto")
              
    )
  
  
  
}

a<-matriz_coeficientes2("Ecuador", 6)
write.xlsx(a, "coef tvar ecuador.xlsx")

matriz_coeficientes1 <- function(pais0,variable){
  
  datos_pais <- series0 %>% 
    filter(pais == pais0) %>%  # me quedo solo con datos_pais
    select(vector_variables)  %>%  # Me quedo solo las variables del mdoelo
    select(-pais)
  
  # El test de linearidad nos dio los siguiotes parametros:
  
  
  datos_pais %>% names
  
  modelo_1 <- TVAR(data = datos_pais,
                   lag = 1,
                   nthresh = 1,
                   thDelay = 1,
                   mTh = variable,           # Posición de la variable que determina el regimen
                   trim = 0.1,
                   dummyToBothRegimes = T,  # Si el modelo debe devolver las dummies del regimen
                   trick = "mapply"
  )
  
  
  resumen <- summary(modelo_1)
  
  
  
  list(
    coefficients(modelo_1)[[1]],
    resumen$Pvalues[[1]],
    coefficients(modelo_1)[[2]],
    resumen$Pvalues[[2]]
  ) %>% 
    map(t) %>% 
    map(~{
      tibble(Parameter = rownames(.x)) %>% 
        bind_cols(as_tibble(.x)) %>% 
        gather("Ecuation","Value",-Parameter)
    }) %>% 
    reduce(right_join,by = c("Parameter","Ecuation")) %>% 
    mutate_if(is.numeric,round,digits = 4) %>% 
    rename_at(c("Value.x" ,"Value.y" ,"Value.x.x" ,"Value.y.y"),
              ~c("Parametro Regimen Bajo","P-valor Regimen Bajo",
                 "Parametro Regimen Alto","P-valor Regimen Alto")
              
    )
  
  
  
}

matriz_coeficientes_salvador <- function(pais0,variable){
  
  datos_pais <- series0 %>% 
    filter(pais == pais0) %>%  # me quedo solo con datos_pais
    select(vector_variables)  %>%  # Me quedo solo las variables del mdoelo
    select(-pais)
  
  # El test de linearidad nos dio los siguiotes parametros:
  
  
  datos_pais %>% names
  
  modelo_1 <- TVAR(data = datos_pais,
                   lag = 1,
                   nthresh = 1,
                   thDelay = 1,
                   mTh = variable,           # Posición de la variable que determina el regimen
                   trim = 0.1,
                   dummyToBothRegimes = T,  # Si el modelo debe devolver las dummies del regimen
                   trick = "mapply",
                   gamma=0.526787892
  )
  
  
  resumen <- summary(modelo_1)
  
  
  
  list(
    coefficients(modelo_1)[[1]],
    resumen$Pvalues[[1]],
    coefficients(modelo_1)[[2]],
    resumen$Pvalues[[2]]
  ) %>% 
    map(t) %>% 
    map(~{
      tibble(Parameter = rownames(.x)) %>% 
        bind_cols(as_tibble(.x)) %>% 
        gather("Ecuation","Value",-Parameter)
    }) %>% 
    reduce(right_join,by = c("Parameter","Ecuation")) %>% 
    mutate_if(is.numeric,round,digits = 4) %>% 
    rename_at(c("Value.x" ,"Value.y" ,"Value.x.x" ,"Value.y.y"),
              ~c("Parametro Regimen Bajo","P-valor Regimen Bajo",
                 "Parametro Regimen Alto","P-valor Regimen Alto")
              
    )
  
  
  
}

s<-matriz_coeficientes_salvador("Salvador",6)
write.xlsx(s, "coef tvar salvador.xlsx")



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


p<-matriz_coeficientes1("Panama",5)
write.xlsx(p, "coef tvar panama.xlsx")
