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
  #"ri",
  "cr_pib",
  "ief"
)

series <- series0 %>% 
  select(vector_variables)


datos_pais <- series0 %>% 
  filter(pais == "Panama") %>%  # me quedo solo con datos_pais
  select(vector_variables)  %>%  # Me quedo solo las variables del mdoelo
  select(-pais)
-
  
  datos_pais %>% names

modelo_1 <- TVAR(data = datos_pais,
                 lag = 1,
                 nthresh = 1,
                 thDelay = 1,
                 mTh = 5,           # Posición de la variable que determina el regimen
                 trim = 0.1,
                 dummyToBothRegimes = T,  # Si el modelo debe devolver las dummies del regimen
                 trick = "mapply"
                
                
              
)

plot(modelo_1)
summary(modelo_1)


modelos_tres <- c(tsDyn::getTh(modelo_1),modelo_1$model.specific$allgammas) %>% 
  map(~{
    
    safely(TVAR)(
      data = datos_pais,
      lag = 1,
      nthresh = 1,
      thDelay = 1,
      mTh = 5,           # Posición de la variable que determina el regimen
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
                      L = cbind(0,+1,+1,+1,+1,+1,
                                0,+1,+1,+1,+1,+1,
                                0,+1,+1,+1,+1,+1,
                                0,+1,+1,+1,+1,+1,
                                0,+1,+1,+1,+1,+1,
                                0,+1,+1,+1,+1,+1,
                                0,+1,+1,+1,+1,+1,
                                0,+1,+1,+1,+1,+1,
                                0,+1,+1,+1,+1,+1,
                                0,+1,+1,+1,+1,+1
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

tabla_final_panama <- tibble(
  Country  = "Panama",
  `Treshold Variable` = names(datos_pais)[5],
  `Estimated treshold` = tsDyn::getTh(modelo_1),
  `Best Wald` = str_c(round(real$chi2.chi2,2)," (",round(real$chi2.P,2),")"),
  `Sup Wald` = str_c(round(sup$chi2.chi2,2)," (",round(sup$chi2.P,2),")"),
  `Avg Wald` = str_c(round(avg$chi2.chi2,2)," (",round(avg$chi2.P,2),")")
  
  
) 

modelo_1$nobs_regimes
tabla_final_panama

write.xlsx(tabla_final_panama, "walds_panama2.xlsx")
