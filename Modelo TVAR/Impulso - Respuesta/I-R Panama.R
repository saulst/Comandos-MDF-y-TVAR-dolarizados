library(scales)
library(tidyverse)
library(tsDyn)
library(readxl)
library(janitor)
library(lubridate)
library(tvarGIRF)
library(xlsx)

base <- read_excel("D:/Saúl/TESIS/BASE DE DATOS DOLARIADOS/Paises unidos.xlsx") 


vector_variables <- c(
  #"pais",
  #"ipc",
  "ipc_a",
  "deuda",
  "m2",
  #"ri",
  "cr_pib",
  "IEF"
)

tramos <- function(vector,TH){
  
  n <- length(vector)
  
  inicial <- rep(1,n)
  
  logico <- vector >= TH
  
  m <- 1
  
  for(i in 2:n){
    
    if(logico[i] != logico[i-1] ){
      
      m <- m + 1
      
    }else{
      m <- m
    }
    
    inicial[i] <- m
    
  }
  
  inicial[!logico] <- NA
  
  return(inicial)
}



reporte_panama <- function(base,treshold,vector_variables,pais0){
  
  
  set_pais <- base %>% 
    filter(pais == pais0) %>%
    select(vector_variables) %>% 
    filter(complete.cases(.))
  
  model <- TVAR(set_pais,
                lag = 1,
                nthresh = 1,
                thDelay = 1,
                mTh = treshold,           # Posición de la variable que determina el regimen
                trim = 0.1,
                dummyToBothRegimes = T,  # Si el modelo debe devolver las dummies del regimen
                trick = "mapply",
                gamma
                plot = T)
  
  TH <- getTh(model)
  
  fitted <- fitted.values(model) 
  
  fitted <- bind_rows(set_names(x = rep(list(NA),ncol(fitted)),nm = colnames(fitted)) %>% as_tibble,
                      as_tibble(fitted)) %>% 
    rename_all(~str_c(.,"_fit"))
  
  name_var <- vector_variables[treshold]
  
  
  # 
  serie_th <- set_pais %>%
    bind_cols(fitted) %>%
    mutate(fecha = seq(ymd("2007-01-25"),ymd("2020-06-30"),by = "month")) %>%
    select(fecha,all_of(c(name_var,str_c(name_var,"_fit")))) %>%
    filter(complete.cases(.))
  #   
  # 
  # 
  # 
  # invoke_map(tab = serie_th ,
  #        .f = list(
  #          function(tab)filter_at(.tbl = tab,.vars = names(serie_th)[2],any_vars(.>TH)),
  #          function(tab)filter_at(.tbl = tab,.vars = names(serie_th)[2],any_vars(.<=TH))
  #        ))
  
  
  rects <- mutate_at(.tbl = serie_th,
                     .vars = names(serie_th)[2],
                     list(tramo =  ~ tramos(.,TH))
  ) %>%
    split(.$tramo) %>%
    map(~summarise(.x,xmin = min(fecha),xmax = max(fecha)) %>%
          mutate(ymin = -Inf,ymax = Inf)) %>%
    reduce(bind_rows)
  
  # Poner labels:
  
  titulo <- names(serie_th)[2] %>% str_split("_") %>%
    unlist %>%
    map_at(1,~str_to_title(.x)) %>%
    map_if(.p = nchar(.) <= 3,~str_to_upper(.x)) %>%
    reduce(str_c,sep = " ") %>%
    str_c(pais0,", variable umbral: ",.)
  
  subtitulo <- str_c("Valor umbral: ",TH,"")
  
  
  
  # png(str_c("treshold_",pais,".png"),res = 150,width = 6,height = 5,units = "in")
  serie_indice <- serie_th %>%
    # map(is.Date)
    gather("variable","valor",-fecha) %>%
    mutate_at("valor",list(~./1)) %>% 
    ggplot()+
    geom_line(aes_string(x = "fecha",y = "valor",color = "variable",linetype = "variable"),size = 1.2) +
    scale_y_continuous(labels)+
    scale_x_date(date_breaks = "12 months",date_labels = "%Y")+
    scale_color_manual(values = c("#aba8ad", "#4190f3" ))+
    geom_rect(data=rects,
              aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
              fill="pink",
              alpha=0.5)+
    
    labs(title = titulo,
         subtitle = subtitulo,
         caption = "In the graph, the shaded section corresponds to the periods of high regime.") +
    theme_bw() +
    theme(
      plot.title = element_text(size = 12, face = "bold"),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 90),
      plot.caption = element_text(hjust = 0),
      legend.position = "top",legend.title = element_blank()
    )
  # dev.off()
  
  vector_variables <- vector_variables %>%
    str_subset("pais",negate = T)
  
  
  graficos_impulso <- vector_variables %>%
    map(~{
      
      # shock <- "gdp_growth"
      shock <- .x
      
      vector <- rep(0,length(vector_variables))
      
      
      ind <- which(vector_variables == shock)
      
      vector[ind] <- 1
      
      tablas_girf <- c(1,2) %>%
        map(~ GIRF(tvar = model,shock = vector,H = 20,R = 50,restrict.to = .x) %>%
              pluck("responses") %>%
              mutate(regimen = .x) %>%
              rowid_to_column(var = "Horizonte")
        ) %>%
        reduce(bind_rows)
      
      
      
      
      titulos_panel <- names(set_pais) %>%
        
        
        map(~.x %>% str_split("_") %>%
              unlist %>%
              map_at(1,~str_to_sentence(.x)) %>%
              map_if(.p = nchar(.) == 3,~str_to_upper(.x)) %>%
              reduce(str_c,sep = " ")
            
        ) %>% unlist
      
      titulo_shock <- str_c(pais0,", positive shock \u0394",titulos_panel[ind])
      
      grafico_shock <- tablas_girf %>%
        rename_all(~c("Horizonte",titulos_panel,"regimen")) %>%
        gather(variable,value,-Horizonte,-regimen) %>%
        mutate(Regime = factor(regimen,levels = c(1,2),labels = c("Low","High"))) %>%
        ggplot() +
        geom_line(aes(x = Horizonte,y = value,color = Regime)) +
        facet_grid(.~variable) +
        geom_hline(aes(yintercept = 0)) +
        
        labs(title = titulo_shock) +
        theme_bw() +
        theme(
          plot.title = element_text(size = 12, face = "bold"),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90),
          plot.caption = element_text(hjust = 0),
          legend.position = "top",legend.title = element_text(face = "bold")
        )
      
      return(grafico_shock)
      
    })
  
  return(list(serie = serie_indice,
              impulsos = graficos_impulso))
  
  # return(TH)
  
}



resumen_panama2<-reporte_panama(base=base, 
                                   vector_variables = vector_variables,
                                   pais0 =  "Panama", 
                                   treshold=5)

##hay resumen panama 1 y 2

plot(resumen_panama2$impulsos[[4]])
plot(resumen_panama2$serie)


impulso_ief_panama<-resumen_panama$impulsos[[5]]$data
impulso_ief_panama<-split(impulso_ief_panama, impulso_ief_panama$Regime)

write.xlsx(impulso_ief_panama, "Panama_impulso_ief2.xlsx")

shock_gdp_panama<-resumen_panama$impulsos[[4]]$data
shock_gdp_panama<-split(shock_gdp_panama, shock_gdp_panama$Regime)
write.xlsx(shock_gdp_panama, "Panama_shock_gdp2.xlsx")

