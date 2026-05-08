# --------------------------------------
# Luis Felipe Ahumada S.
# Fecha: 050526
# Objetivo: estimar el indice palma (ip)
# --------------------------------------

#warning: es importante que los deciles de ingresos se construyan filtrando por ocupados (ocup_ref == 1) por cuanto si no se hace, 
#         se acumularan los ingresos 0 o bajos en los primeros deciles. 

bases <- map(bases, ~ read_dta(.))
bases <- map(bases, ~ variables(.))

#1 funciones -------------------------------------------------------------------
cortes <- function(data){
  
  design <- svydesign(
    id = ~id_directorio,
    weights = ~fact_cal_esi,
    strata = ~estrato,
    data = data,
    check.strata  = TRUE
  )
  options(survey.lonely.psu = "remove")
  
  cuts <- svyquantile(
    ~ing_t_p,
    subset(design, ocup_ref == 1),
    quantiles = seq(0.1, 0.9, 0.1),
    na.rm = TRUE,
    ci = FALSE
  )
  
  cuts <- as.numeric(unlist(cuts)) 
  return(cuts)
}

dec <- function(data, cuts){
  
  data$decil <- 
    cut(data$ing_t_p,
        breaks = c(-Inf, cuts, Inf),
        labels = paste0("D", 1:10),
        include.lowest = TRUE, #los valores minimos son incluidos
        right = FALSE          #los empates caen en el decil superior; TRUE caen en el inferior 
        )  
  
  return(data)
}

decil <- function(data,masa){
  
  #2 calcular los deciles con svyby --------------------------------------------
  design <- 
    svydesign(id = ~id_directorio,
              weights = ~fact_cal_esi,
              strata = ~estrato,
              data = data,
              check.strata  = TRUE
    )
  options(survey.lonely.psu = "remove")
  
  tab <- 
    svyby(reformulate(masa), 
          ~decil,
          subset(design, ocup_ref == 1),
          svytotal, 
          na.rm = TRUE, 
          keep.var = TRUE
    )
  
  tab %<>%
    mutate(anio = data$ano_encuesta[1:10])
  
  return(tab)
  
}

#2 calculos --------------------------------------------------------------------
cuts <- map(bases, ~ cortes(.)) 
bases <- 
  map2(bases,
       cuts,
       ~ dec(data = .x,
             cuts = .y))


deciles_ing <- map(bases,
               ~ decil(data = .x,
                       masa = "ing_t_p"))

deciles_ocu <- map(bases,
                   ~decil(data = .x,
                          masa = "total"))




#3 almacenamiento -----------------------------------------------------
tab_deciles <- tibble("anio" = numeric(),
                      "g1" = numeric(),
                      "g2" = numeric(),
                      "G" = numeric(),
                      
                      "d10" = numeric(),
                      "sd40" = numeric(),
                      "ip" = numeric(),
                      
                      "sd90" = numeric(),
                      "sd70" = numeric(),
                      "G1" = numeric()
                      )

for (i in seq_along(bases)) {
  
  anio <- deciles_ing[[i]]$anio[1]
  
  #comparacion d10/sd40
  d10 <- deciles_ing[[i]]$ing_t_p[10]
  sd40 <- sum(deciles_ing[[i]]$ing_t_p[1:4])
  ip <- d10/sd40

  #comparacion decil5:9 contra d10:c(d1:d4)
  g2 <- sum(d10,sd40)
  g1 <- sum(deciles_ing[[i]]$ing_t_p[5:9])
  G <- g1/g2
  
  #comparacion decil8:9 contra decil 5:7
  sd90 <- sum(deciles_ing[[i]]$ing_t_p[8:9])
  sd70 <- sum(deciles_ing[[i]]$ing_t_p[5:7])
  G1 <- sd90/sd70
         
  tab_deciles[i,"anio"] <- anio
  tab_deciles[i,"g1"]   <- g1
  tab_deciles[i,"g2"]   <- g2
  tab_deciles[i,"G"]    <- G

  tab_deciles[i,"d10"]  <- d10
  tab_deciles[i,"sd40"] <- sd40
  tab_deciles[i,"ip"]   <- ip

  tab_deciles[i,"sd90"] <- sd90
  tab_deciles[i,"sd70"] <- sd70
  tab_deciles[i,"G1"]   <- G1
  
}

tab_deciles
#1 G: division masa ingresos entre decil5:9 = g1 (grupo 1) / (decil10 + decil1:4 = sd40) = g2 (grupo 2)
#2 ip: dentro del grupo 2, ip es la division decil 10 = d10 y deciles1:4 = sd40
#3 G1: dentro del grupo 1, ip3 es la division decil8:9 = sd90/ decil5:7 = sd70 


#4 Exportar --------------------------------------------------------------------
deciles_ing <- bind_rows(deciles_ing)
deciles_ocu <- bind_rows(deciles_ocu)

deciles <- left_join(deciles_ing %>% select(-se),
                     deciles_ocu %>% select(-se), by = c("decil","anio")) %>% 
  relocate("anio", .before = decil)


#agregar a deciles los cortes
c20 <- data.frame("anio" = 2020,
                  "cutof" = c(0,cuts[[1]]))
c21 <- data.frame("anio" = 2021,
                  "cutof" = c(0,cuts[[2]]))                
c22 <- data.frame("anio" = 2022,
                  "cutof" = c(0,cuts[[3]]))
c23 <- data.frame("anio" = 2023,
                  "cutof" = c(0,cuts[[4]]))
c24 <- data.frame("anio" = 2024,
                  "cutof" = c(0,cuts[[5]]))

cutsdf <- bind_rows(c20,c21,c22,c23,c24)
deciles <- bind_cols(deciles, cutsdf %>% select(cutof))


#agregar el rnp por base de los deciles
recuento <- function(data){
  data %>% 
    filter(ocup_ref == 1) %>% 
    group_by(decil) %>% 
    summarise(rnp = n())
}

tab_rnp <- 
  map(bases, 
    ~ recuento(.)) 

tab_rnp <- bind_rows(tab_rnp)
deciles <- bind_cols(deciles,tab_rnp %>% select(rnp))


#5 excel
wb <- createWorkbook()
addWorksheet(wb,"deciles")
addWorksheet(wb,"tab_deciles")

writeData(wb,"deciles",deciles)
writeData(wb,"tab_deciles",tab_deciles)

saveWorkbook(wb,
             file = "indicadores.xlsx",
             overwrite = T)

