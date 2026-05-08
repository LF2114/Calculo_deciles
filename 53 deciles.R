# --------------------------------------
# Luis Felipe Ahumada S.
# Fecha: 050526
# Objetivo: estimar el indice palma (ip)
# --------------------------------------

library(survey)

bases <- map(bases, ~ read_dta(.))
bases <- map(bases, ~ variables(.))
base <- bases[[5]]


#1 calcular los ingresos de corte con el diseño complejo ----------------------- 
design <- svydesign(
  id = ~id_directorio,
  weights = ~fact_cal_esi,
  strata = ~estrato,
  data = base,
  check.strata  = TRUE
)
options(survey.lonely.psu = "remove")

#es necesario filtrar el diseño complejo por ocupados los deciles reflejan la distribución entre quienes generan ingreso.
#si no se filtra se acumulan en la parte baja de la distribución. Los quantiles bajos (q10,q20,...) tienden a ser 0 o muy bajos.
cuts <- svyquantile(
  ~ing_t_p,
  subset(design, ocup_ref == 1),
  quantiles = seq(0.1, 0.9, 0.1),
  na.rm = TRUE,
  ci = FALSE
)
cuts <- as.numeric(unlist(cuts)) %>% print()

base$decil <- cut(
  base$ing_t_p,
  breaks = c(-Inf, cuts, Inf),
  labels = paste0("D", 1:10),
  include.lowest = TRUE, 
  right = FALSE
)

#base$decil

#2 calcular los deciles con svyby ----------------------------------------------
design <- svydesign(
  id = ~id_directorio,
  weights = ~fact_cal_esi,
  strata = ~estrato,
  data = base,
  check.strata  = TRUE
)
options(survey.lonely.psu = "remove")

tab <- 
  svyby(
    ~ing_t_p, 
    ~decil,
    subset(design, ocup_ref == 1),
    svytotal, 
    na.rm = TRUE, 
    keep.var = TRUE
  )

tab

d40 <- sum(tab$ing_t_p[1:4]) %>% print()
d10 <- tab$ing_t_p[10] %>% print()
ip <- (d10/d40) %>% print()
