##############################################
# 00. Simulación de datos para la clase
# Seminario Metodológico 2026
# Última modificación: 19-10-2026
##############################################

# CONTENIDOS ------------------------------------------------------------------
# 1. PREPARACIÓN
# 2. BASE SIMCE 2° MEDIO SIMULADA (MULTINIVEL)
# 3. BASE DE TRANSICIÓN 8° BÁSICO - 1° MEDIO SIMULADA (MULTINIVEL, DOS AÑOS)
# 4. BASES DE MATRÍCULA SIMULADAS 2022-2025 (SUPERVIVENCIA)
# 5. GUARDAR DATOS
#
# NOTA: estos datos son FICTICIOS. Imitan la estructura y los nombres de
# variables de las bases SIMCE y de Mineduc (Matrícula y Rendimiento por
# alumno), para que el código pueda adaptarse a los datos reales cambiando
# solo la ruta y los nombres de variables.
##############################################


# 1. PREPARACIÓN ---------------------------------------------------------------
options(scipen = 999)
rm(list = ls())

library(tidyverse)

set.seed(2026)
dir.create("data", showWarnings = FALSE)


# 2. BASE SIMCE 2° MEDIO SIMULADA (MULTINIVEL) ---------------------------------------

## 2.1 Nivel 2: establecimientos ----
n_rbd <- 200

rbd_data <- tibble(
  rbd           = sample(1000:19999, n_rbd),
  cod_depe2     = sample(1:3, n_rbd, replace = TRUE, prob = c(.40, .50, .10)),
  cod_rural_rbd = rbinom(n_rbd, 1, .15),               # 0 urbano, 1 rural
  n_alu         = round(runif(n_rbd, 10, 60)),           # estudiantes evaluados
  u0            = rnorm(n_rbd, 0, 15),                   # efecto aleatorio escuela
  nse_escuela   = rnorm(n_rbd, 0, 1),                    # composición social (latente)
  prop_mig      = rbeta(n_rbd, 1.2, 8),                  # concentración migrante
  practicas_esc = rnorm(n_rbd, 0, .4)                    # clima de enseñanza del establecimiento
) %>%
  mutate(
    # establecimientos particulares pagados: mayor NSE, menor proporción migrante
    nse_escuela = nse_escuela + case_when(cod_depe2 == 3 ~ 1.5,
                                          cod_depe2 == 2 ~ .3,
                                          TRUE ~ -.3),
    prop_mig    = if_else(cod_depe2 == 3, prop_mig / 3, prop_mig)
  )

## 2.2 Nivel 1: estudiantes ----
simce <- rbd_data %>%
  uncount(n_alu) %>%
  mutate(
    mrun      = sample(1e6:9e6, n(), replace = FALSE),
    gen_alu   = sample(1:2, n(), replace = TRUE),        # 1 hombre, 2 mujer
    migrante  = rbinom(n(), 1, prop_mig),                # 0 nativo, 1 migrante
    ed_padres = round(pmin(pmax(12 + 2 * nse_escuela + rnorm(n(), 0, 2.5) -
                                  .8 * migrante, 0), 20)),
    # Índice de prácticas docentes percibidas (cuestionario de estudiantes, 1 a 4)
    practicas_lat = practicas_esc + rnorm(n(), 0, .9),
    practicas = round(pmin(pmax(2.8 + .5 * practicas_lat, 1), 4), 2),
    ptje_mate2m_alu = 250 +
      3.5 * (ed_padres - 12) +                            # efecto individual
      8   * nse_escuela +                                  # efecto composicional
      -4  * (gen_alu == 2) +
      -9  * migrante +
      # efecto de prácticas, más fuerte en establecimientos de menor NSE
      (7 - 4 * nse_escuela) * practicas_lat * .5 * 2 +
      8   * (cod_depe2 == 2) + 18 * (cod_depe2 == 3) +
      -6  * cod_rural_rbd +
      u0 +
      rnorm(n(), 0, 42),
    ptje_mate2m_alu = round(pmin(pmax(ptje_mate2m_alu, 120), 400), 1)
  ) %>%
  # valores perdidos en escolaridad de los padres (cuestionario de padres)
  mutate(ed_padres = if_else(runif(n()) < .05, NA_real_, ed_padres)) %>%
  select(mrun, rbd, gen_alu, migrante, ed_padres, practicas, ptje_mate2m_alu,
         cod_depe2, cod_rural_rbd)


# 3. BASE DE TRANSICIÓN 8° BÁSICO (2024) - 1° MEDIO (2025) -----------------------------
# Todos los estudiantes cambian de establecimiento entre ambos años.

## 3.1 Establecimientos de origen (básica) y de destino (media) ----
origen <- tibble(
  RBD       = sample(30000:39999, 150),
  RURAL_RBD = rbinom(150, 1, .45),
  COD_DEPE2 = sample(1:2, 150, replace = TRUE, prob = c(.6, .4)),
  IVE       = round(pmin(pmax(rnorm(150, 80, 10), 40), 100), 1),
  u_orig    = rnorm(150, 0, 2.5)
) %>% mutate(IVE = if_else(RURAL_RBD == 1, pmin(IVE + 5, 100), IVE))

destino <- tibble(
  RBD       = sample(40000:49999, 90),
  RURAL_RBD = 0,
  COD_DEPE2 = sample(1:3, 90, replace = TRUE, prob = c(.45, .45, .10)),
  IVE       = round(pmin(pmax(rnorm(90, 70, 15), 20), 100), 1),
  u_dest    = rnorm(90, 0, 3)
)

## 3.2 Estudiantes ----
n_tr <- 1800
tr <- tibble(
  MRUN    = sample(5e7:6e7, n_tr),
  GEN_ALU = sample(1:2, n_tr, replace = TRUE),
  rbd_o   = sample(origen$RBD, n_tr, replace = TRUE),
  # los establecimientos de destino de mayor tamaño reciben más estudiantes
  rbd_d   = sample(destino$RBD, n_tr, replace = TRUE, prob = rexp(90))
) %>%
  left_join(origen %>% rename_with(~ paste0(.x, "_o"), -RBD), by = c("rbd_o" = "RBD")) %>%
  left_join(destino %>% rename_with(~ paste0(.x, "_d"), -RBD), by = c("rbd_d" = "RBD")) %>%
  mutate(
    mujer = as.integer(GEN_ALU == 2),
    asis_2024 = 90 - .10 * (IVE_o - 80) + u_orig_o + 1 * mujer + rnorm(n(), 0, 5),
    # cambio en la asistencia: cae más para quienes vienen de escuelas rurales
    # y cuando el establecimiento de destino es más vulnerable
    asis_2025 = 30 + .62 * asis_2024 - 2.5 * RURAL_RBD_o - .08 * (IVE_d - 70) +
      1.5 * mujer + 1.5 * (COD_DEPE2_d == 2) + u_dest_d + .8 * u_orig_o + rnorm(n(), 0, 5),
    nota_2024 = 5.6 - .010 * (IVE_o - 80) + .15 * mujer + u_orig_o / 10 + rnorm(n(), 0, .5),
    nota_2025 = 1.4 + .70 * nota_2024 - .20 * RURAL_RBD_o - .005 * (IVE_d - 70) +
      .10 * mujer + u_dest_d / 10 + rnorm(n(), 0, .45),
    across(c(asis_2024, asis_2025), ~ round(pmin(pmax(.x, 30), 100), 1)),
    across(c(nota_2024, nota_2025), ~ round(pmin(pmax(.x, 1), 7), 1))
  )

# Formato largo, como los archivos anuales de Mineduc: una fila por estudiante y año
transicion <- bind_rows(
  tr %>% transmute(AGNO = 2024, MRUN, GEN_ALU, RBD = rbd_o, RURAL_RBD = RURAL_RBD_o,
                   COD_DEPE2 = COD_DEPE2_o, IVE = IVE_o, COD_ENSE = 110, COD_GRADO = 8,
                   ASISTENCIA = asis_2024, PROM_MATE = nota_2024),
  tr %>% transmute(AGNO = 2025, MRUN, GEN_ALU, RBD = rbd_d, RURAL_RBD = RURAL_RBD_d,
                   COD_DEPE2 = COD_DEPE2_d, IVE = IVE_d, COD_ENSE = 310, COD_GRADO = 1,
                   ASISTENCIA = asis_2025, PROM_MATE = nota_2025)
) %>% arrange(MRUN, AGNO)


# 4. BASES DE MATRÍCULA SIMULADAS 2022-2025 (SUPERVIVENCIA) -------------------------

## 4.1 Establecimientos de enseñanza media ----
n_liceos <- 60
liceos <- tibble(
  RBD       = sample(20000:29999, n_liceos),
  COD_DEPE2 = sample(1:3, n_liceos, replace = TRUE, prob = c(.45, .45, .10)),
  RURAL_RBD = rbinom(n_liceos, 1, .10),
  COD_ENSE  = sample(c(310, 410), n_liceos, replace = TRUE, prob = c(.6, .4))  # HC / TP
)

## 4.2 Cohorte: estudiantes en 1° medio en 2022 ----
n_cohorte <- 3000
paises_mig <- c(27, 22, 8, 16, 4, 29)          # Venezolana, Peruana, Colombiana,
prob_mig   <- c(.40, .15, .15, .10, .10, .10)  # Haitiana, Boliviana, Otra

cohorte <- tibble(
  MRUN    = sample(1e7:2e7, n_cohorte),
  RBD     = sample(liceos$RBD, n_cohorte, replace = TRUE),
  GEN_ALU = sample(1:2, n_cohorte, replace = TRUE),
  migr    = rbinom(n_cohorte, 1, .15)
) %>%
  left_join(liceos, by = "RBD") %>%
  mutate(
    PAIS_ORIGEN_ALU = if_else(migr == 1,
                              sample(paises_mig, n(), replace = TRUE, prob = prob_mig),
                              6),                                  # 6 = Chilena
    PAIS_ORIGEN_ALU = if_else(runif(n()) < .02, 0, PAIS_ORIGEN_ALU), # sin información
    COD_NAC_ALU     = case_when(PAIS_ORIGEN_ALU == 6 ~ "C",
                                PAIS_ORIGEN_ALU == 0 ~ "C",
                                runif(n()) < .15     ~ "N",
                                TRUE                 ~ "E"),
    extraedad = rbinom(n(), 1, if_else(migr == 1, .35, .12)),
    EDAD_ALU  = 15 + extraedad + rbinom(n(), 1, .1),
    # Estudiantes extranjeros sin RUN: su MRUN corresponde a la máscara de un IPE
    es_ipe   = COD_NAC_ALU == "E" & runif(n()) < .45,
    MRUN     = if_else(es_ipe, sample(7e7:8e7, n()), MRUN),
    MRUN_IPE = NA_real_
  )

## 4.3 Trayectorias 2022-2025 ----
# Riesgo anual de deserción (logit) según características
riesgo <- function(d, t) {
  plogis(-3.6 + c(.2, 0, -.2)[t] +
           .30 * (d$GEN_ALU == 1) +
           .55 * d$migr +
           .70 * d$extraedad +
           .35 * (d$COD_DEPE2 == 1) - .80 * (d$COD_DEPE2 == 3) +
           .25 * (d$COD_ENSE == 410))
}

tray <- cohorte %>% mutate(grado = 1, activo = TRUE, salida_def = FALSE)
matricula <- list()
matricula[["2022"]] <- tray %>% mutate(AGNO = 2022, COD_GRADO = grado)

for (t in 1:3) {
  anio <- 2022 + t
  sale   <- tray$activo & runif(nrow(tray)) < riesgo(tray, t)
  # ~15% de quienes salieron el año anterior reingresan (salidas transitorias)
  vuelve <- !tray$activo & !tray$salida_def & runif(nrow(tray)) < .15
  tray <- tray %>%
    mutate(
      salida_def = salida_def | (!activo & !vuelve),
      activo     = (activo & !sale) | vuelve,
      repite     = activo & runif(n()) < if_else(migr == 1, .07, .04),
      grado      = if_else(activo & !repite & !vuelve, pmin(grado + 1, 4), grado),
      # algunos cambian de establecimiento (no es deserción)
      RBD        = if_else(activo & runif(n()) < .08,
                           sample(liceos$RBD, n(), replace = TRUE), RBD),
      EDAD_ALU   = EDAD_ALU + 1,
      # ~20% anual de estudiantes con IPE obtiene RUN: cambia su MRUN y
      # el IPE anterior queda registrado en MRUN_IPE (desde ese año en adelante)
      regulariza = activo & es_ipe & runif(n()) < .20,
      MRUN_IPE   = if_else(regulariza, MRUN, MRUN_IPE),
      MRUN       = if_else(regulariza, sample(8e7:9e7, n()), MRUN),
      es_ipe     = es_ipe & !regulariza
    ) %>%
    select(-COD_DEPE2, -RURAL_RBD, -COD_ENSE) %>%
    left_join(liceos, by = "RBD")
  matricula[[as.character(anio)]] <- tray %>%
    filter(activo) %>%
    mutate(AGNO = anio, COD_GRADO = grado)
}

## 4.4 Otros estudiantes (otros niveles) presentes en cada archivo anual ----
# Los archivos reales contienen a TODOS los estudiantes; hay que filtrar la cohorte.
otros <- function(anio, n = 4000) {
  tibble(
    AGNO      = anio,
    MRUN      = sample(3e7:4e7, n),
    MRUN_IPE  = NA_real_,
    RBD       = sample(liceos$RBD, n, replace = TRUE),
    # en 2022, 1° medio corresponde solo a la cohorte; los demás están en 2° a 4°
    COD_GRADO = if (anio == 2022) sample(2:4, n, replace = TRUE) else sample(1:4, n, replace = TRUE),
    GEN_ALU   = sample(1:2, n, replace = TRUE),
    EDAD_ALU  = 14 + COD_GRADO + rbinom(n, 1, .15),
    PAIS_ORIGEN_ALU = if_else(runif(n) < .13,
                              sample(paises_mig, n, replace = TRUE, prob = prob_mig), 6),
    COD_NAC_ALU = if_else(PAIS_ORIGEN_ALU == 6, "C", "E")
  ) %>%
    left_join(liceos, by = "RBD")
}

vars_mat <- c("AGNO", "RBD", "COD_DEPE2", "RURAL_RBD", "COD_ENSE", "COD_GRADO",
              "MRUN", "MRUN_IPE", "GEN_ALU", "EDAD_ALU", "COD_NAC_ALU", "PAIS_ORIGEN_ALU")

matricula <- map(names(matricula), function(a) {
  bind_rows(matricula[[a]], otros(as.numeric(a))) %>%
    select(all_of(vars_mat)) %>%
    slice_sample(prop = 1)
}) %>% set_names(names(matricula))


# 5. GUARDAR DATOS -----------------------------------------------------------------
save(simce, file = "data/simce_simulada.RData")

# Formato similar a los archivos de Mineduc: CSV separado por punto y coma
write_csv2(transicion, "data/rendimiento_2024_2025_simulada.csv", na = "")

walk(names(matricula), function(a) {
  write_csv2(matricula[[a]], paste0("data/matricula_", a, "_simulada.csv"), na = "")
})
