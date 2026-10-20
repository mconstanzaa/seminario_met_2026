##############################################
# 01. Manipulación de datos: SIMCE y Mineduc
# Seminario Metodológico 2026
# Última modificación: 19-10-2026
##############################################

# CONTENIDOS ------------------------------------------------------------------
# 1. PREPARACIÓN
# 2. LEER BASES (RData, csv con ";" de Mineduc)
# 3. EXPLORAR LA BASE
# 4. FILTRAR CASOS
# 5. RECODIFICAR Y CREAR VARIABLES
# 6. VALORES PERDIDOS
# 7. UNIR BASES (merge / join)
# 8. AGREGAR INFORMACIÓN AL NIVEL DEL ESTABLECIMIENTO
# 9. FORMATO LARGO Y ANCHO (bases de varios años)
# 10. GUARDAR LA BASE ANALÍTICA
#
# CÓMO USAR ESTE SCRIPT
# - Las rutas son relativas (data/..., output/...): parten desde la CARPETA DE
#   TRABAJO. Abran su proyecto de RStudio si tienen uno o, si no, indiquen la
#   carpeta donde guardaron los materiales (ver sección 1).
# - Los ejemplos usan datos SIMULADOS (carpeta data/).
# - Las líneas marcadas con  # >>> ADAPTAR  son las que deben cambiar
#   para trabajar con SU base (ruta, nombres de variables, códigos).
# - Siempre revisen el LIBRO DE CÓDIGOS de su base antes de recodificar.
##############################################


# 1. PREPARACIÓN ---------------------------------------------------------------
options(scipen = 999)   # evita la notación científica (ej. 1e+06)
rm(list = ls())         # limpia el ambiente de trabajo

# Carpeta de trabajo: debe contener la carpeta data/ (output/ se crea sola).
# Si no usan un proyecto de RStudio, indíquenla con:
# Session > Set Working Directory > Choose Directory, o con setwd():
# setwd("C:/Users/su_usuario/Documents/seminario")                 # >>> ADAPTAR
getwd()   # muestra la carpeta de trabajo actual

# Si no tienen instalado un paquete, ejecuten una vez:
# install.packages("tidyverse")
library(tidyverse)      # dplyr, tidyr, readr, ggplot2, etc.

dir.create("output", showWarnings = FALSE)   # carpeta para guardar resultados


# 2. LEER BASES ------------------------------------------------------------------

## 2.1 Base en formato R (.RData) ----
# load() carga el objeto con el nombre con que fue guardado (aquí: simce)
load("data/simce_simulada.RData")                      # >>> ADAPTAR ruta

## 2.2 Bases de Mineduc (.csv separados por punto y coma) ----
# read_csv2() lee archivos separados por ";" y con coma decimal.
# Si su archivo usa "," como separador, usen read_csv().
mat_2022 <- read_csv2("data/matricula_2022_simulada.csv")   # >>> ADAPTAR ruta

# Para leer varios años de una vez y apilarlos en una sola base:
matricula <- map_dfr(2022:2025, function(anio) {
  read_csv2(paste0("data/matricula_", anio, "_simulada.csv"),   # >>> ADAPTAR nombre de archivo
            # columnas casi siempre vacías: declarar su tipo para no perder valores
            col_types = cols(MRUN_IPE = col_double()))
})

## 2.3 Otros formatos frecuentes (referencia) ----
# library(haven);  base <- read_dta("data/archivo.dta")   # Stata
# library(haven);  base <- read_sav("data/archivo.sav")   # SPSS
# library(readxl); base <- read_excel("data/archivo.xlsx")


# 3. EXPLORAR LA BASE -------------------------------------------------------------
dim(simce)            # número de filas (casos) y columnas (variables)
names(simce)          # nombres de las variables
glimpse(simce)        # tipo de cada variable y primeros valores
summary(simce)        # resumen de todas las variables (incluye NA)

# Frecuencias de una variable categórica
table(simce$cod_depe2, useNA = "ifany")
simce %>% count(cod_depe2)

# ¿Cuántos estudiantes y establecimientos hay?
simce %>% summarise(n_estudiantes = n_distinct(mrun),
                    n_establecimientos = n_distinct(rbd))

# ¿Hay estudiantes repetidos? (debería ser 0 en una base de un año)
sum(duplicated(simce$mrun))


# 4. FILTRAR CASOS -----------------------------------------------------------------
# filter() conserva las filas que cumplen la condición

# Ejemplo: solo establecimientos municipales (cod_depe2 == 1)
simce_municipal <- simce %>% filter(cod_depe2 == 1)

# Varias condiciones a la vez: & (y), | (o), %in% (pertenece a un conjunto)
simce_publico_urbano <- simce %>%
  filter(cod_depe2 %in% c(1), cod_rural_rbd == 0)

# Ejemplo con matrícula: estudiantes de 1° medio (HC y TP) en 2022
primero_medio_2022 <- matricula %>%
  filter(AGNO == 2022, COD_ENSE %in% c(310, 410), COD_GRADO == 1)

# Siempre revisen cuántos casos quedan después de filtrar
nrow(simce); nrow(simce_municipal)


# 5. RECODIFICAR Y CREAR VARIABLES ---------------------------------------------------
# mutate() crea o modifica variables

simce <- simce %>%
  mutate(
    # Variable dicotómica (0/1) a partir de un código
    mujer = if_else(gen_alu == 2, 1, 0),                      # >>> ADAPTAR códigos

    # Variable categórica con etiquetas (factor)
    dependencia = factor(cod_depe2, levels = 1:3,
                         labels = c("Municipal", "Part. subvencionado", "Part. pagado")),

    # Agrupar una variable numérica en tramos
    ed_padres_cat = case_when(
      ed_padres < 12              ~ "Media incompleta o menos",
      ed_padres == 12             ~ "Media completa",
      ed_padres > 12              ~ "Educación superior",
      TRUE                        ~ NA_character_
    ),

    # Estandarizar un puntaje (media 0, desviación estándar 1)
    ptje_mate_z = (ptje_mate2m_alu - mean(ptje_mate2m_alu, na.rm = TRUE)) /
      sd(ptje_mate2m_alu, na.rm = TRUE)
  )

# Verificar SIEMPRE la recodificación cruzando la variable original con la nueva
simce %>% count(gen_alu, mujer)
simce %>% count(cod_depe2, dependencia)

# Ejemplo con matrícula: condición migrante según país de origen
# (Anexo XIII del libro de códigos: 0 = sin información, 6 = Chilena)
matricula <- matricula %>%
  mutate(migrante = if_else(PAIS_ORIGEN_ALU %in% c(0, 6), 0, 1))
matricula %>% count(PAIS_ORIGEN_ALU, migrante)


# 6. VALORES PERDIDOS ------------------------------------------------------------------
# En las bases reales, la no respuesta suele venir con CÓDIGOS (ej. 0, 8, 9, 99)
# y no como NA. Hay que transformarlos a NA según el libro de códigos.

# Ejemplo genérico (en la base simulada no hay códigos 99):
simce <- simce %>%
  mutate(ed_padres = if_else(ed_padres %in% c(99), NA_real_, ed_padres))   # >>> ADAPTAR códigos

# ¿Cuántos NA hay por variable?
colSums(is.na(simce))

# Proporción de casos perdidos en una variable
mean(is.na(simce$ed_padres))

# Eliminar casos con NA en las variables del análisis (mismo N en todos los modelos)
simce_completa <- simce %>% drop_na(ptje_mate2m_alu, ed_padres)   # >>> ADAPTAR variables
nrow(simce); nrow(simce_completa)


# 7. UNIR BASES (MERGE / JOIN) ------------------------------------------------------------
# left_join(x, y, by = "id"): conserva TODAS las filas de x y agrega columnas de y.
# Los identificadores típicos son: mrun (estudiante) y rbd (establecimiento).

# Ejemplo: base de establecimientos con información adicional
info_rbd <- simce %>%
  distinct(rbd, cod_depe2, cod_rural_rbd)       # una fila por establecimiento

# Antes de unir: el identificador NO debe estar duplicado en la base que se agrega
sum(duplicated(info_rbd$rbd))

# Unión estudiante + establecimiento
# (si una variable existe en ambas bases, R agrega sufijos .x e .y)
simce_unida <- simce %>%
  select(mrun, rbd, ptje_mate2m_alu) %>%
  left_join(info_rbd, by = "rbd")

# Después de unir: revisar que el número de filas NO cambió
nrow(simce); nrow(simce_unida)

# Tipos de join (referencia):
# inner_join(): solo casos presentes en ambas bases
# anti_join():  casos de x que NO están en y (útil para revisar pérdidas)
simce %>% anti_join(info_rbd, by = "rbd") %>% nrow()    # debería ser 0


# 8. AGREGAR INFORMACIÓN AL NIVEL DEL ESTABLECIMIENTO ----------------------------------------
# group_by() + summarise(): una fila por establecimiento (base de nivel 2)
base_rbd <- simce %>%
  group_by(rbd) %>%
  summarise(n_estudiantes   = n(),
            porc_migrante   = mean(migrante, na.rm = TRUE) * 100,
            prom_mate       = mean(ptje_mate2m_alu, na.rm = TRUE),
            prom_ed_padres  = mean(ed_padres, na.rm = TRUE))
head(base_rbd)

# group_by() + mutate(): agrega el valor del establecimiento a CADA estudiante
# (mantiene una fila por estudiante; es lo que se usa en modelos multinivel)
simce <- simce %>%
  group_by(rbd) %>%
  mutate(porc_migrante = mean(migrante, na.rm = TRUE) * 100) %>%
  ungroup()                                # importante: desagrupar al terminar

summary(simce$porc_migrante)


# 9. FORMATO LARGO Y ANCHO (BASES DE VARIOS AÑOS) --------------------------------------------
# Las bases de Mineduc son anuales. Al apilarlas quedan en formato LARGO:
# una fila por estudiante y año.
rend <- read_csv2("data/rendimiento_2024_2025_simulada.csv")   # >>> ADAPTAR ruta
rend %>% arrange(MRUN, AGNO) %>% head()

# ¿Cada estudiante aparece una vez por año?
rend %>% count(MRUN, AGNO) %>% filter(n > 1) %>% nrow()   # debe ser 0

# pivot_wider(): de LARGO a ANCHO (una fila por estudiante, columnas por año)
rend_ancha <- rend %>%
  select(MRUN, GEN_ALU, AGNO, RBD, ASISTENCIA, PROM_MATE) %>%     # >>> ADAPTAR variables
  pivot_wider(names_from = AGNO,
              values_from = c(RBD, ASISTENCIA, PROM_MATE))
head(rend_ancha)    # columnas: RBD_2024, RBD_2025, ASISTENCIA_2024, ...

# ¿Quiénes NO están en ambos años? (aparecen con NA en uno de ellos)
rend_ancha %>% filter(is.na(ASISTENCIA_2024) | is.na(ASISTENCIA_2025)) %>% nrow()

# pivot_longer(): de ANCHO a LARGO
rend_larga <- rend_ancha %>%
  pivot_longer(cols = c(ASISTENCIA_2024, ASISTENCIA_2025),
               names_to = "AGNO", names_prefix = "ASISTENCIA_",
               values_to = "ASISTENCIA")
head(rend_larga)


# 10. GUARDAR LA BASE ANALÍTICA --------------------------------------------------------------
# Guarden la base ya procesada para no repetir todo cada vez
saveRDS(simce, "output/simce_analitica.rds")
# Para volver a cargarla en otro script:
# simce <- readRDS("output/simce_analitica.rds")

# Exportar a Excel o CSV (por ejemplo, para revisar en otro programa)
# write_csv2(base_rbd, "output/base_establecimientos.csv")
