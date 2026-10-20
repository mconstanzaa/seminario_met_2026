##############################################
# 02. Construcción de trayectorias para análisis de supervivencia
# Seminario Metodológico 2026
# Última modificación: 19-10-2026
##############################################

# CONTENIDOS ------------------------------------------------------------------
# 1. PREPARACIÓN
# 2. LEER Y APILAR LAS BASES DE MATRÍCULA 2022-2025
# 3. UNIFICAR IDENTIFICADORES (IPE -> RUN)
# 4. DEFINIR LA COHORTE (1° MEDIO 2022)
# 5. PRESENCIA EN EL SISTEMA CADA AÑO
# 6. CONSTRUIR TIEMPO Y EVENTO
# 7. CREAR COVARIABLES
# 8. CHEQUEOS DE VERIFICACIÓN
# 9. GUARDAR LA BASE DE SUPERVIVENCIA
#
# DECISIONES METODOLÓGICAS (deben justificarlas en su informe)
# - Origen: estudiantes en 1° medio en 2022 (HC = 310, TP = 410).
# - Evento (deserción): el estudiante NO aparece matriculado en NINGÚN
#   establecimiento al año siguiente.
# - NO es deserción: cambiarse de establecimiento ni repetir de curso.
# - Salidas transitorias (sale y reingresa): se cuenta la PRIMERA salida.
# - Censura: quienes siguen matriculados en 2025.
# - Con 4 años de datos se observan 3 transiciones: 22->23, 23->24, 24->25.
#
# Las líneas marcadas con  # >>> ADAPTAR  son las que deben revisar
# con sus datos reales.
##############################################


# 1. PREPARACIÓN ---------------------------------------------------------------
options(scipen = 999)
rm(list = ls())

# Carpeta de trabajo: debe contener la carpeta data/ (output/ se crea sola).
# Si no usan un proyecto de RStudio, indíquenla con:
# Session > Set Working Directory > Choose Directory, o con setwd():
# setwd("C:/Users/su_usuario/Documents/seminario")                 # >>> ADAPTAR
getwd()   # muestra la carpeta de trabajo actual

library(tidyverse)

dir.create("output", showWarnings = FALSE)


# 2. LEER Y APILAR LAS BASES DE MATRÍCULA 2022-2025 ---------------------------------
# Seleccionar solo las variables necesarias ahorra memoria (las bases reales
# tienen millones de filas).
vars <- c("AGNO", "MRUN", "MRUN_IPE", "RBD", "COD_DEPE2", "RURAL_RBD",
          "COD_ENSE", "COD_GRADO", "GEN_ALU", "EDAD_ALU",
          "COD_NAC_ALU", "PAIS_ORIGEN_ALU")                            # >>> ADAPTAR

matricula <- map_dfr(2022:2025, function(anio) {
  read_csv2(paste0("data/matricula_", anio, "_simulada.csv"),        # >>> ADAPTAR ruta
            col_select = all_of(vars),
            # MRUN_IPE está vacío en la mayoría de las filas: se declara numérica
            # para que R no la lea como lógica y pierda los valores
            col_types = cols(MRUN_IPE = col_double(), COD_NAC_ALU = col_character()))
})

# Revisión inicial
matricula %>% count(AGNO)
summary(matricula$MRUN)
sum(is.na(matricula$MRUN))     # estudiantes sin MRUN


# 3. UNIFICAR IDENTIFICADORES (IPE -> RUN) ---------------------------------------------
# Estudiantes que ingresan sin RUN reciben un IPE. Mientras lo tienen, su IPE
# aparece en MRUN y MRUN_IPE está vacío. Cuando obtienen RUN, su MRUN cambia
# y el IPE anterior queda en MRUN_IPE (y se mantiene en los años siguientes).
# Ejemplo: 2024 -> MRUN = 45 (IPE) ; 2025 -> MRUN = 75 (RUN), MRUN_IPE = 45
# Sin corregir, el estudiante "deserta" en 2025 y el 75 no pertenece a la cohorte.
# En los datos reales, unos 20.000 estudiantes por año cambian de identificador.

## 3.1 Revisión de MRUN_IPE ----
# Esperado: sin MRUN vacíos y solo unos pocos casos con MRUN == MRUN_IPE
matricula %>%
  count(AGNO,
        mrun_vacio = is.na(MRUN),
        ipe_vacio  = is.na(MRUN_IPE),
        iguales    = !is.na(MRUN_IPE) & MRUN == MRUN_IPE)

## 3.2 Tabla de equivalencias: IPE anterior -> RUN definitivo ----
# Filas con IPE anterior registrado y distinto del MRUN actual
equiv <- matricula %>%
  filter(!is.na(MRUN_IPE), MRUN != MRUN_IPE) %>%
  distinct(MRUN_IPE, MRUN_nuevo = MRUN)

nrow(equiv)    # número de estudiantes que regularizaron su situación

# IPE asociados a más de un RUN (en los datos reales hay unas decenas).
# Se excluyen: si no, esos estudiantes quedarían duplicados en la unión.
equiv %>% count(MRUN_IPE) %>% filter(n > 1) %>% nrow()

equiv <- equiv %>%
  add_count(MRUN_IPE) %>%
  filter(n == 1) %>%
  select(-n)

## 3.3 Identificador único para todos los años ----
# Si el MRUN de una fila es un IPE con equivalencia, se reemplaza por el RUN;
# si no, se conserva el MRUN original.
matricula <- matricula %>%
  left_join(equiv, by = c("MRUN" = "MRUN_IPE")) %>%
  mutate(MRUN_unico = coalesce(MRUN_nuevo, MRUN)) %>%
  select(-MRUN_nuevo)

# ¿Cuántos registros cambiaron de identificador?
sum(matricula$MRUN != matricula$MRUN_unico)

# Después de unificar: un registro por estudiante y año (debe ser 0)
matricula %>% count(MRUN_unico, AGNO) %>% filter(n > 1) %>% nrow()


# 4. DEFINIR LA COHORTE (1° MEDIO 2022) ------------------------------------------------
cohorte <- matricula %>%
  filter(AGNO == 2022,
         COD_ENSE %in% c(310, 410),          # >>> ADAPTAR: HC y TP niños y jóvenes
         COD_GRADO == 1)

nrow(cohorte)
sum(duplicated(cohorte$MRUN_unico))   # debe ser 0


# 5. PRESENCIA EN EL SISTEMA CADA AÑO ----------------------------------------------------
# Para cada estudiante de la cohorte: ¿está matriculado en algún establecimiento
# en cada año? (1 = sí, 0 = no). El RBD no importa: cambiarse no es desertar.
presencia <- matricula %>%
  filter(MRUN_unico %in% cohorte$MRUN_unico) %>%
  distinct(MRUN_unico, AGNO) %>%
  mutate(matriculado = 1) %>%
  pivot_wider(names_from = AGNO, values_from = matriculado,
              names_prefix = "mat_", values_fill = 0)

head(presencia)

# Patrones de trayectoria (1 = matriculado)
presencia %>% count(mat_2022, mat_2023, mat_2024, mat_2025, sort = TRUE)


# 6. CONSTRUIR TIEMPO Y EVENTO ------------------------------------------------------------
surv_data <- cohorte %>%
  left_join(presencia, by = "MRUN_unico") %>%
  mutate(
    # Tiempo: año de la PRIMERA salida, o 3 si nunca sale (censura)
    tiempo = case_when(mat_2023 == 0 ~ 1,
                       mat_2024 == 0 ~ 2,
                       mat_2025 == 0 ~ 3,
                       TRUE          ~ 3),
    # Evento: 1 = desertó en algún momento ; 0 = censurado (sigue en 2025)
    desercion = if_else(mat_2023 == 0 | mat_2024 == 0 | mat_2025 == 0, 1, 0),
    # Salida transitoria: sale y luego reingresa
    reingreso = if_else(
      (mat_2023 == 0 & (mat_2024 == 1 | mat_2025 == 1)) |
        (mat_2024 == 0 & mat_2025 == 1), 1, 0)
  )

surv_data %>% count(tiempo, desercion)


# 7. CREAR COVARIABLES ---------------------------------------------------------------------
# Todas medidas en 2022 (al inicio de la trayectoria)
surv_data <- surv_data %>%
  mutate(
    # País de origen (Anexo XIII del libro de códigos de Matrícula)
    migrante = if_else(PAIS_ORIGEN_ALU %in% c(0, 6), 0, 1),
    pais = case_when(PAIS_ORIGEN_ALU %in% c(0, 6) ~ "Chile",
                     PAIS_ORIGEN_ALU == 27        ~ "Venezuela",
                     PAIS_ORIGEN_ALU == 22        ~ "Perú",
                     TRUE                         ~ "Otro país"),        # >>> ADAPTAR
    pais = factor(pais, levels = c("Chile", "Venezuela", "Perú", "Otro país")),
    # Nacionalidad según Registro Civil: C = chileno, E = extranjero, N = nacionalizado
    nacionalidad = factor(COD_NAC_ALU, levels = c("C", "E", "N"),
                          labels = c("Chilena", "Extranjera", "Nacionalizada")),
    hombre = if_else(GEN_ALU == 1, 1, 0),
    # Extraedad: 16 años o más al 30 de junio en 1° medio
    extraedad = if_else(EDAD_ALU >= 16, 1, 0),                              # >>> ADAPTAR
    dependencia = factor(COD_DEPE2, levels = 1:3,
                         labels = c("Municipal", "Part. subvencionado", "Part. pagado")),
    municipal = if_else(COD_DEPE2 == 1, 1, 0),
    tecnico_prof = if_else(COD_ENSE == 410, 1, 0)
  )

# Verificar las recodificaciones
surv_data %>% count(PAIS_ORIGEN_ALU, pais)
surv_data %>% count(COD_NAC_ALU, nacionalidad)


# 8. CHEQUEOS DE VERIFICACIÓN ---------------------------------------------------------------
# 8.1 Un identificador por estudiante (debe ser 0)
sum(duplicated(surv_data$MRUN_unico))

# 8.2 Todos los estudiantes están en 2022 (debe ser 0)
sum(surv_data$mat_2022 == 0)

# 8.3 Tiempo entre 1 y 3, y evento 0/1
table(surv_data$tiempo, surv_data$desercion)

# 8.4 Salidas transitorias
sum(surv_data$reingreso)

# 8.5 Proporción de deserción por grupo (¿resultados razonables?)
surv_data %>%
  group_by(pais) %>%
  summarise(n = n(), desercion = round(mean(desercion) * 100, 1))

# 8.6 ¿Cuánto cambia la deserción si NO se corrige el IPE?
# (repite los pasos 4 a 6 usando MRUN en lugar de MRUN_unico)
coh_sin <- matricula %>% filter(AGNO == 2022, COD_ENSE %in% c(310, 410), COD_GRADO == 1)
pres_sin <- matricula %>%
  filter(MRUN %in% coh_sin$MRUN) %>% distinct(MRUN, AGNO) %>% mutate(m = 1) %>%
  pivot_wider(names_from = AGNO, values_from = m, names_prefix = "mat_", values_fill = 0)
sin_correccion <- coh_sin %>% left_join(pres_sin, by = "MRUN") %>%
  mutate(desercion = if_else(mat_2023 == 0 | mat_2024 == 0 | mat_2025 == 0, 1, 0),
         migrante = if_else(PAIS_ORIGEN_ALU %in% c(0, 6), 0, 1))

bind_rows(sin_correccion %>% mutate(base = "Sin corrección"),
          surv_data %>% mutate(base = "Con corrección")) %>%
  group_by(base, migrante) %>%
  summarise(desercion = round(mean(desercion) * 100, 1), .groups = "drop")

# LIMITACIÓN: la corrección de IPE no resuelve la salida del país.
# Un estudiante que emigra sigue apareciendo como desertor.


# 9. GUARDAR LA BASE DE SUPERVIVENCIA --------------------------------------------------------
saveRDS(surv_data, "output/surv_data.rds")
# Se carga en 03_descriptivos.R y 05_supervivencia.R con:
# surv_data <- readRDS("output/surv_data.rds")
