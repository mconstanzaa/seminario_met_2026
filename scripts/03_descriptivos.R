##############################################
# 03. Análisis descriptivos
# Seminario Metodológico 2026
# Última modificación: 19-10-2026
##############################################

# CONTENIDOS ------------------------------------------------------------------
# 1. PREPARACIÓN
# 2. DESCRIPTIVOS PARA MODELOS MULTINIVEL (SIMCE)
#    2.1 Tabla descriptiva de la muestra
#    2.2 Descriptivos por grupo
#    2.3 Variabilidad entre establecimientos
#    2.4 Gráficos
# 3. DESCRIPTIVOS PARA TRANSICIONES (DOS AÑOS)
# 4. DESCRIPTIVOS PARA SUPERVIVENCIA (COHORTE)
# 5. EXPORTAR TABLAS
#
# Ejecuten solo las secciones que corresponden a su análisis.
# Las líneas marcadas con  # >>> ADAPTAR  deben cambiarse según su base.
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

# Función auxiliar: media, DE, mínimo, máximo y % de perdidos de varias variables
describir <- function(datos, variables) {
  datos %>%
    select(all_of(variables)) %>%
    pivot_longer(everything(), names_to = "variable", values_to = "valor") %>%
    group_by(variable) %>%
    summarise(n         = sum(!is.na(valor)),
              media     = round(mean(valor, na.rm = TRUE), 2),
              de        = round(sd(valor, na.rm = TRUE), 2),
              min       = round(min(valor, na.rm = TRUE), 2),
              max       = round(max(valor, na.rm = TRUE), 2),
              perdidos  = round(mean(is.na(valor)) * 100, 1),
              .groups = "drop")
}


# 2. DESCRIPTIVOS PARA MODELOS MULTINIVEL (SIMCE) ------------------------------------
load("data/simce_simulada.RData")                                   # >>> ADAPTAR

simce <- simce %>%
  mutate(mujer = if_else(gen_alu == 2, 1, 0),
         dependencia = factor(cod_depe2, levels = 1:3,
                              labels = c("Municipal", "Part. subvencionado", "Part. pagado")))

## 2.1 Tabla descriptiva de la muestra ----
# Variables numéricas o dicotómicas (para 0/1, la media es una proporción)
tabla_desc <- describir(simce, c("ptje_mate2m_alu", "mujer", "migrante",
                                 "ed_padres", "practicas"))          # >>> ADAPTAR
tabla_desc

# Variables categóricas: frecuencias y porcentajes
simce %>% count(dependencia) %>% mutate(porc = round(n / sum(n) * 100, 1))

# Tamaño de la muestra en cada nivel
simce %>% summarise(estudiantes = n(), establecimientos = n_distinct(rbd))

## 2.2 Descriptivos por grupo ----
# Puntaje promedio según condición migrante y sexo
simce %>%
  group_by(migrante, mujer) %>%
  summarise(n = n(),
            media = round(mean(ptje_mate2m_alu, na.rm = TRUE), 1),
            de = round(sd(ptje_mate2m_alu, na.rm = TRUE), 1),
            .groups = "drop")

# Diferencia de medias con prueba t (descriptivo bivariado)
t.test(ptje_mate2m_alu ~ migrante, data = simce)

# Correlación entre variables numéricas
cor(simce$ptje_mate2m_alu, simce$ed_padres, use = "complete.obs")

## 2.3 Variabilidad entre establecimientos ----
# Una fila por establecimiento
por_rbd <- simce %>%
  group_by(rbd, dependencia) %>%
  summarise(n_estudiantes = n(),
            prom_mate     = mean(ptje_mate2m_alu, na.rm = TRUE),
            porc_migrante = mean(migrante, na.rm = TRUE) * 100,
            .groups = "drop")

# Distribución del número de estudiantes por establecimiento
summary(por_rbd$n_estudiantes)
sum(por_rbd$n_estudiantes < 5)       # establecimientos con muy pocos estudiantes

# Descriptivos de las variables de nivel 2
describir(por_rbd, c("n_estudiantes", "prom_mate", "porc_migrante"))

## 2.4 Gráficos ----
# Distribución de la variable dependiente
ggplot(simce, aes(ptje_mate2m_alu)) +
  geom_histogram(bins = 40, fill = "#9467bd", color = "white") +
  labs(x = "Puntaje SIMCE matemática", y = "Frecuencia") +
  theme_minimal()
ggsave("output/histograma_puntaje.png", width = 7, height = 4)

# Puntaje según dependencia
ggplot(simce, aes(dependencia, ptje_mate2m_alu)) +
  geom_boxplot(fill = "#9467bd", alpha = .4) +
  labs(x = NULL, y = "Puntaje SIMCE matemática") +
  theme_minimal()

# Promedio por establecimiento (¿varían los establecimientos?)
ggplot(por_rbd, aes(reorder(factor(rbd), prom_mate), prom_mate)) +
  geom_point(color = "#9467bd") +
  geom_hline(yintercept = mean(simce$ptje_mate2m_alu), linetype = "dashed") +
  labs(x = "Establecimientos (ordenados)", y = "Puntaje promedio") +
  theme_minimal() + theme(axis.text.x = element_blank())
ggsave("output/promedios_establecimientos.png", width = 8, height = 4)

# Relación entre composición del establecimiento y puntaje promedio
ggplot(por_rbd, aes(porc_migrante, prom_mate)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm", color = "#9467bd") +
  labs(x = "% estudiantes migrantes del establecimiento", y = "Puntaje promedio") +
  theme_minimal()


# 3. DESCRIPTIVOS PARA TRANSICIONES (DOS AÑOS) -----------------------------------------
rend <- read_csv2("data/rendimiento_2024_2025_simulada.csv")           # >>> ADAPTAR

trans_ancha <- rend %>%
  select(MRUN, GEN_ALU, AGNO, RBD, RURAL_RBD, IVE, ASISTENCIA, PROM_MATE) %>%
  pivot_wider(names_from = AGNO, values_from = c(RBD, RURAL_RBD, IVE, ASISTENCIA, PROM_MATE)) %>%
  mutate(mujer = if_else(GEN_ALU == 2, 1, 0),
         cambio_asistencia = ASISTENCIA_2025 - ASISTENCIA_2024,
         cambio_nota       = PROM_MATE_2025 - PROM_MATE_2024)

# Descriptivos de ambos años y del cambio
describir(trans_ancha, c("ASISTENCIA_2024", "ASISTENCIA_2025", "cambio_asistencia",
                         "PROM_MATE_2024", "PROM_MATE_2025", "cambio_nota"))

# Cambio promedio según tipo de estudiante (ej. origen rural y sexo)          # >>> ADAPTAR
trans_ancha %>%
  group_by(origen_rural = RURAL_RBD_2024, mujer) %>%
  summarise(n = n(),
            asis_2024 = round(mean(ASISTENCIA_2024), 1),
            asis_2025 = round(mean(ASISTENCIA_2025), 1),
            cambio    = round(mean(cambio_asistencia), 1),
            .groups = "drop")

# ¿El cambio es distinto de 0? (prueba t pareada)
t.test(trans_ancha$ASISTENCIA_2025, trans_ancha$ASISTENCIA_2024, paired = TRUE)

# Estudiantes por establecimiento de ORIGEN y de DESTINO
trans_ancha %>% count(RBD_2024) %>% summarise(min = min(n), mediana = median(n), max = max(n))
trans_ancha %>% count(RBD_2025) %>% summarise(min = min(n), mediana = median(n), max = max(n))

# Gráfico: asistencia en ambos años según origen
trans_ancha %>%
  pivot_longer(c(ASISTENCIA_2024, ASISTENCIA_2025), names_to = "anio",
               names_prefix = "ASISTENCIA_", values_to = "asistencia") %>%
  group_by(anio, origen = if_else(RURAL_RBD_2024 == 1, "Rural", "Urbano")) %>%
  summarise(asistencia = mean(asistencia), .groups = "drop") %>%
  ggplot(aes(anio, asistencia, color = origen, group = origen)) +
  geom_line(linewidth = 1.2) + geom_point(size = 3) +
  labs(x = "Año", y = "Asistencia promedio (%)", color = "Establecimiento de origen") +
  theme_minimal()


# 4. DESCRIPTIVOS PARA SUPERVIVENCIA (COHORTE) -------------------------------------------
# Requiere haber ejecutado 02_construir_trayectorias.R
surv_data <- readRDS("output/surv_data.rds")

# Descripción de la cohorte: N, eventos y censurados
surv_data %>%
  summarise(estudiantes = n(),
            desertores  = sum(desercion),
            censurados  = sum(desercion == 0),
            porc_desercion = round(mean(desercion) * 100, 1))

# Deserción según el año en que ocurre
surv_data %>% filter(desercion == 1) %>% count(tiempo) %>%
  mutate(porc = round(n / sum(n) * 100, 1))

# Composición de la cohorte
surv_data %>% count(pais) %>% mutate(porc = round(n / sum(n) * 100, 1))
surv_data %>% count(dependencia, migrante) %>%
  group_by(dependencia) %>% mutate(porc = round(n / sum(n) * 100, 1))

# Deserción según características (bivariado)
surv_data %>%
  group_by(pais) %>%
  summarise(n = n(), desercion = round(mean(desercion) * 100, 1))

surv_data %>%
  group_by(dependencia, migrante) %>%
  summarise(n = n(), desercion = round(mean(desercion) * 100, 1), .groups = "drop")

# Prueba chi-cuadrado (descriptivo, sin considerar el tiempo)
chisq.test(table(surv_data$migrante, surv_data$desercion))


# 5. EXPORTAR TABLAS ------------------------------------------------------------------------
write_csv2(tabla_desc, "output/tabla_descriptivos_simce.csv")
# Para Word, pueden abrir el .csv en Excel y copiar la tabla,
# o usar paquetes como flextable o gtsummary.
