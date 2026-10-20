##############################################
# 04. Modelos multinivel
# Seminario Metodológico 2026
# Última modificación: 19-10-2026
##############################################

# CONTENIDOS ------------------------------------------------------------------
# 1. PREPARACIÓN
# PARTE A. ESTUDIANTES ANIDADOS EN ESTABLECIMIENTOS (UN AÑO)
#   2. PREPARAR VARIABLES
#   3. PASO 1: MODELO NULO Y CORRELACIÓN INTRACLASE (CCI)
#   4. PASO 2: PREDICTORES DE NIVEL 1
#   5. PASO 3: PREDICTORES DE NIVEL 2
#   6. PASO 4 (OPCIONAL): INTERACCIONES
#   7. COMPARACIÓN DE MODELOS
#   8. EXPORTAR RESULTADOS
# PARTE B. TRANSICIONES: LOS MISMOS ESTUDIANTES EN DOS AÑOS
#   9. CONSTRUIR LA BASE ANCHA
#   10. MODELO DE CAMBIO
#   11. EXTENSIÓN OPCIONAL: CLASIFICACIÓN CRUZADA (ORIGEN Y DESTINO)
#   12. ANÁLISIS DE ROBUSTEZ (OPCIONAL): MODELO CON REZAGO
#   13. EXPORTAR RESULTADOS
#
# ¿Qué parte me corresponde?
# - Un año de datos (ej. SIMCE 2° medio 2025)          -> PARTE A
# - Mismos estudiantes en dos años (8° básico -> media) -> PARTE B
#
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

# install.packages(c("lme4", "texreg"))   # solo la primera vez
library(tidyverse)   # manipulación de datos y gráficos
library(lme4)        # modelos multinivel: lmer()
library(texreg)      # tablas de modelos: screenreg(), htmlreg()

dir.create("output", showWarnings = FALSE)

# Función para calcular la CCI a partir de un modelo nulo
calcular_cci <- function(modelo) {
  v <- as.data.frame(VarCorr(modelo))$vcov
  v[1] / sum(v)
}


# =====================================================================
# PARTE A. ESTUDIANTES ANIDADOS EN ESTABLECIMIENTOS (UN AÑO)
# =====================================================================

# 2. PREPARAR VARIABLES -------------------------------------------------------------
load("data/simce_simulada.RData")                                   # >>> ADAPTAR

simce <- simce %>%
  # 2.1 Mismo N en todos los modelos: eliminar casos con NA en TODAS las
  #     variables que se usarán (si no, no se pueden comparar los modelos)
  drop_na(ptje_mate2m_alu, gen_alu, migrante, ed_padres, practicas) %>%   # >>> ADAPTAR
  mutate(
    # 2.2 Recodificaciones
    mujer       = if_else(gen_alu == 2, 1, 0),
    dependencia = factor(cod_depe2, levels = 1:3,
                         labels = c("Municipal", "Part. subvencionado", "Part. pagado")),
    rural       = cod_rural_rbd,
    # 2.3 Centrado a la gran media: a cada valor se le resta el PROMEDIO DE TODA
    #     LA MUESTRA. Ej.: si el promedio es 12 años, un estudiante con padres de
    #     9 años queda en -3 y uno con 16 años queda en +4.
    #     - El 0 pasa a ser el promedio (y no "0 años de escolaridad").
    #     - El intercepto se interpreta para un estudiante con valores promedio.
    #     - En las interacciones, cada término principal se interpreta en el
    #       promedio de la otra variable.
    #     - No cambia las pendientes ni el ajuste del modelo.
    ed_padres_c = ed_padres - mean(ed_padres),
    practicas_c = practicas - mean(practicas)
  ) %>%
  # 2.4 Variables de nivel 2 construidas desde los estudiantes (composición)
  group_by(rbd) %>%
  mutate(
    n_rbd           = n(),
    porc_migrante   = mean(migrante) * 100,     # % de migrantes del establecimiento
    ed_padres_media = mean(ed_padres_c)         # escolaridad media de los padres (centrada)
  ) %>%
  ungroup()

# 2.5 Revisión de la estructura anidada
simce %>% summarise(estudiantes = n(), establecimientos = n_distinct(rbd))
simce %>% distinct(rbd, n_rbd) %>% summarise(min = min(n_rbd), media = mean(n_rbd), max = max(n_rbd))
# Establecimientos con muy pocos estudiantes aportan poca información sobre la
# varianza entre establecimientos. Pueden informar cuántos hay o excluir los
# que tengan menos de un mínimo (justificándolo).


# 3. PASO 1: MODELO NULO Y CORRELACIÓN INTRACLASE (CCI) -------------------------------
# Sin predictores: separa la varianza ENTRE establecimientos y DENTRO de ellos.
# (1 | rbd) = un intercepto aleatorio para cada establecimiento.
m0 <- lmer(ptje_mate2m_alu ~ 1 + (1 | rbd), data = simce)          # >>> ADAPTAR VD y nivel 2
summary(m0)

# Lectura de la salida:
# - Fixed effects (Intercept): promedio general
# - Random effects:  rbd (Intercept) = varianza ENTRE establecimientos
#                    Residual        = varianza DENTRO de establecimientos

cci <- calcular_cci(m0)
round(cci, 3)
# Interpretación: el (CCI x 100) % de la varianza del puntaje se encuentra
# ENTRE establecimientos. Una CCI mayor a 0 justifica el modelo multinivel.


# 4. PASO 2: PREDICTORES DE NIVEL 1 ----------------------------------------------------
m1 <- lmer(ptje_mate2m_alu ~ mujer + migrante + ed_padres_c + practicas_c +   # >>> ADAPTAR
             (1 | rbd), data = simce)
summary(m1)

# Solo la tabla de coeficientes
summary(m1)$coefficients

# Interpretación (ejemplo):
# - mujer: diferencia promedio entre mujeres y hombres DEL MISMO establecimiento,
#   controlando por las demás variables.
# - ed_padres_c: diferencia promedio en el puntaje asociada a cada año adicional
#   de escolaridad de los padres.
# - Intercepto: puntaje esperado de un hombre nativo con valores promedio
#   en las variables centradas.
# NOTA: lmer() no entrega valores p. Con muestras grandes, |t| > 1,96
# equivale a significancia al 5 %. También pueden usar intervalos de confianza:
confint(m1, method = "Wald")


# 5. PASO 3: PREDICTORES DE NIVEL 2 -----------------------------------------------------
m2 <- lmer(ptje_mate2m_alu ~ mujer + migrante + ed_padres_c + practicas_c +
             dependencia + rural + porc_migrante + ed_padres_media +           # >>> ADAPTAR
             (1 | rbd), data = simce)
summary(m2)$coefficients

# Interpretación:
# - dependencia: diferencia respecto de establecimientos municipales (referencia).
# - porc_migrante: diferencia en el puntaje asociada a cada punto porcentual adicional de
#   estudiantes migrantes en el establecimiento.
# - ed_padres_media: asociación entre la composición social del establecimiento
#   y el puntaje, MÁS ALLÁ de la escolaridad de los propios padres. En la
#   literatura se denomina "efecto contextual", pero con estos datos es una
#   asociación: la selección de estudiantes en establecimientos no es aleatoria.


# 6. PASO 4 (OPCIONAL): INTERACCIONES ----------------------------------------------------
# Una interacción permite que la relación entre X e Y varíe según otra variable.
# X1 * X2 incluye automáticamente X1, X2 y X1:X2.

## 6.1 Interacción dentro del mismo nivel (ej. sexo x escolaridad de los padres) ----
m3a <- lmer(ptje_mate2m_alu ~ migrante + practicas_c + mujer * ed_padres_c +
              dependencia + rural + porc_migrante + ed_padres_media +
              (1 | rbd), data = simce)
summary(m3a)$coefficients

## 6.2 Interacción entre niveles (cross-level) ----
# ¿La relación entre prácticas docentes percibidas (nivel 1) y el puntaje
# varía según la composición social del establecimiento (nivel 2)?
m3 <- lmer(ptje_mate2m_alu ~ mujer + migrante + ed_padres_c +
             dependencia + rural + porc_migrante +
             practicas_c * ed_padres_media +                                 # >>> ADAPTAR
             (1 | rbd), data = simce)
summary(m3)$coefficients

# Interpretación:
# - practicas_c: pendiente de prácticas cuando ed_padres_media = 0
#   (establecimiento de composición promedio).
# - practicas_c:ed_padres_media: cuánto cambia esa pendiente por cada año
#   adicional de escolaridad media de los padres del establecimiento.

# Pendiente de prácticas en distintos contextos
b <- fixef(m3)
tibble(composicion = c(-2, 0, 2)) %>%
  mutate(pendiente_practicas = b["practicas_c"] +
           b["practicas_c:ed_padres_media"] * composicion)

# Gráfico de la interacción (rectas predichas)
expand_grid(practicas_c = seq(-1, 1, .1), composicion = c(-2, 0, 2)) %>%
  mutate(grupo = factor(composicion, labels = c("Baja (-2 años)", "Media", "Alta (+2 años)")),
         predicho = b["(Intercept)"] + b["practicas_c"] * practicas_c +
           b["ed_padres_media"] * composicion +
           b["practicas_c:ed_padres_media"] * practicas_c * composicion) %>%
  ggplot(aes(practicas_c, predicho, color = grupo)) +
  geom_line(linewidth = 1.2) +
  labs(x = "Prácticas docentes percibidas (centradas)", y = "Puntaje predicho",
       color = "Composición social del establecimiento") +
  theme_minimal()
ggsave("output/interaccion_niveles.png", width = 8, height = 4.5)

## 6.3 ¿Mejora el ajuste al agregar la interacción? ----
anova(m2, m3)


# 7. COMPARACIÓN DE MODELOS --------------------------------------------------------------
## 7.1 Tabla comparativa ----
screenreg(list(m0, m1, m2, m3),
          custom.model.names = c("Nulo", "Nivel 1", "Nivel 1 + 2", "Interacción"),
          digits = 2)

## 7.2 Reducción de la varianza en cada nivel (respecto del modelo nulo) ----
var_m <- function(m) as.data.frame(VarCorr(m))$vcov   # [1] entre, [2] dentro
tibble(modelo     = c("Nulo", "Nivel 1", "Nivel 1 + 2", "Interacción"),
       var_entre  = map_dbl(list(m0, m1, m2, m3), ~ var_m(.x)[1]),
       var_dentro = map_dbl(list(m0, m1, m2, m3), ~ var_m(.x)[2])) %>%
  mutate(red_entre  = round((1 - var_entre / var_entre[1]) * 100, 1),
         red_dentro = round((1 - var_dentro / var_dentro[1]) * 100, 1))
# Las variables de nivel 2 solo reducen la varianza ENTRE establecimientos.

## 7.3 Prueba de razón de verosimilitud ----
# anova() re-estima con máxima verosimilitud (ML). Menor AIC/BIC y p < 0,05
# indican que el modelo más complejo ajusta mejor. Requiere los mismos casos.
anova(m0, m1, m2, m3)


# 8. EXPORTAR RESULTADOS -------------------------------------------------------------------
htmlreg(list(m0, m1, m2, m3), file = "output/tabla_multinivel.doc",
        custom.model.names = c("Nulo", "Nivel 1", "Nivel 1 + 2", "Interacción"),
        digits = 2, caption = "Modelos multinivel del puntaje SIMCE de matemática",
        caption.above = TRUE)
# El archivo .doc se abre en Word.


# =====================================================================
# PARTE B. TRANSICIONES: LOS MISMOS ESTUDIANTES EN DOS AÑOS
# (ej. 8° básico 2024 -> 1° medio 2025; todos cambian de establecimiento)
# =====================================================================

# 9. CONSTRUIR LA BASE ANCHA ------------------------------------------------------------------
# Las bases de Mineduc vienen en formato LARGO (una fila por estudiante y año).
rend <- read_csv2("data/rendimiento_2024_2025_simulada.csv")          # >>> ADAPTAR

# 9.1 Un registro por estudiante y año (debe ser 0)
rend %>% count(MRUN, AGNO) %>% filter(n > 1) %>% nrow()

# 9.2 Pasar a formato ANCHO: una fila por estudiante, variables con sufijo del año
trans_ancha <- rend %>%
  select(MRUN, GEN_ALU, AGNO, RBD, RURAL_RBD, COD_DEPE2, IVE,
         ASISTENCIA, PROM_MATE) %>%                                    # >>> ADAPTAR
  pivot_wider(names_from = AGNO,
              values_from = c(RBD, RURAL_RBD, COD_DEPE2, IVE, ASISTENCIA, PROM_MATE))
names(trans_ancha)

# 9.3 Conservar estudiantes con información en ambos años
nrow(trans_ancha)
trans_ancha <- trans_ancha %>% drop_na(ASISTENCIA_2024, ASISTENCIA_2025)   # >>> ADAPTAR
nrow(trans_ancha)
# IMPORTANTE: quienes no aparecen en el segundo año (desertan, emigran o
# cambian de identificador IPE -> RUN) quedan fuera. Infórmenlo como limitación.

# 9.4 Variables para el modelo
trans_ancha <- trans_ancha %>%
  mutate(
    # Variable dependiente: CAMBIO entre ambos años
    # (negativo = disminuye ; positivo = aumenta)
    cambio_asistencia = ASISTENCIA_2025 - ASISTENCIA_2024,             # >>> ADAPTAR
    cambio_nota       = PROM_MATE_2025 - PROM_MATE_2024,                # >>> ADAPTAR
    # Características del estudiante
    mujer            = if_else(GEN_ALU == 2, 1, 0),
    # Características de los establecimientos de origen y destino
    origen_rural     = RURAL_RBD_2024,
    origen_municipal = if_else(COD_DEPE2_2024 == 1, 1, 0),
    destino_particular = if_else(COD_DEPE2_2025 %in% c(2, 3), 1, 0),
    # ¿Pasa de un establecimiento público a uno particular?
    publico_a_particular = if_else(origen_municipal == 1 & destino_particular == 1, 1, 0),
    # Diferencia de vulnerabilidad: > 0 el destino es más vulnerable que el origen
    dif_ive          = IVE_2025 - IVE_2024
  ) %>%
  # Centramos solo la variable continua: el 0 es la diferencia de IVE promedio.
  # Las variables 0/1 quedan sin centrar: el intercepto es el cambio de la
  # categoría de referencia (hombres de origen urbano, etc.).
  mutate(dif_ive_c = dif_ive - mean(dif_ive))

# 9.5 ¿Cuántos estudiantes de la cohorte llegan a cada establecimiento de destino?
trans_ancha %>% count(RBD_2025) %>%
  summarise(establecimientos = n(), min = min(n), mediana = median(n), max = max(n))


# 10. MODELO DE CAMBIO --------------------------------------------------------------------------
# Pregunta: ¿varía la asistencia entre 2024 y 2025? ¿Para quiénes varía más?
# VD: cambio = asistencia 2025 - asistencia 2024 (una fila por estudiante).
# Nivel 2: establecimiento de DESTINO (donde se mide el resultado de 2025).
# Las características del establecimiento de ORIGEN entran como predictores.

## 10.1 Cambio promedio (descriptivo, sin considerar el anidamiento) ----
mean(trans_ancha$cambio_asistencia)
t.test(trans_ancha$ASISTENCIA_2025, trans_ancha$ASISTENCIA_2024, paired = TRUE)

## 10.2 Modelo nulo de cambio ----
# El intercepto es el cambio promedio; la CCI indica qué parte de la variación
# en el cambio se encuentra entre establecimientos de destino.
m_c0 <- lmer(cambio_asistencia ~ 1 + (1 | RBD_2025), data = trans_ancha)
summary(m_c0)$coefficients
calcular_cci(m_c0)

## 10.3 ¿Para quiénes cambia más? Características del estudiante ----
m_c1 <- lmer(cambio_asistencia ~ mujer + origen_rural +
               (1 | RBD_2025), data = trans_ancha)                       # >>> ADAPTAR
summary(m_c1)$coefficients

## 10.4 Características de los establecimientos de origen y destino ----
m_c2 <- lmer(cambio_asistencia ~ mujer + origen_rural +
               dif_ive_c + publico_a_particular +
               (1 | RBD_2025), data = trans_ancha)                       # >>> ADAPTAR
summary(m_c2)$coefficients

# Interpretación:
# - Intercepto: cambio esperado para un hombre de origen urbano con diferencia
#   de IVE promedio. El cambio promedio de toda la cohorte está en el modelo
#   nulo (sección 10.2).
# - mujer: diferencia en el cambio entre mujeres y hombres que llegan al
#   mismo establecimiento de destino.
# - dif_ive_c: diferencia en el cambio asociada a cada punto adicional de
#   vulnerabilidad del destino respecto del origen.
# - publico_a_particular: diferencia en el cambio de quienes pasan de un
#   establecimiento municipal a uno particular.
# Son asociaciones: no permiten afirmar que el cambio de establecimiento
# produzca el cambio en la asistencia.

## 10.5 (Opcional) ¿La diferencia entre grupos varía según otra variable? ----
# Ej. ¿la diferencia por origen rural varía según el sexo?
m_c3 <- lmer(cambio_asistencia ~ mujer * origen_rural + dif_ive_c +
               (1 | RBD_2025), data = trans_ancha)
summary(m_c3)$coefficients

## 10.6 Mismo análisis con notas ----
# Un modelo por asignatura. ADVERTENCIA: los criterios de calificación difieren
# entre establecimientos y entre básica y media; el cambio promedio (intercepto)
# puede reflejar en parte criterios más exigentes en media. Interpreten sobre
# todo las DIFERENCIAS ENTRE GRUPOS.
m_nota <- lmer(cambio_nota ~ mujer + origen_rural + dif_ive_c +
                 (1 | RBD_2025), data = trans_ancha)                     # >>> ADAPTAR
summary(m_nota)$coefficients


# 11. EXTENSIÓN OPCIONAL: CLASIFICACIÓN CRUZADA (ORIGEN Y DESTINO) ---------------------------
# Cada estudiante pertenece a DOS establecimientos que no están anidados entre sí.
# lme4 lo estima agregando un segundo intercepto aleatorio.
m_c_cruz <- lmer(cambio_asistencia ~ mujer + origen_rural + dif_ive_c +
                   publico_a_particular +
                   (1 | RBD_2024) + (1 | RBD_2025), data = trans_ancha)
summary(m_c_cruz)
VarCorr(m_c_cruz)
# Una varianza para el establecimiento de origen y otra para el de destino.
# Si aparece "boundary (singular) fit", la varianza de un nivel es ~0:
# pueden informar el modelo solo con el destino.


# 12. ANÁLISIS DE ROBUSTEZ (OPCIONAL): MODELO CON REZAGO -------------------------------------
# Otra forma de estudiar el cambio: la VD es la asistencia de 2025 y la
# asistencia de 2024 entra como control. Compara estudiantes con el MISMO
# punto de partida. Si los grupos parten de niveles distintos, ambos modelos
# pueden diferir (paradoja de Lord). Si llegan a la misma conclusión, el
# resultado es más sólido.
trans_ancha <- trans_ancha %>%
  mutate(asis_2024_c = ASISTENCIA_2024 - mean(ASISTENCIA_2024))

m_rezago <- lmer(ASISTENCIA_2025 ~ asis_2024_c + mujer + origen_rural +
                   dif_ive_c + publico_a_particular +
                   (1 | RBD_2025), data = trans_ancha)
summary(m_rezago)$coefficients

# Comparar los coeficientes de interés en ambos modelos
round(cbind(cambio = fixef(m_c2)[-1],
            rezago = fixef(m_rezago)[names(fixef(m_c2))[-1]]), 2)


# 13. EXPORTAR RESULTADOS ----------------------------------------------------------------------
screenreg(list(m_c0, m_c1, m_c2, m_c_cruz),
          custom.model.names = c("Nulo", "Estudiante", "Estudiante + contexto", "Cruzado"),
          digits = 2)
htmlreg(list(m_c0, m_c1, m_c2, m_c_cruz), file = "output/tabla_transicion.doc",
        custom.model.names = c("Nulo", "Estudiante", "Estudiante + contexto", "Cruzado"),
        digits = 2,
        caption = "Modelos multinivel del cambio en la asistencia entre 8° básico y 1° medio",
        caption.above = TRUE)

# ADVERTENCIAS PARA EL INFORME:
# - Selección: los estudiantes no se asignan al azar a los establecimientos.
# - Notas: los criterios de calificación difieren entre establecimientos.
# - Quienes no completan la transición quedan fuera del análisis.
