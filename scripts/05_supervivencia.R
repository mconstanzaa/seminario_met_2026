##############################################
# 05. Análisis de supervivencia
# Seminario Metodológico 2026
# Última modificación: 19-10-2026
##############################################

# CONTENIDOS ------------------------------------------------------------------
# 1. PREPARACIÓN
# 2. CARGAR LA BASE DE SUPERVIVENCIA
# 3. KAPLAN-MEIER: SUPERVIVENCIA DE LA COHORTE
# 4. KAPLAN-MEIER POR GRUPOS
# 5. PRUEBA LOG-RANK
# 6. MODELO DE COX
# 7. SUPUESTO DE RIESGOS PROPORCIONALES
# 8. INTERACCIONES EN EL MODELO DE COX
# 9. PRESENTAR RESULTADOS: TABLA Y FOREST PLOT
# 10. (OPCIONAL) MODELO EN TIEMPO DISCRETO
#
# Requiere haber ejecutado antes 02_construir_trayectorias.R
# Las líneas marcadas con  # >>> ADAPTAR  deben cambiarse según su análisis.
##############################################


# 1. PREPARACIÓN ---------------------------------------------------------------
options(scipen = 999)
rm(list = ls())

# Carpeta de trabajo: debe contener la carpeta data/ (output/ se crea sola).
# Si no usan un proyecto de RStudio, indíquenla con:
# Session > Set Working Directory > Choose Directory, o con setwd():
# setwd("C:/Users/su_usuario/Documents/seminario")                 # >>> ADAPTAR
getwd()   # muestra la carpeta de trabajo actual

# install.packages(c("survival", "texreg"))   # solo la primera vez
library(tidyverse)
library(survival)    # Surv(), survfit(), survdiff(), coxph(), cox.zph()
library(texreg)      # tablas de modelos

dir.create("output", showWarnings = FALSE)


# 2. CARGAR LA BASE DE SUPERVIVENCIA --------------------------------------------
surv_data <- readRDS("output/surv_data.rds")

# Variables clave:
# - tiempo:    años desde 1° medio 2022 hasta la deserción o la censura (1, 2 o 3)
# - desercion: 1 = desertó ; 0 = censurado (sigue matriculado en 2025)
surv_data %>% count(tiempo, desercion)

# El objeto Surv() combina tiempo y evento. Los "+" indican casos censurados.
head(Surv(surv_data$tiempo, surv_data$desercion), 20)


# 3. KAPLAN-MEIER: SUPERVIVENCIA DE LA COHORTE ------------------------------------
# Estimador no paramétrico de S(t): proporción que PERMANECE en el sistema.
km_total <- survfit(Surv(tiempo, desercion) ~ 1, data = surv_data)
km_total
summary(km_total)
# - n.risk:   estudiantes en riesgo al inicio de cada año
# - n.event:  deserciones en ese año
# - survival: proporción acumulada que permanece
# - lower/upper 95% CI: intervalo de confianza

# Riesgo anual h(t) = deserciones / estudiantes en riesgo
tibble(tiempo = km_total$time, en_riesgo = km_total$n.risk, eventos = km_total$n.event) %>%
  mutate(riesgo_anual = round(eventos / en_riesgo, 3))

# Gráfico
plot(km_total, ylim = c(.7, 1), xaxt = "n", col = "#9467bd", lwd = 2,
     xlab = "Años desde 1° medio (2022)", ylab = "Proporción que permanece")
axis(1, at = 0:3)


# 4. KAPLAN-MEIER POR GRUPOS ---------------------------------------------------------
## 4.1 Por condición migrante ----
km_mig <- survfit(Surv(tiempo, desercion) ~ migrante, data = surv_data)   # >>> ADAPTAR
summary(km_mig)

## 4.2 Gráfico con ggplot (permite guardar y personalizar) ----
# Convertimos la salida de survfit en una tabla, agregando el punto de partida (t = 0)
km_tabla <- function(km) {
  s <- summary(km)
  tibble(tiempo = s$time, supervivencia = s$surv,
         inf = s$lower, sup = s$upper,
         grupo = if (is.null(s$strata)) "Total" else as.character(s$strata))
}
datos_km <- km_tabla(km_mig) %>%
  bind_rows(tibble(tiempo = 0, supervivencia = 1, inf = 1, sup = 1,
                   grupo = unique(.$grupo))) %>%
  mutate(grupo = recode(grupo, "migrante=0" = "Nativos", "migrante=1" = "Migrantes"))  # >>> ADAPTAR

ggplot(datos_km, aes(tiempo, supervivencia, color = grupo)) +
  geom_step(linewidth = 1.2) +
  scale_color_manual(values = c("#9467bd", "#6495ED")) +
  scale_x_continuous(breaks = 0:3) +
  labs(x = "Años desde 1° medio (2022)", y = "Proporción que permanece en el sistema",
       color = NULL) +
  theme_minimal(base_size = 13)
ggsave("output/kaplan_meier_migrante.png", width = 7, height = 4.5)

## 4.3 Por país de origen ----
km_pais <- survfit(Surv(tiempo, desercion) ~ pais, data = surv_data)
summary(km_pais)


# 5. PRUEBA LOG-RANK -----------------------------------------------------------------------
# H0: las curvas de supervivencia son iguales en todos los grupos.
survdiff(Surv(tiempo, desercion) ~ migrante, data = surv_data)
survdiff(Surv(tiempo, desercion) ~ pais, data = surv_data)
survdiff(Surv(tiempo, desercion) ~ dependencia, data = surv_data)
# p < 0,05 -> al menos un grupo tiene una trayectoria de permanencia distinta.
# Columna O = deserciones observadas ; E = esperadas si no hubiera diferencias.


# 6. MODELO DE COX -------------------------------------------------------------------------
# h_i(t) = h_0(t) * exp(b1*X1 + b2*X2 + ...)
# exp(b) = razón de riesgos (hazard ratio, HR):
#   HR > 1 mayor riesgo de desertar ; HR < 1 menor riesgo ; HR = 1 sin asociación
# ties = "efron": corrección para empates (muchos desertan el mismo año).

cox0 <- coxph(Surv(tiempo, desercion) ~ pais, data = surv_data, ties = "efron")
cox1 <- coxph(Surv(tiempo, desercion) ~ pais + hombre + extraedad + dependencia +
                tecnico_prof, data = surv_data, ties = "efron")        # >>> ADAPTAR
summary(cox1)

# HR con intervalos de confianza al 95 %
round(summary(cox1)$conf.int[, -2], 2)

# Interpretación (ejemplo):
# - paisVenezuela HR = 1,7: el riesgo de desertar de estudiantes venezolanos es
#   1,7 veces el de estudiantes chilenos (70 % mayor), controlando por las demás
#   variables.
# - HR = 0,4: riesgo 60 % menor.
# Ojo: un HR de 1,7 es un RIESGO 70 % mayor, no "70 % más probabilidad".

# ¿Mejora el ajuste al agregar covariables? (mismos casos)
anova(cox0, cox1)


# 7. SUPUESTO DE RIESGOS PROPORCIONALES ----------------------------------------------------
# El HR debe ser constante en el tiempo.
cox.zph(cox1)
# p > 0,05 -> no hay evidencia de que el supuesto se viole.
# Si una variable lo viola, se puede estratificar por ella:
# coxph(Surv(tiempo, desercion) ~ pais + hombre + strata(dependencia), data = surv_data)


# 8. INTERACCIONES EN EL MODELO DE COX -----------------------------------------------------
# ¿La brecha de riesgo entre migrantes y nativos depende de la dependencia?
cox_a <- coxph(Surv(tiempo, desercion) ~ migrante + municipal + hombre + extraedad,
               data = surv_data)
cox_b <- coxph(Surv(tiempo, desercion) ~ migrante * municipal + hombre + extraedad,
               data = surv_data)                                          # >>> ADAPTAR
anova(cox_a, cox_b)       # ¿la interacción mejora el ajuste?
round(exp(coef(cox_b)), 2)

# HR de ser migrante en cada tipo de establecimiento:
b <- coef(cox_b)
c(no_municipal = exp(b["migrante"]),
  municipal    = exp(b["migrante"] + b["migrante:municipal"]))
# Si la interacción no es significativa, se informa el modelo sin interacción.

# Interacción con una variable de varias categorías (ej. nacionalidad x dependencia)
cox_c <- coxph(Surv(tiempo, desercion) ~ nacionalidad * municipal + hombre,
               data = surv_data)
round(exp(coef(cox_c)), 2)


# 9. PRESENTAR RESULTADOS: TABLA Y FOREST PLOT ----------------------------------------------
## 9.1 Tabla con HR e intervalos de confianza ----
hr <- summary(cox1)$conf.int
screenreg(cox1,
          override.coef = list(hr[, "exp(coef)"]),
          override.ci.low = list(hr[, "lower .95"]),
          override.ci.up = list(hr[, "upper .95"]),
          ci.test = 1)        # * = el IC no incluye 1

htmlreg(cox1, file = "output/tabla_cox.doc",
        override.coef = list(hr[, "exp(coef)"]),
        override.ci.low = list(hr[, "lower .95"]),
        override.ci.up = list(hr[, "upper .95"]),
        ci.test = 1, caption = "Modelo de Cox: razones de riesgo de deserción (HR e IC 95 %)",
        caption.above = TRUE)

## 9.2 Forest plot ----
hr %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  mutate(variable = recode(variable,                                     # >>> ADAPTAR etiquetas
                           "paisVenezuela" = "Venezuela (ref. Chile)",
                           "paisPerú" = "Perú (ref. Chile)",
                           "paisOtro país" = "Otro país (ref. Chile)",
                           "hombre" = "Hombre",
                           "extraedad" = "Extraedad",
                           "dependenciaPart. subvencionado" = "Part. subvencionado (ref. municipal)",
                           "dependenciaPart. pagado" = "Part. pagado (ref. municipal)",
                           "tecnico_prof" = "Técnico-profesional")) %>%
  ggplot(aes(x = `exp(coef)`, y = fct_rev(fct_inorder(variable)))) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_pointrange(aes(xmin = `lower .95`, xmax = `upper .95`), color = "#9467bd") +
  scale_x_log10() +
  labs(x = "Razón de riesgos (HR, escala logarítmica) e IC 95 %", y = NULL) +
  theme_minimal(base_size = 13)
ggsave("output/forest_plot_cox.png", width = 8, height = 4.5)


# 10. (OPCIONAL) MODELO EN TIEMPO DISCRETO ----------------------------------------------------
# Alternativa al modelo de Cox cuando el tiempo es anual (muchos empates).
# Se construye una base PERSONA-AÑO: una fila por cada año en riesgo.
persona_anio <- surv_data %>%
  select(MRUN_unico, tiempo, desercion, pais, hombre, extraedad, dependencia, tecnico_prof) %>%
  uncount(tiempo, .remove = FALSE, .id = "anio") %>%           # repite cada fila 'tiempo' veces
  mutate(evento = if_else(anio == tiempo & desercion == 1, 1, 0))

# Ejemplo: un estudiante que deserta en el año 2 tiene dos filas (anio 1 y 2),
# con evento = 0 en el año 1 y evento = 1 en el año 2.
persona_anio %>% filter(tiempo == 2, desercion == 1) %>% head(4)

# Regresión logística: el riesgo anual se modela según el año y las covariables
m_discreto <- glm(evento ~ factor(anio) + pais + hombre + extraedad + dependencia +
                    tecnico_prof, family = binomial, data = persona_anio)
round(exp(coef(m_discreto)), 2)    # odds ratios: similares a los HR del modelo de Cox
