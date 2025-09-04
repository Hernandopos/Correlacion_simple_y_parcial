# ==========================================================
# Universidad de Guanajuato
# Profesora: Lesly Flores
# Estudiantes: Hernando Posadas, Sergio Segoviano y Mariana Rodríguez
# Comparar correlacion simple y correlación parcial 

# ==========================================================

#install.packages("devtools")
devtools::install_github("ropengov/rqog")
library(rqog)
qog <- read_qog(which_data = "basic", data_type = "cross-sectional")

names(qog)
library(ppcor)
library(dplyr)
library(ggplot2)
#-----------------------------------------------------------
# (2) Seleccionar variables

#  bti_ci: Conflict Intensity (Quality of goverment)
#  bti_ep: Economic Performance (Quality of goverment)
#  bti_prp: Private Property (Quality of goverment)

base <- qog %>%
  select(
    pais = cname,
    intensidad_conflicto = bti_ci,
    rendimiento_economico = bti_ep,
    propiedad_privada = bti_prp)
#-----------------------------------------------------------
# (3) Limpiar NAs
base <- na.omit(base)
#-----------------------------------------------------------
# (4) Correlación simple (sin control)
# Relación "bruta" entre intensidad en el confilicto y rendimiento económico
cor_simple <- cor(base$intensidad_conflicto, base$rendimiento_economico)
cor_simple
#-----------------------------------------------------------
# (5) Correlación parcial (controlando propiedad_privada)
# Mide la asociación entre intensidad en el conflicto y el redimiento económico "controlando" el efecto de la Propiedad privada
pc <- pcor.test(base$intensidad_conflicto, base$rendimiento_economico, base$propiedad_privada)
pc

#-----------------------------------------------------------
#  (6)Crear un gráfico comparativo
resultados <- data.frame(
  Tipo = c("Correlación Simple", "Correlación Parcial"),
  Coeficiente = c(-0.478, -0.169),
  Significancia = c("Significativa", "Marginal (p=0.051)")
)

library(ggplot2)
ggplot(resultados, aes(x = Tipo, y = Coeficiente, fill = Significancia)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Comparación de Correlaciones: Economía vs Intensidad de Conflicto",
       subtitle = "Efecto de controlar por derechos de propiedad privada",
       y = "Coeficiente de Correlación") +
  theme_minimal() +
  scale_fill_manual(values = c("Marginal (p=0.051)" = "orange", "Significativa" = "steelblue"))
