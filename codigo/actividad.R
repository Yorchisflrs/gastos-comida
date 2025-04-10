# ------------------------------------------------------------------------------
# ANÁLISIS ESTADÍSTICO: GASTOS EN COMIDA (PUNO vs. JULIACA)
# ------------------------------------------------------------------------------

# --------------------------------------
# 1. INSTALAR PAQUETES (SI ES NECESARIO)
# --------------------------------------
if (!require("dplyr")) install.packages("dplyr", repos = "https://cloud.r-project.org")
if (!require("ggplot2")) install.packages("ggplot2", repos = "https://cloud.r-project.org")
if (!require("carData")) install.packages("carData", repos = "https://cloud.r-project.org") # Paquete crítico
if (!require("ggpubr")) install.packages("ggpubr", repos = "https://cloud.r-project.org")

# --------------------------------------
# 2. GENERAR DATOS (n = 15 POR GRUPO)
# --------------------------------------
set.seed(123)
puno <- round(rnorm(15, mean = 500, sd = 80), 0)    # Media: 500 soles (Puno)
juliaca <- round(rnorm(15, mean = 600, sd = 90), 0)  # Media: 600 soles (Juliaca)

datos <- data.frame(
  Ciudad = rep(c("Puno", "Juliaca"), each = 15),
  Gasto = c(puno, juliaca)
)

# --------------------------------------
# 3. ESTADÍSTICOS DESCRIPTIVOS
# --------------------------------------
library(dplyr)
estadisticos <- datos %>%
  group_by(Ciudad) %>%
  summarise(
    Promedio = mean(Gasto),
    Desviación = sd(Gasto),
    CV = (Desviación / Promedio) * 100,
    Mínimo = min(Gasto),
    Máximo = max(Gasto)
  )

# --------------------------------------
# 4. PRUEBA T DE STUDENT (HIPÓTESIS)
# --------------------------------------
cat("Hipótesis:\n")
cat("- H0: μ_puno = μ_juliaca (No hay diferencia)\n")
cat("- H1: μ_puno ≠ μ_juliaca (Existe diferencia)\n\n")

prueba_t <- t.test(Gasto ~ Ciudad, data = datos)

# --------------------------------------
# 5. GRÁFICOS (USANDO ggpubr)
# --------------------------------------
library(ggplot2)
library(ggpubr)

# Boxplot
boxplot <- ggplot(datos, aes(x = Ciudad, y = Gasto, fill = Ciudad)) +
  geom_boxplot() +
  labs(title = "Distribución de gastos por ciudad")

# Histograma
histograma <- ggplot(datos, aes(x = Gasto, fill = Ciudad)) +
  geom_histogram(binwidth = 50, alpha = 0.7, position = "identity")

# Combinar gráficos
graficos <- ggarrange(boxplot, histograma, ncol = 2)

# Guardar
ggsave("graficos.png", graficos, width = 12, height = 6)

# --------------------------------------
# 6. RESULTADOS
# --------------------------------------
cat("Estadísticos:\n")
print(estadisticos)
cat("\nPrueba t:\n")
print(prueba_t)

