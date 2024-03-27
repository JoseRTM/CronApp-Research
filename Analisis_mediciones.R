#Analisis grupo control

#Paquetes 
install.packages("ggplot2")
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)
install.packages("readxl")
library(readxl)

#Cargar base
Base_pacientes <- read_excel("Base_pacientes.xlsx")
View(Base_pacientes)
base_mediciones <- Base_pacientes

# Convertir la columna FECHA a formato de fecha
base_mediciones$FECHA <- as.Date(base_mediciones$FECHA, format="%d/%m/%Y")

#Box plot comparacion mediciones mañana noche
ggplot(base_mediciones, aes(x = factor(1), y = MEDICION_MANANA)) +
  geom_boxplot() +
  geom_boxplot(aes(x = factor(2), y = MEDICION_NOCHE)) +
  labs(title = "Comparación de Mediciones de la Mañana y de la Noche",
       x = "Período",
       y = "Mediciones") +
  scale_x_discrete(labels = c("Mañana", "Noche"))

#Box plot comparación mediciones por genero
s_noche <- ggplot(base_mediciones, aes(x = factor(SEXO), y = MEDICION_NOCHE)) +
  geom_boxplot() +
  labs(title = "Noche", x = "Género", y = "Medición de Glucosa") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, max(base_mediciones$MEDICION_NOCHE) * 1.1)) +
  expand_limits(y = 0)
s_dia <- ggplot(base_mediciones, aes(x = factor(SEXO), y = MEDICION_MANANA)) +
  geom_boxplot() +
  labs(title = "Mañana", x = "Género", y = "Medición de Glucosa") +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 500))

box_genero <- grid.arrange(s_dia, s_noche, ncol = 2)







