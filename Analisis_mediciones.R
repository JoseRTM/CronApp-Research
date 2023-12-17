
base_mediciones <- Base_pacientes

# Convertir la columna FECHA a formato de fecha
base_mediciones$FECHA <- as.Date(base_mediciones$FECHA, format="%d/%m/%Y")

# Tendencia de glucosa en la mañana
ggplot(base_mediciones, aes(x=FECHA, y=MEDICION_MANANA, color=factor(ID))) +
  geom_line() +
  labs(title="Tendencia de Glucosa en la Mañana a lo largo del tiempo", x="Fecha", y="Medición de Glucosa") +
  theme_minimal()

# Tendencia de glucosa en la noche
ggplot(base_mediciones, aes(x=FECHA, y=MEDICION_NOCHE, color=factor(ID))) +
  geom_line() +
  labs(title="Tendencia de Glucosa en la Noche a lo largo del tiempo", x="Fecha", y="Medición de Glucosa") +
  theme_minimal()

#Box plot comparación por genero
dia  <- ggplot(base_mediciones, aes(x=factor(SEXO), y=MEDICION_MANANA)) +
  geom_boxplot() +
  labs(title="Comparación de Glucosa en la Mañana por Género", x="Género", y="Medición de Glucosa") +
  theme_minimal()
noche <- ggplot(base_mediciones, aes(x=factor(SEXO), y=MEDICION_NOCHE)) +
  geom_boxplot() +
  labs(title="Comparación de Glucosa en la Noche por Género", x="Género", y="Medición de Glucosa") +
  theme_minimal()

grid.arrange(dia, noche, ncol = 2)
