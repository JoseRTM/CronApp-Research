#######################
###CRONAPP RESEARCH###
#######################

#PACKAGES 
pkg_names <- c("skimr","rio","readxl","dplyr", "ggplot2","epiDisplay",
               "tidyverse", "httr", "nortest","gridExtra","stargazer","gt",
               "lubridate")

# Packages
for (package in pkg_names) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package)
    library(package, character.only = TRUE)
  }
}

# LOADING THE DATA
# Set the URL of the Excel file in the GitHub repository
file_url <- "https://github.com/JoseRTM/CronApp-Research/raw/main/datos_full.xlsx"

# Download the file
temp_file <- tempfile(fileext = ".xlsx")
GET(file_url, write_disk(temp_file, overwrite = TRUE))

# Load the data
register <- import(temp_file, sheet = 1)
baseline <- import(temp_file, sheet = 3)

# JOIN
baseline <- baseline %>% 
  dplyr::select(-id)
data <- register %>% 
  dplyr::select(-id) %>% 
  inner_join(baseline, by = "user_id", relationship = "many-to-many") %>% 
  dplyr::select(-created_at, -updated_at) %>% 
  mutate(imc = weight/(height/100)^2,
         edad = year(Sys.Date()) - year(edad))

# n 

sum(table(unique(data$user_id)))
# 40 participantes

# STRUCTURE OF THE DATA
str(data)

# BLOODSUGAR DISTRIBUTION
ggplot(data, aes(x = bloodSugar)) +
  geom_density()

# A LOT OF OUTLIERS
# RULE OF THUMB Q3-1.5*IQR
data <- data %>% 
  filter(bloodSugar<=(quantile(bloodSugar,0.75)+(1.5*IQR(bloodSugar))),
         bloodSugar>=(quantile(bloodSugar,0.25)-(1.5*IQR(bloodSugar))))
# way better

# N° registries by patient
data <- data %>%
  group_by(user_id) %>%
  mutate(n_medicion = n()) %>%
  ungroup() %>% 
  arrange(user_id)

summary(data$n_medicion)

# DUPLICATES
data <- data %>%
  group_by(user_id, measured_at) %>%
  distinct() %>%
  ungroup()
data <- data %>%
  group_by(user_id) %>%
  mutate(n_medicion = n()) %>% 
  ungroup()

# FILTRAR A LA GENTE QUE NO SE MIDIO
data <- data %>% 
  filter(n_medicion > 3)

mediciones_por_usuario <- data %>%
  group_by(user_id) %>%
  summarize(n_medicion = n())
# Graficar la distribución del número de mediciones
ggplot(mediciones_por_usuario, aes(x = n_medicion)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(x = "Número de Mediciones por Usuario", y = "Frecuencia", title = "Distribución del Número de Mediciones por Usuario") +
  theme_minimal()

##########
data <- data %>%
  mutate(measured_date = as.Date(measured_at))

data_promedio_diario <- data %>%
  group_by(user_id, measured_date) %>%
  summarize(promedio_bloodSugar = mean(bloodSugar, na.rm = TRUE)) %>%
  ungroup() 

ggplot(data_promedio_diario, aes(x = measured_date, y = promedio_bloodSugar, group = user_id)) +
  geom_line(alpha = 0.3) + # Líneas individuales con transparencia
  geom_smooth(se = FALSE) + # Líneas de tendencia suavizada por 'label'
  facet_wrap(~ user_id, scales = "free_y") + # Facetas por 'label'
  theme_minimal() +
  labs(x = "Fecha", y = "Promedio de Glucosa en Sangre", title = "Tendencias de Glucosa en Sangre por Paciente y Grupo") +
  theme(legend.position = "none") #

# BASELINE DESCRIPTIVE STATISTICS
data_baseline <- data %>% 
  group_by(user_id) %>% 
  slice(1) %>% 
  ungroup()

sum(table(unique(data_baseline$user_id)))

# PROPORTION OF SMOKERS
mean(data_baseline$smoker) ## 33%
summary(data_baseline$weight)
summary(data_baseline$height)
summary(data$imc) # its a character



colSums(is.na(data_baseline))
# normality
model <- lmer(bloodSugar ~ 1 + (1 | user_id), data = data)
residuals <- residuals(modelo)

# QQ-plot
qqnorm(residuals)
qqline(residuals, col = "red")

# Shapiro-Wilk
shapiro.test(residuals)

mean_by_patient <- data %>%
  group_by(user_id) %>%
  summarize(promedio_bloodSugar = mean(bloodSugar))

shapiro.test(mean_by_patient$promedio_bloodSugar)
###############
### COSAS QUE HACER
# HACER LA LIMPIEZA DE LA BASE DE DATOS DE CONTROL
# HACER UN MERGE ENTRE LA BASE DE LA APP Y LA BASE DE CONTROL
# REPRODUCTIR EL ANÁLISIS DE FORMA COMPARATIVA ENTRE LOS DOS GRUPOS


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







