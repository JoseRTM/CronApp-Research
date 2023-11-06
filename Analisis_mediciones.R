
base_mediciones <- Base


#Estadistica descriptiva mediciones noche
Est_desc <- base_mediciones %>% 
  group_by(ID) %>% 
  summarise(M_mean=mean(MEDICION_MANANA,na.rm = T),
            M_Min=min(MEDICION_MANANA,na.rm = T),
            M_Max=max(MEDICION_MANANA,na.rm = T),
            M_desv = sd(MEDICION_MANANA,na.rm = T),
            N_Mean=mean(MEDICION_NOCHE,na.rm = T),
            N_Min=min(MEDICION_NOCHE,na.rm = T),
            N_Max=max(MEDICION_NOCHE,na.rm = T),
            N_desv = sd(MEDICION_NOCHE,na.rm = T),
            Dif_Promedio = mean(abs(MEDICION_MANANA - MEDICION_NOCHE), na.rm = TRUE))

#Las mediciones de la mañana están más dispersas que las de la noche
#En la mañana la medicion más alta fue de 231 (p3), y la más baja de 97 (p2)
#En la noche la medicion más alta fue de 200 (p3), y la más baja de 110 (p2)
#Tanto  en la mañana como en la noche el paciente 3 tiene mediciones mas dispersas
# También tiene la mayor dif promedio entre la mañana y la noche


#Grafico comparativo 1:

datos_combinados <- bind_rows(
  base_mediciones %>%
    select(FECHA, MEDICION_MANANA, ID) %>%
    mutate(Tipo = "Mañana"),
  base_mediciones %>%
    select(FECHA, MEDICION_NOCHE, ID) %>%
    mutate(Tipo = "Noche")
)

ggplot(datos_combinados, aes(x = FECHA, y = ifelse(Tipo == "Mañana", MEDICION_MANANA, MEDICION_NOCHE), color = Tipo)) +
  geom_point() +
  labs(title = "Comaparacion entre los niveles de Glicemia en la mañana y noche",
       x = "Fecha", y = "Nivel de Glicemia") +
  facet_wrap(~ ID, scales = "free_x", ncol = 1) +
  scale_color_manual(values = c("Mañana" = "orange", "Noche" = "blue"))

#Las mediciones del paciente 3 se ven mucho más dispersas, lo que tiene sentido considerando lo alta
#que fue su desviacion estandar y dif promedio entre la mañana y la noche

