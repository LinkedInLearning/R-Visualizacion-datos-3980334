ruta <- "C:/Users/linkedin/Dropbox/LinkedIn/"
df <- read.csv(paste0(ruta,"flights_small.csv"), sep=";")
df

###################
# ggplot2 Básicos #
###################

# Instalar ggplot2
# install.packages("ggplot2")

# Carga del paquete ggplot2
library(ggplot2)


# Primer gráfico con ggplot2: puntos
# Se construye en capas: primero se define el dataset y los mapeos estéticos
ggplot(data = df, aes(x = DEPARTURE_DELAY, y = ARRIVAL_DELAY)) +
  geom_point()

ggplot(data = df, aes(x = DEPARTURE_DELAY, y = ARRIVAL_DELAY)) +
  geom_point() + geom_abline()


# Es posible cambiar el tipo de gráfico simplemente cambiando la capa geom
ggplot(data = df, aes(x = DEPARTURE_DELAY)) +
  geom_histogram()

# También se puede empezar con una estructura vacía y añadir capas una a una
base <- ggplot(data = df, aes(x = DISTANCE, y = AIR_TIME))
base + geom_point()

# El eje x y el eje y se definen dentro de aes()
# La función aes() define los mapeos estéticos: qué variable va en qué eje


# También se puede construir con variables categóricas
ggplot(data = df, aes(x = AIRLINE)) +
  geom_bar()


# Entender el aes()
ggplot(data = df, aes(x = DEPARTURE_DELAY, 
                      y = ARRIVAL_DELAY, 
                      colour = AIRLINE)) +
  geom_point()

ggplot(data = df, aes(x = DEPARTURE_DELAY, y = ARRIVAL_DELAY)) +
  geom_point() + geom_smooth(aes(colour = AIRLINE))




########################
# Barras e Histogramas # 
########################

library(ggplot2)

# Histograma básico del retraso en salida
ggplot(df, aes(x = DEPARTURE_DELAY)) +
  geom_histogram()

# Ajustar el número de bins manualmente
ggplot(df, aes(x = DEPARTURE_DELAY)) +
  geom_histogram(bins = 50)

# También se puede controlar el ancho de los intervalos en lugar del número de bins
ggplot(df, aes(x = DEPARTURE_DELAY)) +
  geom_histogram(binwidth = 10)

# Personalización del color de las barras y del borde
ggplot(df, aes(x = DEPARTURE_DELAY)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white")

# Histograma del tiempo de vuelo con colores diferentes
ggplot(df, aes(x = AIR_TIME)) +
  geom_histogram(binwidth = 20, fill = "darkorange", color = "black")


# Gráfico de barras para contar vuelos por aerolínea
ggplot(df, aes(x = AIRLINE)) +
  geom_bar()

# Cambiar el color de las barras
ggplot(df, aes(x = AIRLINE)) +
  geom_bar(fill = "skyblue")


# Añadir contornos blancos a las barras
ggplot(df, aes(x = AIRLINE)) +
  geom_bar(fill = "tomato", color = "white")

# Gráfico de barras con variable mes
ggplot(df, aes(x = factor(MONTH))) +
  geom_bar(fill = "darkgreen", color = "black")

# Reordenar las barras por frecuencia con fct_infreq()
#install.packages("forcats")
library(forcats)

ggplot(df, aes(x = fct_infreq(AIRLINE))) +
  geom_bar(fill = "purple", color = "white")



##########################################
# Barras agrupadas, boxplot y dispersión #
##########################################


# Gráfico de barras agrupadas: número de vuelos por aerolínea y por mes
ggplot(df, aes(x = factor(MONTH), fill = AIRLINE)) +
  geom_bar(position = "dodge")

# Gráfico de barras apiladas: misma información, distinta presentación
ggplot(df, aes(x = factor(MONTH), fill = AIRLINE)) +
  geom_bar(position = "stack")


# Boxplot del tiempo de vuelo por aerolínea
ggplot(df, aes(x = AIRLINE, y = AIR_TIME)) +
  geom_boxplot()

# Boxplot del tiempo de vuelo por aerolínea y aeropuerto

tabla_origen <- sort(table(df$ORIGIN_AIRPORT), decreasing = TRUE)
top1 <- names(tabla_origen)[1]
top2 <- names(tabla_origen)[2]

# Crear nueva variable categórica
df$TIPO_AEROPUERTO <- ifelse(df$ORIGIN_AIRPORT == top1, "Principal",
                             ifelse(df$ORIGIN_AIRPORT == top2, "Secundario", "Otros"))

# Convertir a factor para análisis
df$TIPO_AEROPUERTO <- factor(df$TIPO_AEROPUERTO, levels = c("Principal", "Secundario", "Otros"))

ggplot(df, aes(x = AIRLINE, y = AIR_TIME, colour = TIPO_AEROPUERTO)) +
  geom_boxplot()
ggsave(filename = "box.jpg",
       height = 1080, width = 1920, units = "px")

# Diagrama de dispersión entre distancia y tiempo de vuelo
ggplot(df, aes(x = DISTANCE, y = AIR_TIME)) +
  geom_point() 
  #geom_point(alpha = 0.3)
  #geom_point(size = 1)


# Dispersión con línea de regresión lineal simple
ggplot(df, aes(x = DEPARTURE_DELAY, y = ARRIVAL_DELAY)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE)

# Con rug plot para ver la concentración de puntos en ejes
ggplot(df, aes(x = DEPARTURE_DELAY, y = ARRIVAL_DELAY)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  geom_rug()


# Función de agregación en barras
# Gráfico de barras agrupadas: retraso medio por aerolínea y por mes


ggplot(df, aes(x = factor(MONTH), y = ARRIVAL_DELAY, fill = AIRLINE)) +
  stat_summary(fun = mean, geom = "bar", position = "dodge") +
  labs(title = "Retraso medio en llegada por aerolínea y mes",
       x = "Mes", y = "Retraso medio (minutos)")


###############################
# color, formas y dimensiones # 
###############################

df$AEROLINEA <- ifelse(df$AIRLINE == "AA", "Destacada", "Otras")
df$DAY_OF_WEEK <- factor(df$DAY_OF_WEEK)

# Diagrama de dispersión con color según la aerolínea
ggplot(df, aes(x = DEPARTURE_DELAY, y = ARRIVAL_DELAY)) +
  geom_point() + geom_smooth(aes(color = DAY_OF_WEEK))

# Añadir forma según día de la semana
ggplot(df, aes(x = DEPARTURE_DELAY, y = ARRIVAL_DELAY, 
               color = DAY_OF_WEEK, 
               size = AIR_TIME)) +
  geom_point()


ggplot(df, aes(x = DEPARTURE_DELAY, y = ARRIVAL_DELAY, 
               color = DAY_OF_WEEK)) +
  geom_point(alpha = 0.6) +
  scale_color_brewer(palette = "Blues")




# Boxplot con color y forma más estética
ggplot(df, aes(x = DAY_OF_WEEK, y = ARRIVAL_DELAY, 
               fill = DAY_OF_WEEK)) +
  geom_boxplot(show.legend = FALSE)



#### Color destacado
ggplot(df, aes(x = DEPARTURE_DELAY, y = ARRIVAL_DELAY,
               color = AEROLINEA)) +
  geom_point(alpha = 0.2) +
  scale_color_manual(values = c("Destacada" = "red", "Otras" = "black"))



###################
# Ajustes ggplot2 # 
###################

library(ggplot2)

# Gráfico base con dispersión
p <- ggplot(df, aes(x = DEPARTURE_DELAY, y = ARRIVAL_DELAY)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred")

# Añadir títulos, subtítulo y etiquetas de ejes
p + labs(
  title = "Relación entre retraso en salida y llegada",
  subtitle = "Datos de vuelos comerciales en EE.UU.",
  x = "Retraso en salida (min)",
  y = "Retraso en llegada (min)",
  caption = "Fuente: dataset flights"
)

# Personalización de tema con theme()
p + labs(
  title = "Relación entre retraso en salida y llegada",
  x = "Retraso en salida (min)",
  y = "Retraso en llegada (min)"
) +
  theme_minimal()

# Otros temas disponibles
p + theme_classic()
p + theme_light()
p + theme_void()

# Modificar el texto del gráfico manualmente
p + theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 11, color = "gray20")
  )

# Personalizar leyenda
ggplot(df, aes(x = factor(MONTH), fill = AIRLINE)) +
  geom_bar(position = "dodge") +
  labs(title = "Número de vuelos por aerolínea y mes",
       x = "Mes",
       y = "Número de vuelos",
       fill = "Aerolínea") +
  theme_classic() +
  theme(legend.position = "bottom")

# Cambiar escala de colores
ggplot(df, aes(x = DISTANCE, y = AIR_TIME, color = factor(DAY_OF_WEEK))) +
  geom_point(alpha = 0.5) +
  scale_color_brewer(palette = "Set2") +
  labs(color='Día Semana') +
  theme_minimal()

# Otra opción con escala manual
ggplot(df, aes(x = AIRLINE, y = AIR_TIME, fill = factor(DAY_OF_WEEK))) +
  geom_boxplot() +
  scale_fill_manual(values = c("red", "blue", "orange", "purple", "pink", "cyan", "darkgreen")) +
  labs(fill='Día Semana') +
  theme_bw()

#################################
# Guardar y automatizar ggplot2 # 
#################################


# Crear un gráfico y guardarlo con ggsave()
p <- ggplot(df, aes(x = DEPARTURE_DELAY, y = ARRIVAL_DELAY)) +
  geom_point(alpha = 0.4, color = "steelblue") +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(title = "Relación entre retraso en salida y llegada")

# Guardar el gráfico como PNG
ggsave("scatter_retrasos.png", plot = p, width = 8, height = 6)

# Guardar en PDF
ggsave("scatter_retrasos.pdf", plot = p)

# También se puede guardar en JPG o SVG simplemente cambiando la extensión
ggsave("scatter_retrasos.jpg", plot = p, dpi = 300)

# --------
# Crear funciones personalizadas para automatizar gráficos

# Función para crear un histograma dado el nombre de una variable numérica
crear_histograma <- function(data, variable, bins = 30, color = "steelblue") {
  ggplot(data, aes(x = .data[[variable]])) +
    geom_histogram(bins = bins, fill = color, color = "white") +
    labs(title = paste("Histograma de", variable),
         x = variable,
         y = "Frecuencia") +
    theme_minimal()
}

# Uso de la función para varias variables
crear_histograma(df, "DEPARTURE_DELAY")
crear_histograma(df, "AIR_TIME", bins = 40, color = "darkorange")

# Función para guardar automáticamente un gráfico con nombre basado en la variable
guardar_histograma <- function(data, variable) {
  plot <- crear_histograma(data, variable)
  nombre_archivo <- paste0("hist_", variable, ".png")
  ggsave(nombre_archivo, plot = plot, width = 7, height = 5)
}

# Guardar histogramas de varias variables
guardar_histograma(df, "DEPARTURE_DELAY")
guardar_histograma(df, "AIR_TIME")
guardar_histograma(df, "DISTANCE")
