ruta <- "C:/Users/linkedin/Dropbox/LinkedIn/"
df <- read.csv(paste0(ruta,"flights_small.csv"), sep=";")
df

################
# Intro Plotly # 
################

# install.packages("plotly")
library(plotly)
library(ggplot2)

# Gráfico básico interactivo con plot_ly: dispersión entre retrasos
plot_ly(data = df, x = ~DEPARTURE_DELAY, y = ~ARRIVAL_DELAY, 
        type = "scatter", mode = "markers")

# Gráfico interactivo con etiquetas emergentes (hover info)
plot_ly(data = df,
        x = ~DEPARTURE_DELAY,
        y = ~ARRIVAL_DELAY,
        type = "scatter",
        mode = "markers",
        text = ~paste("Vuelo:", FLIGHT_NUMBER,
                      "<br>Salida:", DEPARTURE_TIME,
                      "<br>Llegada:", ARRIVAL_TIME),
        hoverinfo = "text")

# Histograma interactivo del tiempo de vuelo
plot_ly(data = df, x = ~AIR_TIME, type = "histogram")

# Gráfico de barras interactivo: número de vuelos por aerolínea
plot_ly(data = df, x = ~AIRLINE, type = "histogram")


# Conversión de un gráfico ggplot en uno interactivo con ggplotly
p <- ggplot(df, aes(x = DEPARTURE_DELAY, y = ARRIVAL_DELAY)) +
  geom_point(alpha = 0.4, color = "blue") +
  labs(title = "Relación entre retraso en salida y llegada")

ggplotly(p)


##############################
# Barras, sectores y boxplot #
##############################

library(plotly)
library(dplyr)

# Gráfico de barras interactivo: número de vuelos por aerolínea
df_bar <- df %>%
  count(AIRLINE) 

plot_ly(data = df_bar, x = ~AIRLINE, y = ~n, type = "bar")

# Ordenado
df_bar <- df %>%
  count(AIRLINE) %>%
  mutate(AIRLINE = factor(AIRLINE, 
                          levels = AIRLINE[order(n, decreasing = TRUE)]))

# Gráfico de barras ordenado
plot_ly(data = df_bar, x = ~AIRLINE, y = ~n, type = "bar")


# Gráfico de barras apiladas: vuelos por mes y aerolínea
df_apilado <- df %>%
  count(MONTH, AIRLINE)

plot_ly(data = df_apilado, 
        x = ~AIRLINE, 
        y = ~n, 
        color = ~factor(MONTH), 
        type = "bar") %>%
  layout(barmode = "stack")

# Gráfico de sectores interactivo: proporción de vuelos por aerolínea
df_pie <- df %>%
  count(AIRLINE) %>%
  mutate(porcentaje = round(100 * n / sum(n), 1))

plot_ly(df_pie, labels = ~AIRLINE, values = ~n, type = "pie",
        textinfo = "label+percent", hoverinfo = "text",
        text = ~paste(AIRLINE, "<br>", porcentaje, "%"))

# Mejorado
# Crear resumen y ordenar de forma decreciente
df_pie <- df %>%
  count(AIRLINE) %>%
  mutate(porcentaje = round(100 * n / sum(n), 1)) %>%
  arrange(desc(n)) %>%
  mutate(AIRLINE = factor(AIRLINE, levels = AIRLINE))

# Gráfico de sectores desde las 12 en punto, ordenado
plot_ly(df_pie,
        labels = ~AIRLINE,
        values = ~n,
        type = "pie",
        sort = FALSE,
        direction = "clockwise",
        rotation = 0,
        textinfo = "label+percent",
        hoverinfo = "text",
        text = ~paste(AIRLINE, "<br>", porcentaje, "%"))




# Boxplot de tiempo de vuelo por día de la semana
plot_ly(df, x = ~factor(DAY_OF_WEEK), y = ~AIR_TIME, type = "box")


##############
# Dispersión #
##############


library(plotly)

# Dispersión básica entre retraso en salida y llegada
plot_ly(df, x = ~DEPARTURE_DELAY, y = ~ARRIVAL_DELAY,
        type = "scatter", mode = "markers")

# Dispersión con color según aerolínea
plot_ly(df, x = ~DEPARTURE_DELAY, y = ~ARRIVAL_DELAY,
        color = ~AIRLINE,
        type = "scatter", mode = "markers")

# Añadir texto emergente personalizado
plot_ly(df,
        x = ~DEPARTURE_DELAY,
        y = ~ARRIVAL_DELAY,
        type = "scatter", mode = "markers",
        color = ~AIRLINE,
        text = ~paste("Vuelo:", FLIGHT_NUMBER,
                      "<br>Aerolínea:", AIRLINE,
                      "<br>Salida:", DEPARTURE_TIME,
                      "<br>Llegada:", ARRIVAL_TIME),
        hoverinfo = "text")

# Añadir una línea de regresión lineal con modelo previo
modelo <- lm(ARRIVAL_DELAY ~ DEPARTURE_DELAY, data = df)

# Crear datos para la línea
linea <- data.frame(
  DEPARTURE_DELAY = seq(min(df$DEPARTURE_DELAY, na.rm = TRUE),
                        max(df$DEPARTURE_DELAY, na.rm = TRUE), 
                        length.out = 100)
)
linea$ARRIVAL_DELAY <- predict(modelo, newdata = linea)

# Añadir línea al gráfico de dispersión
plot_ly(df, x = ~DEPARTURE_DELAY, y = ~ARRIVAL_DELAY,
        type = "scatter", mode = "markers", alpha = 0.4) %>%
  add_lines(data = linea, x = ~DEPARTURE_DELAY, y = ~ARRIVAL_DELAY,
            line = list(color = "red", width = 2),
            name = "Regresión lineal")
library(plotly)

# Calcular la correlación entre retraso en salida y llegada
correlacion <- cor(df$DEPARTURE_DELAY, df$ARRIVAL_DELAY, 
                   use = "complete.obs")
cor_texto <- paste("Correlación:", round(correlacion, 2))

# Gráfico de dispersión con texto de correlación añadido
plot_ly(df, x = ~DEPARTURE_DELAY, y = ~ARRIVAL_DELAY,
        type = "scatter", mode = "markers", alpha = 0.4) %>%
  layout(
    title = "Relación entre retraso en salida y llegada",
    annotations = list(
      x = 0.05,
      y = 0.95,
      xref = "paper",
      yref = "paper",
      text = cor_texto,
      showarrow = FALSE,
      font = list(size = 14)
    )
  )

# 3d

# Limpiamos NAs para las 3 variables que se van a usar
df_3d <- df[!is.na(df$AIR_TIME) & !is.na(df$DISTANCE) & !is.na(df$DEPARTURE_DELAY), ]

# Gráfico 3D
df_3d$hover <- paste(
  "Aerolínea:", df_3d$AIRLINE,
  "<br>Vuelo:", df_3d$FLIGHT_NUMBER,
  "<br>Duración:", df_3d$AIR_TIME, "min",
  "<br>Distancia:", df_3d$DISTANCE, "mi",
  "<br>Retraso salida:", df_3d$DEPARTURE_DELAY, "min"
)

# Gráfico 3D con hover personalizado
plot_ly(df_3d,
        x = ~AIR_TIME,
        y = ~DISTANCE,
        z = ~DEPARTURE_DELAY,
        color = ~AIRLINE,
        colors = "Set2",
        type = "scatter3d",
        mode = "markers",
        text = ~hover,
        hoverinfo = "text",
        marker = list(size = 3, opacity = 0.5)) %>%
  layout(title = "Retraso en salida según distancia y tiempo de vuelo")


######################
# Evolución temporal #
######################

library(plotly)
library(dplyr)

# Crear una variable de tipo fecha a partir del año, mes y día
df_fecha <- df %>%
  mutate(FECHA = as.Date(paste(YEAR, MONTH, DAY, sep = "-")))

# Resumir el retraso medio diario en salida
df_diario <- df_fecha %>%
  group_by(FECHA) %>%
  summarise(RETRASO_MEDIO = mean(DEPARTURE_DELAY, na.rm = TRUE))

# Gráfico de líneas interactivo: evolución del retraso medio diario
plot_ly(df_diario, x = ~FECHA, y = ~RETRASO_MEDIO,
        type = "scatter", mode = "lines",
        line = list(color = "darkred"),
        name = "Retraso medio diario") %>%
  layout(title = "Evolución del retraso medio en salida",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "Retraso medio (min)"))

# Otra opción: añadir puntos sobre la línea
plot_ly(df_diario, x = ~FECHA, y = ~RETRASO_MEDIO,
        type = "scatter", mode = "lines+markers",
        line = list(color = "steelblue"),
        marker = list(size = 4),
        name = "Retraso diario") %>%
  layout(title = "Retraso medio diario en vuelos",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "Minutos de retraso"))

# Si se quiere comparar varias aerolíneas a lo largo del tiempo
df_aerolineas <- df_fecha %>%
  group_by(FECHA, AIRLINE) %>%
  summarise(RETRASO_MEDIO = mean(DEPARTURE_DELAY, na.rm = TRUE), .groups = "drop")

# Gráfico con líneas separadas por aerolínea
plot_ly(df_aerolineas, x = ~FECHA, y = ~RETRASO_MEDIO, color = ~AIRLINE,
        type = "scatter", mode = "lines") %>%
  layout(title = "Retraso medio diario por aerolínea",
         xaxis = list(title = "Fecha"),
         yaxis = list(title = "Minutos de retraso"))


# Segmentador eje X

# Gráfico con filtro interactivo de fechas (rangeselector y rangeselector slider)
plot_ly(df_diario, x = ~FECHA, y = ~RETRASO_MEDIO,
        type = "scatter", mode = "lines",
        line = list(color = "darkblue")) %>%
  layout(
    title = "Retraso medio diario con filtro de fechas",
    xaxis = list(
      title = "Fecha",
      rangeselector = list(
        buttons = list(
          list(count = 7, label = "1 semana", step = "day", stepmode = "backward"),
          list(count = 1, label = "1 mes", step = "month", stepmode = "backward"),
          list(count = 3, label = "3 meses", step = "month", stepmode = "backward"),
          list(step = "all", label = "Todo")
        )
      ),
      rangeslider = list(visible = TRUE),  # Este es el "segmentador" visual
      type = "date"
    ),
    yaxis = list(title = "Retraso medio (min)")
  )

# Añadir medias móviles
# install.packages("zoo")
library(zoo)

df_diario <- df_diario %>%
  mutate(
    MEDIA_MOVIL_7 = rollmean(RETRASO_MEDIO, k = 7, fill = NA, align = "right"),
    MEDIA_MOVIL_30 = rollmean(RETRASO_MEDIO, k = 30, fill = NA, align = "right")
  )

# Gráfico con evolución y medias móviles
plot_ly(df_diario, x = ~FECHA) %>%
  add_lines(y = ~RETRASO_MEDIO, name = "Retraso diario", line = list(color = "lightgray")) %>%
  add_lines(y = ~MEDIA_MOVIL_7, name = "Media móvil 7 días", line = list(color = "blue")) %>%
  add_lines(y = ~MEDIA_MOVIL_30, name = "Media móvil 30 días", line = list(color = "red", dash = "dash")) %>%
  layout(
    title = "Evolución del retraso medio con medias móviles",
    xaxis = list(title = "Fecha"),
    yaxis = list(title = "Retraso medio (min)")
  )


##########################
# Exportación automática #
##########################

library(plotly)
library(dplyr)

# Agrupar y contar vuelos por aerolínea
df_bar <- df %>%
  count(AIRLINE) %>%
  mutate(AIRLINE = factor(AIRLINE, 
                          levels = AIRLINE[order(n, decreasing = TRUE)]))

# Crear gráfico interactivo
grafico_barras <- plot_ly(df_bar,
                          x = ~AIRLINE,
                          y = ~n,
                          type = "bar",
                          text = ~paste("Vuelos:", n),
                          hoverinfo = "text",
                          color = ~AIRLINE,
                          showlegend = FALSE) %>%
  layout(title = "Número de vuelos por aerolínea",
         xaxis = list(title = "Aerolínea"),
         yaxis = list(title = "Cantidad de vuelos"))

# Guardar en HTML
htmlwidgets::saveWidget(grafico_barras, "vuelos_por_aerolinea.html",selfcontained = TRUE)


# Histograma básico del tiempo de vuelo
grafico_hist <- plot_ly(df, x = ~AIR_TIME, type = "histogram",
                        marker = list(color = "orange", line = list(color = "white", width = 1)),
                        nbinsx = 50) %>%
  layout(title = "Distribución del tiempo de vuelo",
         xaxis = list(title = "Duración (min)"),
         yaxis = list(title = "Frecuencia"))

# Exportar como HTML
htmlwidgets::saveWidget(grafico_hist, "histograma_airtime.html",selfcontained = TRUE)


# Bucle Automático

library(plotly)
library(htmlwidgets)

# Función para crear y exportar histogramas interactivos
exportar_histogramas <- function(data, variables, carpeta = ".") {
  for (var in variables) {
    # Crear el gráfico con programación no estándar (evaluar nombre de variable)
    grafico <- plot_ly(data, x = data[[var]], type = "histogram",
                       nbinsx = 50,
                       marker = list(color = "steelblue", line = list(color = "white", width = 1))) %>%
      layout(title = paste("Histograma de", var),
             xaxis = list(title = var),
             yaxis = list(title = "Frecuencia"))
    
    # Construir nombre de archivo
    nombre_archivo <- file.path(carpeta, paste0("histograma_", var, ".html"))
    
    # Guardar gráfico como HTML
    htmlwidgets::saveWidget(grafico, file = nombre_archivo, selfcontained = TRUE)
  }
}

# Variables que quieres graficar
vars_para_exportar <- c("DEPARTURE_DELAY", "ARRIVAL_DELAY", "AIR_TIME", "DISTANCE")

# Llamada a la función
exportar_histogramas(df, vars_para_exportar)



