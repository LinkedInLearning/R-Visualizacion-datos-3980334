ruta <- "C:/Users/linkedin/Dropbox/LinkedIn/"
df <- read.csv(paste0(ruta,"flights_small.csv"), sep=";")
head(df)

###################
# Gráficos base R #
###################


# Histograma del retraso en salida
hist(df$DEPARTURE_DELAY)

# Histograma del tiempo de vuelo
hist(df$AIR_TIME)

# Diagrama de dispersión entre retraso en salida y retraso en llegada
plot(df$DEPARTURE_DELAY, df$ARRIVAL_DELAY)

# Diagrama de dispersión entre distancia y tiempo de vuelo
plot(df$DISTANCE, df$AIR_TIME)

# Gráfico de barras con el número de vuelos por aerolínea
barplot(table(df$AIRLINE))

# Gráfico de barras por mes
barplot(table(df$MONTH))

# Boxplot del retraso en llegada por día de la semana
boxplot(ARRIVAL_DELAY ~ DAY_OF_WEEK, data = df)

# Boxplot del tiempo de vuelo según el mes
boxplot(AIR_TIME ~ MONTH, data = df)

# Gráfico de sectores con proporción de vuelos por aerolínea
aerolineas <- sort(table(df$AIRLINE), decreasing = TRUE)
pie(aerolineas)


######################
# Modificar Gráficos #
######################


# Histograma del tiempo de vuelo
# Útil para observar si hay mucha variabilidad
hist(df$AIR_TIME,  breaks = 100,
     main = "Duración de los vuelos",
     xlab = "Tiempo de vuelo (min)",
     col = "lightgreen",
     border = "white")



# Diagrama de dispersión entre retraso en salida y retraso en llegada
# Para ver si hay relación entre ambos
plot(df$DEPARTURE_DELAY, df$ARRIVAL_DELAY,
     main = "Relación entre retraso en salida y llegada",
     xlab = "Retraso en salida (min)",
     ylab = "Retraso en llegada (min)",
     pch = 3,
     )  



# Gráfico de barras con el número de vuelos por aerolínea
# Requiere agrupar con table()
tabla <- table(df$AIRLINE)
#tabla <- sort(table(df$AIRLINE), decreasing = TRUE)
barplot(tabla,
        las = 2,
        col = "orange",
        main = "Número de vuelos por aerolínea")

# Boxplot del retraso en llegada
boxplot(x =df$ARRIVAL_DELAY, 
        horizontal = FALSE,
        main = "Retraso en llegada",
        ylab = "Retraso en llegada (min)",
        col = "lightpink")

# Boxplot del retraso en llegada por día de la semana
# Permite ver si hay días más conflictivos
boxplot(ARRIVAL_DELAY ~ DAY_OF_WEEK, data = df,
        main = "Retraso en llegada por día de la semana",
        xlab = "Día de la semana (1 = lunes)",
        ylab = "Retraso en llegada (min)",
        col = "lightpink")


# Tabla de frecuencias de aerolíneas
tabla <- table(df$AIRLINE)
tabla <- sort(table(df$AIRLINE), decreasing = TRUE)


# Calculamos los porcentajes
porcentajes <- round(100 * tabla / sum(tabla), 1)

# Creamos las etiquetas con nombre y porcentaje
etiquetas <- paste(names(tabla), ": ", 
                   porcentajes, "%", sep = "")

# Gráfico de sectores
pie(tabla,
    labels = etiquetas,
    col = rainbow(length(tabla)),
    clockwise = TRUE,
    main = "Proporción de vuelos por aerolínea")

