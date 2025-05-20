# install.packages(c("ggplot2", "plotly", "patchwork"))

# Cargar datos
ruta <- "C:/Users/linkedin/Dropbox/LinkedIn/"

df <- read.csv(paste0(ruta,"titles.csv"), stringsAsFactors = FALSE)

# Cargar paquetes necesarios
library(ggplot2)
library(plotly)
library(gridExtra)


# --------------------------------------------------
# 1. Evolución del catálogo por año


conteo1 <- as.data.frame(table(df$release_year))
names(conteo1) <- c("year", "count")
conteo1$year <- as.integer(as.character(conteo1$year))
conteo1 <- conteo1[order(conteo1$year), ]

g1 <- ggplot(conteo1, aes(x = year, y = count)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(color = "darkred") +
  labs(title = "Títulos añadidos por año", x = "Año", y = "Cantidad") +
  theme_minimal()
g1

# --------------------------------------------------
# 2. Comparación de películas y series

conteo2 <- as.data.frame(table(df$type))
names(conteo2) <- c("type", "count")

g2 <- ggplot(conteo2, aes(x = type, y = count, fill = type)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  labs(title = "Películas vs Series", x = "", y = "Cantidad") +
  theme_minimal()
g2
# --------------------------------------------------
# 3. Evolución temporal por tipo


df3 <- df[!is.na(df$release_year), ]
conteo3 <- as.data.frame(table(df3$release_year, df3$type))
names(conteo3) <- c("year", "type", "count")
conteo3$year <- as.integer(as.character(conteo3$year))

g3 <- ggplot(conteo3, aes(x = year, y = count, color = type)) +
  geom_line() +
  labs(title = "Evolución por tipo de contenido", x = "Año", y = "Cantidad") +
  theme_minimal()

g3

# --------------------------------------------------
# 4. Top 10 países productores

df$country <- sapply(strsplit(df$country, ","), `[`, 1)
top_paises <- sort(table(df$country), decreasing = TRUE)[1:10]
df4 <- data.frame(pais = names(top_paises), cantidad = as.integer(top_paises))

g4 <- ggplot(df4, aes(x = reorder(pais, cantidad), y = cantidad)) +
  geom_col(fill = "tomato") +
  coord_flip() +
  labs(title = "Top 10 países productores", x = "", y = "Cantidad") +
  theme_minimal()
g4
# --------------------------------------------------
# 5. Duración de películas

df_movies <- df[df$type == "Movie", ]
df_movies$duration_num <- as.numeric(gsub(" min", "", df_movies$duration))

g5 <- ggplot(df_movies, aes(x = duration_num)) +
  geom_histogram(binwidth = 10, fill = "lightblue", color = "white") +
  labs(title = "Duración de películas", x = "Minutos", y = "Frecuencia") +
  theme_minimal()

g5
# --------------------------------------------------
# 6. Frecuencia de categorías (primer género)

df$genero <- sapply(strsplit(df$listed_in, ","), `[`, 1)
conteo6 <- sort(table(df$genero), decreasing = TRUE)[1:10]
df6 <- data.frame(genero = names(conteo6), cantidad = as.integer(conteo6))

g6 <- ggplot(df6, aes(x = reorder(genero, cantidad), y = cantidad)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(title = "Categorías más comunes", x = "", y = "Frecuencia") +
  theme_minimal()

g6

# --------------------------------------------------
# 7. Clasificación por edad a lo largo del tiempo

conteo7 <- as.data.frame(table(df$release_year, df$rating))
names(conteo7) <- c("year", "rating", "count")
conteo7 <- conteo7[conteo7$year != "", ]
conteo7$year <- as.integer(as.character(conteo7$year))

g7 <- ggplot(conteo7, aes(x = year, y = count, fill = rating)) +
  geom_area(alpha = 0.8, position = "fill") +
  labs(title = "Clasificaciones por edad a lo largo del tiempo", x = "Año", y = "Cantidad") +
  theme_minimal()
g7
# --------------------------------------------------
# 8. Boxplot duración por género

df_movies$genero <- sapply(strsplit(df_movies$listed_in, ","), `[`, 1)
g8 <- ggplot(df_movies, aes(x = genero, y = duration_num)) +
  geom_boxplot(fill = "orange") +
  coord_flip() +
  labs(title = "Duración por género", x = "", y = "Minutos") +
  theme_minimal()

g8
# --------------------------------------------------
# 9. Visualización interactiva (plotly)

plot_interactivo <- plot_ly(df_movies, x = ~duration_num, type = "histogram",
                            nbinsx = 30, marker = list(color = "purple"))

plot_interactivo
# --------------------------------------------------
# 10. Panel comparativo (nueva librería)

library(patchwork)
panel <- (g1 | g2) / (g3 | g4)

# Mostrar el panel
print(panel)

