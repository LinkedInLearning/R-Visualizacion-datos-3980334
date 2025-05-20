ruta <-"C:/Users/linkedin/Dropbox/LinkedIn/"
df <- read.csv(paste0(ruta,"flights_small.csv"), sep=";")
df

#####################
# Combinar gráficos # 
#####################

# install.packages("gridExtra")

library(ggplot2)
library(gridExtra)

# Histograma del retraso en salida
g1 <- ggplot(df, aes(x = DEPARTURE_DELAY)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  labs(title = "Retraso en salida", x = "Minutos", y = "Frecuencia") +
  theme_minimal()

# Histograma del retraso en llegada
g2 <- ggplot(df, aes(x = ARRIVAL_DELAY)) +
  geom_histogram(binwidth = 10, fill = "darkorange", color = "white") +
  labs(title = "Retraso en llegada", x = "Minutos", y = "Frecuencia") +
  theme_minimal()

# Boxplot del tiempo de vuelo por aerolínea
g3 <- ggplot(df, aes(x = AIRLINE, y = AIR_TIME, fill = AIRLINE)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Tiempo de vuelo por aerolínea", x = "Aerolínea", y = "Minutos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Dispersión entre distancia y tiempo de vuelo
g4 <- ggplot(df, aes(x = DISTANCE, y = AIR_TIME)) +
  geom_point(alpha = 0.3, color = "darkgreen") +
  labs(title = "Distancia vs. Tiempo de vuelo", x = "Distancia (millas)", 
       y = "Tiempo de vuelo (min)") +
  theme_minimal()

# Ejemplo 0. Combinar todos los gráficos en un layout 2x2
grid.arrange(g1, g2, g3, g4, ncol = 2)

# Ejemplo 1: Disposición vertical
grid.arrange(g1, g2, g3, g4, ncol = 1)

# Ejemplo 2: Tres arriba, uno grande abajo
grid.arrange(
  arrangeGrob(g1, g2, g3, ncol = 3),
  g4,
  nrow = 2,
  heights = c(1, 1.2)
)

# Ejemplo 3: Dos filas de dos columnas
grid.arrange(
  arrangeGrob(g1, g2, ncol = 2),
  arrangeGrob(g3, g4, ncol = 2),
  nrow = 2
)

# Ejemplo 4: Uno encima, dos al medio, uno abajo
grid.arrange(
  g1,
  arrangeGrob(g2, g3, ncol = 2),
  g4,
  nrow = 3,
  heights = c(1, 1, 1.3)
)

# Ejemplo 5: Principal a la izquierda, tres en columna a la derecha
grid.arrange(
  g3,
  arrangeGrob(g1, g2, g4, ncol = 1),
  ncol = 2,
  widths = c(1.5, 1)
)
ggsave(filename = "matriz.jpg",
       height = 1080, width = 1920, units = "px")

#########
# Mapas #
#########

# Script 2: Creación de mapas en R con datos geoespaciales

#install.packages("maps")

library(ggplot2)
library(maps)
library(dplyr)

# Obtener un mapa base de EE.UU.
usa <- map_data("state")

# Crear un data frame de ejemplo con valores ficticios por estado
set.seed(123)
df_mapa <- data.frame(
  region = tolower(state.name),  # nombres en minúsculas como en map_data()
  retraso_medio = runif(50, min = 5, max = 60)  # valores aleatorios de retraso
)

# Unir los datos del mapa con los valores del data frame
mapa_completo <- left_join(usa, df_mapa, by = "region")

# Crear el mapa con ggplot2
ggplot(mapa_completo, aes(long, lat, group = group, fill = retraso_medio)) +
  geom_polygon(color = "white") +
  coord_fixed(1.3) +
  scale_fill_viridis_c(option = "C", name = "Retraso medio (min)") +
  labs(title = "Retraso medio por estado (ejemplo ficticio)") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())


# install.packages(c("sf","tmap"))

library(sf)
library(tmap)

# Cargar un shapefile de países desde la base de datos de tmap
data("World")

# Crear valores ficticios de retraso medio por país
set.seed(42)
World$delay <- runif(nrow(World), min = 10, max = 90)

# Mapa temático con tmap
tmap_mode("plot")  # modo estático
tm_shape(World) +
  tm_polygons("delay")

#############

# install.packages(c("rnaturalearth","rnaturalearthdata"))

library(rnaturalearth)
library(rnaturalearthdata)

# 1. Descargar países del mundo como objeto sf
mundo <- ne_countries(scale = "medium", returnclass = "sf")


ggplot(data = mundo) +
  geom_sf(fill = "lightblue", color = "white") +
  labs(title = "Mapa del mundo") +
  theme_minimal()
ggsave(filename = "mapa.jpg",
       height = 1080, width = 1920, units = "px")

# 2. Filtrar solo los países de Europa
europa <- subset(mundo, continent == "Africa")

ggplot(data = europa) +
  geom_sf(fill = "lightblue", color = "white") +
  labs(title = "Mapa de África") +
  theme_minimal()



##########
# igraph #
##########

# install.packages(c("igraph","ggraph","visNetwork"))

library(igraph)
library(ggraph)
library(visNetwork)

# Definir nodos (aeropuertos)
nombres_nodos <- c("JFK", "LAX", "ORD", "DFW", "ATL", "DEN")
nodos <- data.frame(name = nombres_nodos)

# Definir aristas (conexiones entre aeropuertos)
aristas <- data.frame(
  from = c("JFK", "JFK", "LAX", "ORD", "ORD", "ATL", "DEN"),
  to =   c("LAX", "ORD", "ATL", "DFW", "DEN", "DFW", "ATL"),
  vuelos = c(200, 180, 150, 220, 170, 160, 190)
)

# Crear el objeto de red
grafo <- graph_from_data_frame(d = aristas, vertices = nodos, directed = TRUE)

# Visualización básica con ggraph
ggraph(grafo, layout = "fr") +
  geom_edge_link(aes(width = vuelos), arrow = arrow(length = unit(4, 'mm')), 
                 end_cap = circle(3, 'mm')) +
  geom_node_point(size = 8, color = "steelblue") +
  geom_node_text(aes(label = name), repel = TRUE, size = 4, color = "black") +
  labs(title = "Red de vuelos entre aeropuertos (simulado)") +
  theme_void()

# Calcular centralidad (grado de entrada)
grado_entrada <- degree(grafo, mode = "in")
V(grafo)$grado <- grado_entrada

# Visualización con tamaño de nodo según grado
ggraph(grafo, layout = "fr") +
  geom_edge_link(alpha = 0.4) +
  geom_node_point(aes(size = grado), color = "darkred") +
  geom_node_text(aes(label = name), repel = TRUE, size = 4) +
  labs(title = "Centralidad de entrada en red de vuelos") +
  theme_void()




######## Interactiva ##########


# Definir nodos
nombres_nodos <- c("JFK", "LAX", "ORD", "DFW", "ATL", "DEN")
nodos <- data.frame(id = nombres_nodos, label = nombres_nodos)

# Definir aristas (conexiones con pesos)
aristas <- data.frame(
  from = c("JFK", "JFK", "LAX", "ORD", "ORD", "ATL", "DEN"),
  to   = c("LAX", "ORD", "ATL", "DFW", "DEN", "DFW", "ATL"),
  value = c(200, 180, 150, 220, 170, 160, 190),  # peso visual de las aristas
  title = paste("Vuelos:", c(200, 180, 150, 220, 170, 160, 190))  # texto al pasar el ratón
)

# Crear la red interactiva
visNetwork(nodes = nodos, edges = aristas, height = "500px", width = "100%") %>%
  visEdges(arrows = "to") %>%
  visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
  visLayout(randomSeed = 42) %>%
  visPhysics(stabilization = TRUE) %>%
  visLegend()


#############
# gganimate #
#############

# Script 4: Generación de gráficos animados en R con gganimate

# install.packages("gganimate")
# install.packages("gifski")
# install.packages("png")
# install.packages("av")

library(ggplot2)
library(gganimate)
library(av)

# Simular evolución de ventas mensuales por producto
productos <- c("Producto A", "Producto B", "Producto C", "Producto D")
meses <- paste0("2023-", sprintf("%02d", 1:8))

# Crear base de datos con ventas acumuladas por producto y mes
df <- expand.grid(producto = productos, mes = meses, stringsAsFactors = FALSE)
df$mes_n <- as.integer(sub("2023-", "", df$mes))  # para ordenar

# Simular ventas: crecimiento con algo de variación
df$ventas <- NA
for (prod in productos) {
  ventas_iniciales <- sample(500:1000, 1)
  crecimiento <- cumsum(round(rnorm(8, mean = 150, sd = 50)))
  df$ventas[df$producto == prod] <- ventas_iniciales + crecimiento
}

# Calcular ranking en cada mes
df <- df[order(df$mes_n, -df$ventas), ]
df$rank <- ave(-df$ventas, df$mes_n, FUN = rank)

df

# Gráfico de barras animado
p <- ggplot(df, aes(x = reorder(producto, -ventas), y = ventas, fill = producto)) +
  geom_col(width = 0.8) +
  coord_flip() +
  labs(title = "Ventas acumuladas por producto",
       subtitle = "Mes: {closest_state}",
       x = "", y = "Ventas") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  transition_states(mes_n, transition_length = 2, state_length = 1) +
  ease_aes("cubic-in-out")

# Exportar como vídeo MP4
animate(p, nframes = 100, fps = 10, width = 800, height = 500,
        renderer = av_renderer("ventas_bar_chart_race.mp4"))



##################
# Automatización #
##################

# install.packages("svglite")
library(ggplot2)

ruta <- "C:/Users/linkedin/Dropbox/LinkedIn/"
df <- read.csv(paste0(ruta,"flights_small.csv"), sep=";")
df

# Crear varios gráficos simples
g1 <- ggplot(df, aes(x = DEPARTURE_DELAY)) +
  geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
  labs(title = "Histograma de retraso en salida") +
  theme_minimal()

g2 <- ggplot(df, aes(x = DISTANCE, y = AIR_TIME)) +
  geom_point(alpha = 0.3, color = "darkgreen") +
  labs(title = "Distancia vs. tiempo de vuelo") +
  theme_minimal()

g3 <- ggplot(df, aes(x = factor(MONTH), fill = AIRLINE)) +
  geom_bar(position = "dodge") +
  labs(title = "Número de vuelos por mes y aerolínea") +
  theme_minimal()

# Función para exportar cualquier gráfico en múltiples formatos
exportar_grafico <- function(grafico, nombre_base, carpeta = ".", ancho = 8, 
                             alto = 6, dpi = 300) {
  ggsave(filename = file.path(carpeta, paste0(nombre_base, ".png")),
         plot = grafico, width = ancho, height = alto, dpi = dpi)
  ggsave(filename = file.path(carpeta, paste0(nombre_base, ".pdf")),
         plot = grafico, width = ancho, height = alto)
  ggsave(filename = file.path(carpeta, paste0(nombre_base, ".svg")),
         plot = grafico, width = ancho, height = alto)
}

# Exportar los gráficos de ejemplo
exportar_grafico(g1, "grafico_histograma")
exportar_grafico(g2, "grafico_dispersion")
exportar_grafico(g3, "grafico_barras")



## Ejemplo 2

# Obtener columnas numéricas del dataframe

df <- iris
columnas_numericas <- names(df)[sapply(df, is.numeric)]

# Crear combinaciones únicas de pares (sin repetir ni invertir el orden)
combinaciones <- combn(columnas_numericas, 2, simplify = FALSE)

# Generar y exportar cada gráfico de dispersión
for (par in combinaciones) {
  x_var <- par[1]
  y_var <- par[2]
  
  p <- ggplot(df, aes_string(x = x_var, y = y_var)) +
    geom_point(alpha = 0.3, color = "steelblue") +
    labs(title = paste("Dispersión:", x_var, "vs", y_var),
         x = x_var, y = y_var) +
    theme_minimal()
  
  nombre_archivo <- paste0("scatter_", x_var, "_vs_", y_var)
  exportar_grafico(p, nombre_archivo)
}



