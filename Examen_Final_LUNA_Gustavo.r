
# LUNA, Gustavo David

"""Haciendo uso del lenguaje R y el dataset generado en el TP N° 3, realice lo siguiente:
1.	Importe y lectura del csv.
2.	Cree funciones para ayudarle a optimizar su trabajo.
3.	Filtrar los teléfonos que tienen soporte para NFC.
4.	Calcular el precio mínimo, promedio y máximo de los teléfonos con soporte para NFC.
5.	Graficar el TOP 10 de celulares con mayor precio.
6.	Graficar el TOP 5 de celulares con menor precio
7.	Graficar la distribución de los tipos de memorias.
8.	Graficar el TOP 5 de baterías más utilizadas.
9.	Realiza un análisis descriptivo de la columna ‘precio’ y compara los resultados con los del punto 4.
10.	Realiza un análisis numérico con frecuencias de la columna ‘ram’.
11.	Muestre una tabla con las correlaciones entre 'precio', 'rating', 'ram' y 'almacenamiento'.
12.	Utilizando el método de K-means para clustering, grafique las características ‘precio’ y ‘rating’.
13.	Haciendo uso del modelo de Regresión Lineal, determine las variables relevantes para poder predecir el ‘rating’.
14.	 Cree una tabla de contingencia que muestre la cantidad de celulares según su tamaño de 'ram' y tipo de 'sim'.

## Setup
"""

library(tidyverse)

library(RColorBrewer)

library(ggplot2)

install.packages("factoextra")

install.packages("Metrics")

install.packages("caret")

install.packages("descr")

library(factoextra)
library(cluster)
library(caret)
library(dplyr)
library(Metrics)
library(descr)

separador<- "=============================================================="

"""##  1.Importe y lectura del csv."""

##########################################################################################
## 1.	Importe y lectura del csv.
##########################################################################################

#ruta<-"/content/celulares_PySpark.csv"
ruta <- "https://raw.githubusercontent.com/gustavodluna/TP_FINAL_PROG_CS_DATOS/master/celulares_PySpark.csv"
df<-read.csv(ruta)
#df

"""## 2.Cree funciones para ayudarle a optimizar su trabajo."""

##########################################################################################
## 2.	Cree funciones para ayudarle a optimizar su trabajo.
##########################################################################################

valores_unicos <- function(df, columna) {
  # Extraer la columna
  columna_datos <- df[[columna]]

  # Obtener los valores únicos y contarlos
  conteo_valores <- table(columna_datos)

  # Mostrar el conteo de valores
  print(conteo_valores)
}

# Definir la función para cambiar el nombre de una columna
cambiar_nombre_columna <- function(dataframe, nombre_columna_antiguo, nombre_columna_nuevo) {
  if (nombre_columna_antiguo %in% names(dataframe)) {
    names(dataframe)[names(dataframe) == nombre_columna_antiguo] <- nombre_columna_nuevo
    return(dataframe)
  } else {
    print("El nombre de la columna antiguo no existe en el dataframe.")
    return(NULL)
  }
}

"""## 3.Filtrar los teléfonos que tienen soporte para NFC."""

##########################################################################################
## 3.	Filtrar los teléfonos que tienen soporte para NFC.
##########################################################################################

# Llamamos a la función para cambiar el nombre de la columna
df <- cambiar_nombre_columna(df, "X5G", "n5G")

# Convertir la columna 'batería' a tipo string
df$bateria <- as.character(df$bateria)

# Filtrar los teléfonos
df_2 <- subset(df,NFC == "true")
#df_2

"""## 4.Calcular el precio mínimo, promedio y máximo de los teléfonos con soporte para NFC."""

##########################################################################################
## 4.	Calcular el precio mínimo, promedio y máximo de los teléfonos con soporte para NFC.
##########################################################################################

# Calcular el precio promedio de los teléfonos con soporte para NFC:

price_min<- min(df_2$precio)
price_min_round<-round(price_min,digits = 2)
avg_price_nfc <- mean(df_2$precio)
avg_price_nfc_round<-round(avg_price_nfc,digits = 2)
price_max<- max(df_2$precio)
price_max_round<-round(price_max,digits = 2)

paste(separador)
paste("El precio mínimo de los celulares con soporte para NFC es: ",price_min_round," USD")
paste("El precio promedio de los celulares con soporte para NFC es: ",avg_price_nfc_round," USD")
paste("El precio máximo de los celulares con soporte para NFC es: ",price_min_round," USD")
paste(separador)

"""## 5.Graficar el TOP 10 de celulares con mayor precio."""

##########################################################################################
## 5.	Graficar el TOP 10 de celulares con mayor precio.
##########################################################################################

# Filtrar los celulares que tienen NFC igual a true
celulares_con_nfc <- df %>% filter(NFC == "true")

# Obtener el top 10 de celulares con mejor rating y mayor precio, teniendo en cuenta NFC
top_10_celulares_max_precio <- celulares_con_nfc %>%
  arrange(desc(precio)) %>%
  distinct(precio, .keep_all = "true") %>%
  head(10)
options(repr.plot.width = 18, repr.plot.height = 10)
# Crear el gráfico de barras con ggplot2
ggplot(top_10_celulares_max_precio, aes(x = reorder(modelo,-precio), y = precio,fill = modelo)) +
  geom_bar(stat = "identity",width = 0.8) +
  geom_text(aes(label = precio), vjust = -0.5,size=3) +  # Añadir etiquetas de los valores
  labs(title = "Top 10 celulares con mayor precio",
       x = "Modelo del Celular",
       y = "Precio") +
  theme(
        panel.background = element_rect(fill = "white"),  # Cambiar el fondo a blanco
        plot.background = element_rect(fill = "white"),   # Cambiar el fondo del gráfico a blanco
        axis.text.x = element_text(angle = 45, hjust = 1,size=8),
        axis.title.y = element_blank(),  # Eliminar el título del eje y
        axis.text.y = element_blank(),   # Eliminar las etiquetas del eje y
        axis.ticks.y = element_blank(),  # Eliminar las marcas del eje y
        axis.line.y = element_blank(),   # Eliminar la línea del eje y
        axis.title.x = element_blank(),  # Eliminar el título del eje x
        axis.ticks.x = element_blank(),  # Eliminar las marcas del eje x
        axis.line.x = element_blank(),    # Eliminar la línea del eje x
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)  # Centrar el título
       )  +
  scale_fill_brewer(palette = "Set3")  # Usar una paleta predefinida de RColorBrewer

"""## 6.Graficar el TOP 5 de celulares con menor precio."""

##########################################################################################
## 6.	Graficar el TOP 5 de celulares con menor precio
##########################################################################################

# Obtener el top 10 de celulares con menor precio, teniendo en cuenta NFC
top_10_celulares_min_precio <- df_2 %>%
  arrange(desc(precio)) %>%
  #distinct(precio, .keep_all = "true") %>%
  tail(5)
options(repr.plot.width = 18, repr.plot.height = 8)
# Crear el gráfico de barras con ggplot2 usando una paleta de RColorBrewer
ggplot(top_10_celulares_min_precio, aes(x = reorder(modelo, -precio), y = precio, fill = modelo)) +
  geom_bar(stat = "identity", width = 0.6) +
  geom_text(aes(label = precio), vjust = -0.5, size = 5) +
  labs(title = "Top 5 Celulares con menor precio",
       x = "Modelo del Celular",
       y = "Precio") +
  theme(
        panel.background = element_rect(fill = "white"),  # Cambiar el fondo a blanco
        plot.background = element_rect(fill = "white"),   # Cambiar el fondo del gráfico a blanco
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5)  # Centrar el título
       ) +
  scale_fill_brewer(palette = "Set3")  # Usar una paleta predefinida de RColorBrewer

"""## 7.Graficar la distribución de los tipos de memorias."""

#######################################################################
## 7.	Graficar la distribución de los tipos de memorias.###############
#######################################################################

# Agrupar y contar los datos por la columna 'memoria'
memoria_counts <- df %>%
  group_by(memoria) %>%
  summarise(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)  # Calcular el porcentaje para cada categoría

options(repr.plot.width = 8, repr.plot.height = 8)

# Crear el gráfico de pie con ggplot2
ggplot(memoria_counts, aes(x = "", y = count, fill = memoria)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")),
            position = position_stack(vjust = 0.5)) +  # Añadir etiquetas con el porcentaje
  labs(title = "Distribución de Tipos de Memoria",
       x = NULL,
       y = NULL) +
  theme_void() +  # Quitar los ejes y el fondo del gráfico
  theme(legend.title = element_blank(),  # Quitar el título de la leyenda
        plot.title = element_text(hjust = 0.5))  # Centrar el título del gráfico

"""## 8.Graficar el TOP 5 de baterías más utilizadas."""

###################################################################################
############## 8.	Graficar el TOP 5 de baterías más utilizadas.####################
###################################################################################

# Calcular las frecuencias y filtrar el top 5
top_5_baterias <- df %>%
  count(bateria) %>%
  arrange(desc(n)) %>%
  top_n(5, n)

# Filtrar el dataframe original para incluir solo las filas con los valores de batería en el top 5
df_top_5 <- df %>%
  filter(bateria %in% top_5_baterias$bateria)

# Ordenar el factor de la columna 'batería' de mayor a menor
df_top_5$bateria <- factor(df_top_5$bateria, levels = top_5_baterias$bateria[order(-top_5_baterias$n)])

# Configuración del tamaño de la figura
options(repr.plot.width = 10, repr.plot.height = 6)

# Crear el gráfico de barras con ggplot2
ggplot(df_top_5, aes(x = bateria, fill = bateria)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) +
  labs(title = "Top 5 baterías más comunes", x = "batería", y = "") +
  #theme_minimal(base_size = 15) +  # Cambiar el tema a minimal y establecer tamaño base de fuente
  theme(
    panel.background = element_rect(fill = "white"),  # Cambiar el fondo a blanco
    plot.background = element_rect(fill = "white"),   # Cambiar el fondo del gráfico a blanco
    axis.text.x = element_text(angle = 0, hjust = 1, size = 10),
    axis.title.y = element_blank(),  # Eliminar el título del eje y
    axis.text.y = element_blank(),   # Eliminar las etiquetas del eje y
    axis.ticks.y = element_blank(),  # Eliminar las marcas del eje y
    axis.line.y = element_blank(),   # Eliminar la línea del eje y
    axis.title.x = element_blank(),  # Eliminar el título del eje x
    axis.ticks.x = element_blank(),  # Eliminar las marcas del eje x
    axis.line.x = element_blank(),   # Eliminar la línea del eje x
    legend.position = "none",        # Eliminar la leyenda
    plot.title = element_text(hjust = 0.5)  # Centrar el título
  ) +
  scale_fill_brewer(palette = "Set3")  # Usar la paleta Set3 de RColorBrewer

"""##  9.Realiza un análisis descriptivo de la columna ‘precio’ y compara los resultados con los del punto 4."""

##########################################################################################################
## 9.	Realiza un análisis descriptivo de la columna ‘precio’ y compara los resultados con los del punto 4.
##########################################################################################################

paste(separador)
# Descriptivos específicos para una columna:
summary(df_2$precio)
paste(separador)
paste("Los resultados coinciden con los del punto 4.")
paste(separador)

"""##  10.Realiza un análisis numérico con frecuencias de la columna ‘ram’."""

##########################################################################################
######### 10.	Realiza un análisis numérico con frecuencias de la columna ‘ram’.############
##########################################################################################

freq(df_2$ram, plot = FALSE)

"""## 11.Muestre una tabla con las correlaciones entre 'precio', 'rating', 'ram' y 'almacenamiento'."""

#########################################################################################################
##### 11.	Muestre una tabla con las correlaciones entre 'precio', 'rating', 'ram' y 'almacenamiento'.#####
##########################################################################################################

# Uso de correlaciones
cor_matrix <- cor(df_2 %>% select(precio,rating, ram,almacenamiento))
cor_matrix

"""## 12.Utilizando el método de K-means para clustering, grafique las características **‘precio’** y ‘rating’."""

################################################################################################################
##### 12. Utilizando el método de K-means para clustering, grafique las características ‘precio’ y ‘rating’.#####
#################################################################################################################

# Aplicar un gráfico de clústeres
# Selección de características numéricas para el clúster
data_cluster <- df_2 %>% select(precio,rating)

# Escalado de datos
data_scaled <- scale(data_cluster)

# Aplicación del método de k-means para clustering
set.seed(123) # Para reproducibilidad
km_res <- kmeans(data_scaled, centers = 3, nstart = 25)

# Visualización de clústeres
fviz_cluster(km_res, data = data_scaled,
             ellipse.type = "convex",
             geom = "point",
             stand = FALSE,
             ggtheme = theme_minimal())

"""#  13.Haciendo uso del modelo de Regresión Lineal, determine las variables relevantes para poder predecir el 'rating'."""

#############################################################################################################################
##### 13. Haciendo uso del modelo de Regresión Lineal, determine las variables relevantes para poder predecir el 'rating'.#####
############################################################################################################################

# Selecciona las columnas relevantes y elimina filas con valores NA
df_regression <- df %>%
  select(precio, rating,bateria,n5G, NFC, ram, rom, carga_rapida,almacenamiento) %>%
  na.omit()

# Convertir variables categóricas a factores
df_regression <- df_regression %>%
  mutate(
   n5G = as.factor(n5G),
    NFC = as.factor(NFC),
    carga_rapida = as.factor(carga_rapida)
  )

# Dividir los datos en conjuntos de entrenamiento y prueba
set.seed(123)  # Para reproducibilidad

trainIndex <- createDataPartition(df_regression$rating, p = .8,
                                  list = FALSE,
                                  times = 1)
df_train <- df_regression[trainIndex, ]
df_test <- df_regression[-trainIndex, ]

# Ajustar el modelo de regresión lineal
modelo <- lm(rating ~ precio +n5G+ NFC + ram + rom+almacenamiento, data = df_train)

# Resumen del modelo
summary(modelo)

# Hacer predicciones en el conjunto de prueba
predicciones <- predict(modelo, newdata = df_test)

# Calcular las métricas de evaluación
# Cargar paquete Metrics
if (!require(Metrics)) install.packages("Metrics")
library(Metrics)

# Calcular el error cuadrático medio (MSE)
mse <- mse(df_test$rating, predicciones)

# Calcular el error absoluto medio (MAE)
mae <- mae(df_test$rating, predicciones)

# Calcular el R²
rss <- sum((predicciones - df_test$rating) ^ 2)
tss <- sum((df_test$rating - mean(df_test$rating)) ^ 2)
r2 <- 1 - rss/tss

# Imprimir las métricas
cat("MSE:", mse, "\n")
cat("MAE:", mae, "\n")
cat("R²:", r2, "\n")

# Crear un nuevo dataframe con los valores de las variables predictoras
nuevo_dato <- data.frame(
  precio = 208.97,
  n5G = "true",
  NFC = "true",
  ram = 6,
  rom = 128,
  almacenamiento = 1000
)

# Predecir el rating utilizando el modelo ajustado
prediccion_rating <- predict(modelo, newdata = nuevo_dato)

# Mostrar la predicción
round(prediccion_rating,0)

"""# 14.Cree una tabla de contingencia que muestre la cantidad de celulares segun su tamaño de **texto en negrita**'ram' y tipo de 'sim'."""

#############################################################################################################################
##### 14. Cree una tabla de contingencia que muestre la cantidad de celulares segun su tamaño de 'ram' y tipo de 'sim'.#####
############################################################################################################################

tabla_abs<-table(df_2$ram, df_2$sim)
tabla_abs

round(prop.table(tabla_abs), 2) #proporciones totales reducidas a 2 decimales