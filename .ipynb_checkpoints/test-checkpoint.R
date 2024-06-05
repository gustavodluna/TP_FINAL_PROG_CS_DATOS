2+2

1:100
print("hola")
a<- 1

vNUmeric<-c(1,2,3)
vCharacter<-c("a","b","c")
vLogic<-c(T,F,T)

df1<- cbind(vNUmeric,vCharacter,vLogic)
df1

df2<-data.frame(vNUmeric,vCharacter,vLogic)
df2
library(datasets)
?datasets

library(tidyverse)
ruta<-"G:/Mi unidad/00-LICENCIATURA DE DATOS/TERCER AÑO/PROG PARA CIENCIA DE DATOS/TPs/TP5_LUNA_Gustavo/celulares_PySpark.csv"
df<-read.csv(ruta)
df



glimpse(df)
       
# Mostrar un resumen estadístico:
summary(df)

# Filtrar los teléfonos que tienen una batería mayor a 4000 mAh y soporte para 5G:

filtered_df <- subset(df, bateria > 5000 & X5G == "true")
filtered_df

# Calcular el precio promedio de los teléfonos con soporte para NFC:
avg_price_nfc <- mean(df$precio[df$NFC == "true"])
avg_price_nfc
avg_price_nfc_round<-round(avg_price_nfc,digits = 2)
avg_price_nfc_round        

#Crear un gráfico de dispersión del precio frente al rating:

library(ggplot2)
ggplot(df, aes(x = rating, y = precio)) +
  geom_point() +
  labs(title = "Precio vs Rating", x = "Rating", y = "Precio")



#Gráfico de barras del número de teléfonos por sistema operativo (OS) con colores:


library(ggplot2)

ggplot(df, aes(x = os, fill = os)) +
  geom_bar() +
  labs(title = "Número de teléfonos por Sistema Operativo", x = "Sistema Operativo", y = "Frecuencia") +
  theme_minimal()

# Gráfico de dispersión del precio frente a la memoria RAM con colores según si tienen carga rápida:

ggplot(df, aes(x = ram, y = precio, color = carga_rapida)) +
  geom_point() +
  labs(title = "Precio vs RAM con colores por Carga Rápida", x = "RAM (GB)", y = "Precio (USD)") +
  theme_minimal()


# Histograma de los precios con colores por compatibilidad con NFC:

ggplot(df, aes(x = precio, fill = NFC)) +
  geom_histogram(binwidth = 50, position = "dodge") +
  labs(title = "Histograma de Precios con colores por NFC", x = "Precio (USD)", y = "Frecuencia") +
  theme_minimal()

# Análisis Numérico
# Frecuencias de tipos de procesadores:

table(df$procesador)

# Resumen descriptivo de las variables numéricas:
summary(df[c("precio", "rating", "bateria", "almacenamiento", "ram", "rom")])

# Correlaciones entre variables numéricas:
numeric_columns <- df[c("precio", "rating", "bateria", "almacenamiento", "ram", "rom")]
correlation_matrix <- cor(numeric_columns, use = "complete.obs")
print(correlation_matrix)

# Frecuencia de teléfonos con y sin NFC:
table(df$NFC)

# Media y desviación estándar del precio por sistema operativo:
aggregate(precio ~ os, data = df, FUN = function(x) c(mean = mean(x), sd = sd(x)))

