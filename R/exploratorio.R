# Cargamos el fichero de datos
load("data_mammographic_mass.RData")

# Cargamos las librerías necesarias
library(ggplot2) # Para los gráficos
library(reshape2)  # Para moldear los datos para el mapa de calor

## Análisis descriptivo

# Total de NA's por variable 
colSums(is.na(data))

# Total de registros con algún NA
length(which(rowSums(is.na(data))!=0))

# Eliminamos los NA's
data<- na.omit(data[data$BIRADS <= 6,]) 

# Relación BIRADS-Type
t(as.matrix(table(data$BIRADS, data$Type))) # Relación BIRADS-Type

# Se elimina la incongruencia BIRADS = 6 y Type = 0
data<- data[-which(data$BIRADS==6 & data$Type==0),]

# Comprobamos la cantidad de registros en cada clase
length(which(data$Type == 0))
length(which(data$Type == 1))

# Eliminamos BIRADS para el análisis gráfico
data<- data[,2:6]

# Tranformamos a factor la varaibles categóricas y respuesta
data[,2:5]<- lapply(data[,2:5], as.factor)

# Guardamos el conjunto de datos para ser utilizado a posteriori
save(data, file = "data_models.RData")

## Gráficas para la sección 5.1

# Histograma de la edad en función de tipo de tumor
ggplot(data, aes(x =data$Age, fill = data$Type)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.7) +
  scale_fill_manual(values = c("0" = "lightgreen", "1" = "lightblue"),
                    name = "Tipo de Tumor",
                    labels = c("0" = "Benigno", "1" = "Maligno")) +
  labs(title = "Histograma del Tipo de Tumor según la Edad",
       x = "Edad",
       y = "Frecuencia") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
      axis.title.y = element_text(angle = 90, vjust = 0.5))

# Histogramas de las variables categóricas en función del tipo de tumor

ggplot(data, aes(x = Type, fill = Margin)) +
  geom_bar(position = "dodge", color = "white", alpha = 0.7) +
  labs(title = "Distribución del margen del tumor según tipo de tumor",
       x = "Tipo de tumor",
       y = "",
       fill = "Tipo de margen") +
  scale_x_discrete(labels = c("Benigno", "Maligno")) +
  scale_fill_manual(values = viridisLite::viridis(5),
                    labels = c("Circunscrito", "Microlobulado"," Oscurecido", "Mal definido", "Espiculado")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Ajustar la posición del título
        axis.title.y = element_text(angle = 90, vjust = 0.5),
        legend.text = element_text(size = 11.5))

ggplot(data, aes(x = data$Type, fill = data$Density)) +
  geom_bar(position = "dodge", color = "white", alpha = 0.7) +
  labs(title = "Distribución de la densidad del tumor según tipo de tumor",
       x = "Tipo de tumor",
       y = "Frecuencia",
       fill = "Tipo de densidad") +
  scale_x_discrete(labels = c("Benigno", "Maligno")) +
  scale_fill_manual(values = viridisLite::viridis(4),
                    labels = c("Alta", "Iso"," Baja", "Contiene grasa")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Ajustar la posición del título
        axis.title.y = element_text(angle = 90, vjust = 0.5),
        legend.text = element_text(size = 11.5))

ggplot(data, aes(x = data$Type, fill = data$Shape)) +
  geom_bar(position = "dodge", color = "white", alpha = 0.7) +
  labs(title = "Distribución de la forma del tumor según tipo de tumor",
       x = "Tipo de tumor",
       y = "Frecuencia",
       fill = "Tipo de forma") +
  scale_x_discrete(labels = c("Benigno", "Maligno")) +
  scale_fill_manual(values = viridisLite::viridis(4),
                    labels = c("Redondeada", "Oval"," Lobular", "Irregular")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),  # Ajustar la posición del título
        axis.title.y = element_text(angle = 90, vjust = 0.5),
        legend.text = element_text(size = 11.5))

## Porcentajes para la sección 5.1, gráficas anteriores

# Porcentaje tumores benignos margen
length(which(data$Margin==1&data$Type==0))/length(which(data$Type==0))

# Porcentaje tumores malignos margen
length(which((data$Margin==4&data$Type==1)|(data$Margin==3&data$Type==1)))/length(which(data$Type==1))
	   
# Porcentaje tumores benignos densidad 4
length(which(data$Density==3&data$Type==0))/length(which(data$Type==0))	
	   
# Porcentaje tumores malignos densidad 4
length(which(data$Density==3&data$Type==1))/length(which(data$Type==1))
	   
# Porcentaje tumores malignos forma 4
length(which(data$Shape==4&data$Type==1))/length(which(data$Type==1))

## Mapa de calor entre variables

# Creamos una función para calcular las correlaciones entre las distintas variables
# Entre variables categóricas se usa el coeficiente de Camer
# Entre una variable categórica y una numérica se usa el coeficiente eta cuadrado de ANOVA
calculate_association <- function(df, var1, var2) {
  if (is.factor(df[[var1]]) && is.factor(df[[var2]])) {
    # Coeficiente de Cramér para cat-cat
    # Crear una tabla de frecuencias
    frecuencias <- table(df[,var1], df[,var2])
	# Realizamos la prueba de chi-cuadrado
	chi_cuadrado <- chisq.test(frecuencias)
	# Calculamos el número total de observaciones en la tabla de contingencia
	n <- sum(frecuencias)
	# Determinamos la dimensión menor entre el número de filas y columnas de la tabla de contingencia
	min_dim <- min(nrow(frecuencias), ncol(frecuencias))
	# Cálculo del coeficiente
	cramer_v <- sqrt(chi_cuadrado$statistic / (n * (min_dim - 1)))
    return(cramer_v)
  } else if (is.factor(df[[var1]]) && is.numeric(df[[var2]]) || 
             is.numeric(df[[var1]]) && is.factor(df[[var2]])) {
    # ANOVA para cat-num
    if(is.numeric(df[[var1]])) {
      # Realizar ANOVA
      modelo_anova <- aov(df[,var1] ~ df[,var2], data = df)
      summary<- summary(modelo_anova)
      # La suma de cuadrados del grupo (variable independiente)
      suma_cuadrados_grupo <- summary[[1]][1, "Sum Sq"]
      # Suma total de cuadrados (Total)
      suma_cuadrados_total <- sum(summary[[1]][, "Sum Sq"])
      # Calcular Eta Cuadrado
      eta_cuadrado <- suma_cuadrados_grupo / suma_cuadrados_total
      # Imprimir el valor de Eta Cuadrado
      print(eta_cuadrado)
    } else {
      # Realizar ANOVA
      modelo_anova <- aov(df[,var2] ~ df[,var1], data = df)
      summary<- summary(modelo_anova)
      # La suma de cuadrados del grupo (variable independiente)
      suma_cuadrados_grupo <- summary[[1]][1, "Sum Sq"]
      # Suma total de cuadrados (Total)
      suma_cuadrados_total <- sum(summary[[1]][, "Sum Sq"])
      # Calcular Eta Cuadrado
      eta_cuadrado <- suma_cuadrados_grupo / suma_cuadrados_total
      # Imprimir el valor de Eta Cuadrado
      print(eta_cuadrado)
    }
  } else {return(NA)  }
}

# Guardamos los nombres de las variables
variables <- colnames(data)

# Creamos una matriz vacía para las correlaciones
cor_matrix <- matrix(NA, nrow = length(variables), ncol = length(variables))
rownames(cor_matrix) <- colnames(cor_matrix) <- variables

# Calculamos las correlacioens para cada par de variblaes
for (i in 1:length(variables)) {
  for (j in 1:length(variables)) {
    if (i != j) {  # Evitar calcular la asociación de una variable consigo misma
      cor_matrix[i, j] <- round(calculate_association(data, variables[i], variables[j]),3)
    }
    else{cor_matrix[i,j] <- 1.000} # Añadimos la correlación consigo misma
  }
}

# Moldeamos la matriz para el mapa de calor
melted_matrix <- melt(cor_matrix)

# Creamos el mapa de calor
ggplot(melted_matrix, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), name = "Correlación") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +  # Centrar el título
  xlab("") +
  ylab("") +
  ggtitle("Mapa de calor para las correlaciones")













