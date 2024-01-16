# Cargamos el data.frame con los datos
load("data_models.RData")

# Cargamos las librerías necesarias
library(caret) # Para el entrenamiento y validación de modelos
library(pROC) # Para realizar las gráficas ROC
library(Matrix) # Para el cálculo del rango

# Separamos en train y test (75% - 25%)
set.seed(2024)
indx <- createDataPartition(y = data$Type, p = 0.75, list = FALSE) # Creamos la partición
trainData <- data[indx, ] # Entrenamiento
testData <- data[-indx, ] # Test

# Preparamos el objeto para almacenar la codificación one-hot
trainDataOH <- data[indx, ]
testDataOH <- data[-indx, ]

# Aplicamos One-Hot Encoding a las variables categóricas (excepto la variable respuesta)
dummies <- dummyVars("~ . ", data = trainData[,-5])
trainDataOH <- as.data.frame(predict(dummies, newdata = trainDataOH))
testDataOH <- as.data.frame(predict(dummies, newdata = testDataOH))

# Preprocesamos Age
preProcessAge <- preProcess(trainDataOH[,"Age", drop = FALSE], method = "range")
trainDataOH$Age <- unlist(predict(preProcessAge, trainDataOH[,"Age", drop = FALSE]))
testDataOH$Age <- unlist(predict(preProcessAge, testDataOH[,"Age", drop = FALSE]))

# Añadimos la variable de respuesta de nuevo al conjunto transformado
trainDataOH$Type <- trainData$Type
testDataOH$Type <- testData$Type

# Control de entrenamiento
trainControlNone <- trainControl(method = "none") # Para no implementar CV
trainControlCV <- trainControl(method = "cv", number = 10) # Implementamos 10-fold CV

# Comprobamos rango de matriz de diseño one-hot
rankMatrix(trainDataOH[,-15])[1] # Rango 12, 14 variables (se elimina la respuesta)

# Selección de variables
set.seed(2024)
varImp(train(Type~., data = trainData, method = "glm")) # Eliminamos Density2 y Shape2
# Comprobamos que la nueva matriz es de rango máximo
rankMatrix(trainDataOH[,-c(3,13,15)])

# Guardamos los conjuntos para entrenar las penalizaciones Lasso, Ridge y Elastic
trainDataOHrl<- trainDataOH
testDataOHrl<- testDataOH

# Ajustamos la matriz de diseño para evitar la multicolinealidad
trainDataOH <- trainDataOH[,-c(3,13)]

# Función para entrenar y evaluar un modelo
train_and_evaluate <- function(model_name, train_data, test_data, normalization, method, tr_control, tune_grid=NULL) {
	
  # Entrenamiento del modelo
  if (tr_control$method == "none") {
    set.seed(14041996)
  }
  else{set.seed(54126600)}
	# Usamos el parámetro family para regresión logística
    if (method == "glm") {
        model <- train(Type ~ ., data = train_data, method = method, trControl = tr_control, tuneGrid = tune_grid, family = "binomial")
    } 
	else if (method == "svmLinear" || method == "svmRadial"){
		tr_control$classProbs <- TRUE
		tr_control$summaryFunction<- twoClassSummary
		
		levels(train_data$Type) <- c("Benigno", "Maligno")
	    levels(test_data$Type) <- c("Benigno", "Maligno")
		
        model <- train(Type ~ ., data = train_data, method = method, trControl = tr_control, tuneGrid = tune_grid, metric = "ROC")
    }
	else{
		model <- train(Type ~ ., data = train_data, method = method, trControl = tr_control, tuneGrid = tune_grid)
	}

  # Predicciones y métricas
	
  # Usamos semillas diferentes para no cv / cv
  if (tr_control$method == "none") {
    set.seed(14041996)
  }
  else{set.seed(54126600)}
  predictions <- predict(model, newdata = test_data)
	
  # Para modelos SVM (el método difiere)
  if(method == "svmLinear" || method == "svmRadial"){	  
	  prob <- predict(model, newdata = test_data, type = "prob")
	  
	  rocCurve <- roc(response = test_data$Type, predictor = prob[,2], levels = c("Benigno", "Maligno"), auc = TRUE)
      aucValue <- round(rocCurve$auc, 3)
	  
	  confusionMat <- confusionMatrix(predictions, test_data$Type, positive = "Maligno")
  }
  # Para modelos kNN, NB, RL
  else{
	  prob <- predict(model, newdata = test_data, type = "prob")
	  
	  rocCurve <- roc(response = test_data$Type, predictor = prob[,2])
      aucValue <- round(rocCurve$auc, 3)
	  
	  confusionMat <- confusionMatrix(predictions, test_data$Type, positive = "1")
	  
	  dimnames(confusionMat$table) <- list(Predicción = c("Benigno", "Maligno"),
									 Real = c("Benigno", "Maligno"))
  }	
  
  TP <- confusionMat$table[2,2]
  TN <- confusionMat$table[1,1]
  FP <- confusionMat$table[2,1]
  FN <- confusionMat$table[1,2]	

  # Cálculo de métricas
	
  # Calculamos la prueba binomial para obtener el intervalo de confianza para la precisión (Accuracy)
  acc_test <- binom.test(TP + TN, TP + TN + FP + FN)
  acc_confint <- round(acc_test$conf.int, 3)

  # Calculamos la prueba binomial para obtener el intervalo de confianza para el FPR
  fpr_test <- binom.test(FP, FP + TN)
  fpr_confint <- round(fpr_test$conf.int, 3)

  # Almacenar resultados
  metrics <- data.frame(
    Model = model_name,
	Normalization = normalization,
    Accuracy = round(confusionMat$overall['Accuracy'], 3),
	Kappa = round(confusionMat$overall['Kappa'], 3),
	ROC_AUC = round(aucValue,3),
	Sensitivity = round(confusionMat$byClass['Sensitivity'], 3),
	Specificity = round(confusionMat$byClass['Specificity'], 3),
	PPV = round(confusionMat$byClass['Pos Pred Value'], 3),
	FPR = round(FP / (FP + TN), 3)
  )
  
  # Generar gráficos ROC
   
  rocData <- data.frame(
  specificity = rocCurve$specificities,
  sensitivity = rocCurve$sensitivities,
  ratio = rocCurve$sensitivities / (1 - rocCurve$specificities)
  )
  

  # Curva ROC
  ggrocPlot <- ggplot(rocData) +
    geom_line(aes(x = 1 - specificity, y = sensitivity), color = "blue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    labs(title = model_name, x = "Ratio falsos positivos", y = "Ratio verdaderos positivos") +
    annotate("text", x = 0.7, y = 0.2, label = paste("AUC =", aucValue), hjust = 0, vjust = 0, size = 7, color = "black") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))  # Centrar el título	
  
  png(filename = paste(model_name, "_auc.png", sep = ""))
	  
  print(ggrocPlot)
	  
  dev.off()
	
  png(filename = paste(model_name, "_cm.png", sep = ""))
	
  # Gráfico matriz de confusión (en todos los casos)
  fourfoldplot(confusionMat$table)
  mtext(paste("Matriz de confusión modelo", model_name), side = 3, line = -1.1, cex = 1)
  text(x = 0.5, y = 0.75, labels = paste("Precisión = ", round(confusionMat$overall['Accuracy'], 3)), cex = 0.7)
  text(x = 0.5, y = 0.6, labels = paste("Precisión CI: [", acc_confint[1], ",", acc_confint[2], "]"), cex = 0.7)
  text(x = -0.55, y = -0.5, labels = paste("FPR = ", round(FP / (FP + TN), 3)), cex = 0.7)
  text(x = -0.55, y = -0.65, labels = paste("FPR CI: [", fpr_confint[1], ",", fpr_confint[2], "]"), cex = 0.7)
	
  dev.off()	
	
  return(list(model = model, metrics = metrics, confusionMat = confusionMat, rocCurve))
}

# Lista de parámetros para cada modelo
model_params <- list(
  knnModel1 = list(
    model_name = "knn-k14-raw-sincv", 
    normalization = "none",
    train_data = trainData, 
    test_data = testData, 
    method = "knn", 
    tr_control = trainControlNone, 
    tune_grid = data.frame(k = 14)
  ),
  knnModel2 = list(
    model_name = "knn-k14-onehot-sincv",
    normalization = "one-hot",
    train_data = trainDataOH, 
    test_data = testDataOH, 
    method = "knn", 
    tr_control = trainControlNone, 
    tune_grid = data.frame(k = 14)
  ),
  knnModel3 = list(
    model_name = "knn-k.interval.3_25-raw-concv", 
    normalization = "none",
    train_data = trainData, 
    test_data = testData, 
    method = "knn", 
    tr_control = trainControlCV, 
    tune_grid = expand.grid(k = seq(3, 25, by = 1))
  ),
  knnModel4 = list(
    model_name = "knn-k.interval.3_25-onehot-concv",
    normalization = "one-hot",
    train_data = trainDataOH, 
    test_data = testDataOH, 
    method = "knn", 
    tr_control = trainControlCV, 
    tune_grid = expand.grid(k = seq(3, 25, by = 1))
  ),
  svmModel1 = list(
    model_name = "svm-linear-onehot-sincv-C1",
    normalization = "one-hot",
    train_data = trainDataOH,
    test_data = testDataOH,
    method = "svmLinear",
    tr_control = trainControlNone,
    tune_grid = data.frame(C = 1)
  ),
  svmModel2 = list(
    model_name = "svm-linear-onehot-concv",
    normalization = "one-hot",
    train_data = trainDataOH,
    test_data = testDataOH,
    method = "svmLinear",
    tr_control = trainControlCV,
    tune_grid = expand.grid(C = seq(0.1, 2, by = 0.1))
  ),
  svmModel3 = list(
    model_name = "svm-rbf-onehot-sincv-C1",
    normalization = "one-hot",
    train_data = trainDataOH,
    test_data = testDataOH,
    method = "svmRadial",
    tr_control = trainControlNone,
    tune_grid = NULL # Optimiza por defecto
  ),
  svmModel4 = list(
    model_name = "svm-rbf-onehot-concv",
    normalization = "one-hot",
    train_data = trainDataOH,
    test_data = testDataOH,
    method = "svmRadial",
    tr_control = trainControlCV,
    tune_grid = expand.grid(C = seq(0.1, 2, by = 0.1), sigma = seq(0.2, 1.5, length = 21))
  ),
  rlModel1 = list(
    model_name = "rl-raw-cv",
    normalization = "none",
    train_data = trainData,
    test_data = testData,
    method = "glm",
    tr_control = trainControlCV,
    tune_grid = NULL
  ),
	rlModel2 = list(
    model_name = "rl-onehot-nocv",
    normalization = "one-hot",
    train_data = trainDataOH,
    test_data = testDataOH,
    method = "glm",
    tr_control = trainControlNone,
    tune_grid = NULL
  ),
	rlModel3 = list(
    model_name = "rl-onehot-cv",
    normalization = "one-hot",
    train_data = trainDataOH,
    test_data = testDataOH,
    method = "glm",
    tr_control = trainControlCV,
    tune_grid = NULL
  ),
  rlModel4 = list(
    model_name = "rl-onehot-lasso-cv",
    normalization = "one-hot",
    train_data = trainDataOHrl,
    test_data = testDataOHrl,
    method = "glmnet",
    tr_control = trainControlCV,
    tune_grid = expand.grid(alpha = 1, lambda = seq(0.001, 0.1, length = 10))
  ),
  rlModel5 = list(
    model_name = "rl-onehot-ridge-cv",
    normalization = "one-hot",
    train_data = trainDataOHrl,
    test_data = testDataOHrl,
    method = "glmnet",
    tr_control = trainControlCV,
    tune_grid = expand.grid(alpha = 0, lambda = seq(0.001, 0.1, length = 10))
  ),
  rlModel6 = list(
    model_name = "rl-onehot-elastic-cv",
    normalization = "one-hot",
    train_data = trainDataOHrl,
    test_data = testDataOHrl,
    method = "glmnet",
    tr_control = trainControlCV,
    tune_grid = expand.grid(alpha = seq(0, 1, length = 5), lambda = seq(0.001, 0.1, length = 10))
  ),
  nbModel1 = list(
    model_name = "nb-raw-nocv-laplace0",
    normalization = "none",
    train_data = trainData,
    test_data = testData,
    method = "naive_bayes",
    tr_control = trainControlNone,
    tune_grid = expand.grid(laplace = 0, usekernel = FALSE, adjust = 1)
  ),
  nbModel2 = list(
    model_name = "nb-raw-laplace0_0.5_1-cv-usekernel",
    normalization = "none",
    train_data = trainData,
    test_data = testData,
    method = "naive_bayes",
    tr_control = trainControlCV,
    tune_grid = expand.grid(laplace = c(0, 0.5, 1), usekernel = c(FALSE,TRUE), adjust = c(0.5,1,2))
  ),
  nbModel3 = list(
    model_name = "nb-onehot-nocv-laplace0",
    normalization = "one-hot",
    train_data = trainDataOH,
    test_data = testDataOH,
    method = "naive_bayes",
    tr_control = trainControlNone,
    tune_grid = expand.grid(laplace = 0, usekernel = FALSE, adjust = 1)
  ),
  nbModel4 = list(
    model_name = "nb-onehot-laplace0.5_1-cv",
    normalization = "one-hot",
    train_data = trainDataOH,
    test_data = testDataOH,
    method = "naive_bayes",
    tr_control = trainControlCV,
    tune_grid = expand.grid(laplace = c(0, 0.5, 1), usekernel = c(FALSE,TRUE), adjust = c(0.5,1,2))
  )
)

# Función para aplicar train_and_evaluate a cada conjunto de parámetros
apply_model <- function(params) {
  do.call(train_and_evaluate, params)
}

# Aplicamos la función a cada conjunto de parámetros
results_list <- lapply(model_params, apply_model)

# Combinamos todas las métricas en un dataframe
all_metrics <- do.call(rbind, lapply(results_list, function(x) x$metrics))
