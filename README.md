# TFM
Este repositorio se ha creado con el objetivo de depositar los códigos relacionados con mi Trabajo de Fin de Máster.
Podrás encontrar 2 carpetas y 2 ficheros pdf.

## Shiny
Contiene un fichero app.R con el código Shiny de R desarrollado para implementar una aplicación web para la predicción de cáncer de mama.
Además, contiene dos ficheros de datos de R (.RData) que corresponden al modelo implementado en la aplicación web ("optimal_model.RData") y el preprocesado de datos necesario ("preProcess.RData")
La subcarpeta www contiene las imágenes que se utilizan en la aplicación web

## R
Contiene diversos ficheros de código y datos R para reproducir los resultados del TFM.
- exploratorio.R contiene el código para reproducir los resultados del capítulo 5.1
- modelos.R contiene el código para reproducir los resultados del capítulo 5.2
- mama.data es el fichero de datos original
- data_mammographic_mass.RData es un fichero de datos de R con el conjunto de datos original
- data_models.RData es un fichero de datos de R con el conjunto de datos reducido para los resultados del capítulo 5.2
- modelos.RData es un fichero de datos de R que contiene una lista con los modelos entrenados, sus métricas y matrices de confusión
- metricas.RData es un fichero de datos de R que contiene un data.frame con las métricas de todos los modelos entrenados

