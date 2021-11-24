barras_abandono <- function(variable){
  if(is.numeric(personal[[variable]])){
    stop('Esta función grafica variables categóricas, no numéricas.')
  }
  
  personal %>%  
    group_by(.data[[variable]]) %>% 
    summarise(Tasa_abandono = round(mean(abandono)*100, 2) %>%  paste0('%')) %>%  
    print()
  
  qplot(data = personal,
        x = get(variable),
        geom = 'bar',
        fill = abandono) +
    theme_minimal() +
    xlab(variable) +
    scale_fill_manual(values = c('#93C6F0', "#F87060"))
}

boxplot_abandono <- function(variable){
  if(!is.numeric(personal[[variable]])){
    stop('Esta función grafica variables numéricas, no categóricas.')
  }
  
  qplot(data = personal,
        y = get(variable),
        geom = 'boxplot',
        x = abandono,
        fill = abandono,
        xlab = '',
        ylab = gsub(pattern = '_', replacement = ' ', x = variable)) +
    theme_minimal() +
    scale_fill_manual(values = c('#93C6F0', "#F87060"))
}

entrenar_modelo_abandono <- function(formula){
  # fijar una semilla hace que los resultados sean reproducibles
  set.seed(1)
  
  # define muestreo aleatorio de filas para entrenar el modelo
  indice_aleatorio_80_porciento <- runif(nrow(datos_elegidos)) < .8 
  entrenamiento <- datos_elegidos[indice_aleatorio_80_porciento, ]
  
  # entrena el modelo
  modelo <- glm(data = entrenamiento, formula = formula, family = binomial())
  
  # probamos el modelo sobre los datos que no se usaron en el entrenamiento
  prueba <- datos_elegidos[!indice_aleatorio_80_porciento, ]
  
  # calcula la métrica de ajuste "AUC".
  # para una explicacion detallada, ver 
  # https://www.youtube.com/watch?v=fzZBpgm22PI
  AUC <- auc(actual = prueba$abandono, 
             predicted = predict(modelo, 
                                 newdata = prueba,
                                 type = 'response'))
  
  message('El área sobre la curva para los datos de test para este modelo es: ', round(AUC, 3), '\n')
  
  # devuelve una versión simplificada del resumen de coeficientes del modelo
  resumen <- modelo %>% 
    tidy() %>% 
    transmute(variable = term,
              coeficiente = estimate, 
              significancia_estadistica = p.value < 0.05)
  
  print(resumen)
  
  return(modelo)
}

normalizar_numericas <- function(datos){
  # elegimos solamente las variables numericas
  indice_numericas <- sapply(datos, is.numeric) 
  numericas <- datos[indice_numericas]
  
  # la funcion lapply aplica una transformacion a cada fila
  # y la funcion scale hace la normalizacion propiamente
  numericas_estandarizadas <- as.data.frame(lapply(numericas, scale))
  
  # y le unimos las categoricas para no perderlas
  categoricas <- datos[!indice_numericas]
  tabla_completa <- cbind(categoricas,
                          numericas_estandarizadas)
  
  return(tabla_completa)
}