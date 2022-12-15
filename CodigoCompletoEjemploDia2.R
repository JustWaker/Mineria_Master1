# Cargo las funciones que voy a utilizar después
source("FuncionesRosa.R")

# Cargo las librerias que me van a hacer falta sino las tengo ya cargadas

# Parto de los datos sin atípicos ni ausentes guardados
datos<-readRDS("datosVinoDep")
varObjCont<-datos$varObjCont
varObjBin<-datos$varObjBin
input<-datos[,-(1:2)]


#Obtengo la importancia de las variables. Falla si hay alguna variable cuantitativa con menos de 6 valores diferentes
graficoVcramer(input,varObjBin)
graficoVcramer(input,varObjCont)

#Veo gráficamente el efecto de dos variables cualitativas sobre la binaria
mosaico_targetbinaria(input$Region,varObjBin,"Region") #esta no influye
mosaico_targetbinaria(input$Clasificacion,varObjBin,"Clasificacion") #esta sí influye

barras_targetbinaria(input$Region,varObjBin,"Region")
barras_targetbinaria(input$Clasificacion,varObjBin,"Clasificacion")

#Veo gráficamente el efecto de dos variables cuantitativas sobre la binaria

boxplot_targetbinaria(input$Densidad,varObjBin,"Densidad")
boxplot_targetbinaria(input$Acidez,varObjBin,"Acidez")

hist_targetbinaria(input$Densidad,varObjBin,"Densidad")
hist_targetbinaria(input$Acidez,varObjBin,"Acidez")


#Todas las variables numéricas frente a la objetivo continua
graficoCorrelacion(varObjCont,input) #Nos fijamos en la forma de las líneas rojas (si hay muchas variables numéricas, tarda un poco)
corrplot(cor(cbind(varObjCont,Filter(is.numeric, input)), use="pairwise", method="pearson"), method = "ellipse",type = "upper")

#Busco las mejores transformaciones para las variables numéricas con respesto a los dos tipos de variables
input_cont<-cbind(input,Transf_Auto(Filter(is.numeric, input),varObjCont))
input_bin<-cbind(input,Transf_Auto(Filter(is.numeric, input),varObjBin))

saveRDS(data.frame(input_bin,varObjBin),"todo_bin")
saveRDS(data.frame(input_cont,varObjCont),"todo_cont")


## Comienzo con la regresión lineal
todo<-data.frame(input,varObjCont)

#Obtengo la partición
set.seed(123456)
trainIndex <- createDataPartition(todo$varObjCont, p=0.8, list=FALSE)
data_train <- todo[trainIndex,]
data_test <- todo[-trainIndex,]

#Construyo un modelo preliminar con todas las variables
modelo1<-lm(varObjCont~.,data=data_train)
summary(modelo1)

Rsq(modelo1,"varObjCont",data_train)
Rsq(modelo1,"varObjCont",data_test) #En test hay bastante diferencia, seguramente sobren variables

# Nos fijamos en la importancia de las variables. Podemos sacar un gráfico que muestra lo que se pierde en R2 en train al quitarlas del modelo
modelEffectSizes(modelo1)
barplot(sort(modelEffectSizes(modelo1)$Effects[-1,4],decreasing =T),las=2,main="Importancia de las variables (R2)")

#Pruebo un modelo con menos variables. Recuerdo el gráfico de Cramer
graficoVcramer(todo,varObjCont) #Pruebo con las más importantes
modelo2<-lm(varObjCont~Clasificacion+Etiqueta+CalifProductor+prop_missings,data=data_train)
modelEffectSizes(modelo2)
summary(modelo2)
Rsq(modelo2,"varObjCont",data_train)
Rsq(modelo2,"varObjCont",data_test) 

modelox<-lm(varObjCont~Acidez+Alcohol+Clasificacion+Etiqueta+CalifProductor,data=data_train)
summary(modelox)
Rsq(modelox,"varObjCont",data_train)
Rsq(modelox,"varObjCont",data_test)

#Pruebo un modelo con menos variables, basándome en la importancia de las variables
modelo3<-lm(varObjCont~Clasificacion+Etiqueta+CalifProductor,data=data_train)
summary(modelo3)
Rsq(modelo3,"varObjCont",data_train)
Rsq(modelo3,"varObjCont",data_test) 

#Pruebo con una interaccion sobre el anterior
# Se podrían probar todas las interacciones dos a dos. Esta es la que mejor resultado da
modelo4<-lm(varObjCont~Clasificacion+Etiqueta+CalifProductor+Clasificacion:Etiqueta,data=data_train)
summary(modelo4)
Rsq(modelo4,"varObjCont",data_train)
Rsq(modelo4,"varObjCont",data_test)

#Hago validación repetida para ver qué modelo es mejor
modelo1VC <- train(formula(modelo1),
                   data = todo,method = "lm",
                   trControl = trainControl(method="repeatedcv", number=5, repeats=20, returnResamp="all")
)

modelo2VC <- train(formula(modelo2),
                   data = todo,method = "lm",
                   trControl = trainControl(method="repeatedcv", number=5, repeats=20, returnResamp="all")
)

modelo3VC <- train(formula(modelo3),
                   data = todo,method = "lm",
                   trControl = trainControl(method="repeatedcv", number=5, repeats=20, returnResamp="all")
)

modelo4VC <- train(formula(modelo4),
                   data = todo,method = "lm",
                   trControl = trainControl(method="repeatedcv", number=5, repeats=20, returnResamp="all")
)

results<-data.frame(rbind(modelo1VC$resample,modelo2VC$resample,modelo3VC$resample,modelo4VC$resample),modelo=c(rep(1,100),rep(2,100),rep(3,100),rep(4,100)))
boxplot(Rsquared~modelo,data=results)
aggregate(Rsquared~modelo, data = results, mean) #el 4 tiene mayor R2 medio
aggregate(Rsquared~modelo, data = results, sd) #tb tiene mayor variabilidad

length(coef(modelo1));length(coef(modelo2));length(coef(modelo3));length(coef(modelo4))

#Si se considera que la mejora en R2 no compensa el aumento de parámetros, elegimos el 3
# Vemos los coeficientes del modelo ganador
coef(modelo3)

#Evaluamos la estabilidad del modelo a partir de las diferencias en train y test:
Rsq(modelo3,"varObjCont",data_train)
Rsq(modelo3,"varObjCont",data_test) 

# Vemos las variables más importantes del modelo ganador
modelEffectSizes(modelo3)
barplot(sort(modelEffectSizes(modelo3)$Effects[-1,4],decreasing =T),las=2,main="Importancia de las variables (R2)")

