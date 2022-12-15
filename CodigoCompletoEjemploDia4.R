# Cargo las funciones que voy a utilizar después
source("FuncionesRosa.R")

# Cargo las librerias 


#Cargo los datos depurados
datos<-readRDS("todo_cont") #todo_cont contiene las variables y transformaciones para la variable continua

#Hago la partición
set.seed(12345678)
trainIndex <- createDataPartition(datos$varObjCont, p=0.8, list=FALSE)
data_train <- datos[trainIndex,]
data_test <- datos[-trainIndex,]

# Este fue el modelo ganador el día 2
modeloManual<-lm(varObjCont~Clasificacion+Etiqueta+CalifProductor,data=data_train)
summary(modeloManual)
Rsq(modeloManual,"varObjCont",data_train)
Rsq(modeloManual,"varObjCont",data_test)

#A la vista de los resultados todas las categorías son significativas en todas sus categorías
#por lo que no es necesario unir ninguna. 

#En CalifProductor unimos categoría 0 y 1, por otro lado 5-12, el resto de categorías de la variable solas

#Si la categoría 2 hubiese salido no significativa la unimos a la 0-1
#Se crea una nueva variable CalifProductor2 
# Uno con el 0 y el 1 la categoría 2 y evalúo el modelo

#datos$CalifProductor2<-recode(datos$CalifProductor, "'0-1'='0-2';2='0-2'")
#data_train <- datos[trainIndex,]
#data_test <- datos[-trainIndex,]

#modeloManual2<-lm(varObjCont~Clasificacion+Etiqueta+CalifProductor2,data=data_train)
#summary(modeloManual2)
#Rsq(modeloManual2,"varObjCont",data_train) 
#Rsq(modeloManual2,"varObjCont",data_test) 
#el modelo es prácticamente el mismo, por lo que la unión no ha supuesto pérdida de información

# Seleccion de variables "clásica"
null<-lm(varObjCont~1, data=data_train) #Modelo minimo
full<-lm(varObjCont~., data=data_train[,c(1:14,25)]) #Modelo maximo, le quitamos las transformaciones
modeloStepAIC<-step(null, scope=list(lower=null, upper=full), direction="both")
summary(modeloStepAIC)
Rsq(modeloStepAIC,"varObjCont",data_test)

modeloBackAIC<-step(full, scope=list(lower=null, upper=full), direction="backward")
summary(modeloBackAIC)
Rsq(modeloBackAIC,"varObjCont",data_test) #son iguales

coef(modeloStepAIC)
coef(modeloBackAIC)

modeloStepBIC<-step(null, scope=list(lower=null, upper=full), direction="both",k=log(nrow(data_train)))
summary(modeloStepBIC)
Rsq(modeloStepBIC,"varObjCont",data_test) #Un pelin peor que el anterior, habrá que mirar el número de parámetros

modeloBackBIC<-step(full, scope=list(lower=null, upper=full), direction="backward",k=log(nrow(data_train)))
summary(modeloBackBIC)
Rsq(modeloBackBIC,"varObjCont",data_test) # son iguales

coef(modeloStepBIC)
coef(modeloBackBIC)

#Método Stepwise y Backward son iguales,ver mejor modelo AIC o BIC según número parámetros
modeloStepAIC$rank
modeloStepBIC$rank

Rsq(modeloStepBIC,"varObjCont",data_test)
Rsq(modeloStepAIC,"varObjCont",data_test)


#Genero interacciones
formInt<-formulaInteracciones(datos[,c(1:14,25)],15)#en el subconjunto de las vbles. originales, la objetivo está en la columna 25
fullInt<-lm(formInt, data=data_train) #Modelo con todas las variables y todas las interacciones

modeloStepAIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both")
modeloStepAIC_int$rank
summary(modeloStepAIC_int)
Rsq(modeloStepAIC_int,"varObjCont",data_test) #Parecen algo mejores que los anteriores

modeloStepBIC_int<-step(null, scope=list(lower=null, upper=fullInt), direction="both",k=log(nrow(data_train)))
summary(modeloStepBIC_int)
Rsq(modeloStepBIC_int,"varObjCont",data_test) #Un pelin mejor

modeloStepAIC_int$rank #muchos más parámetros
modeloStepBIC_int$rank

#Por el principio de parsimonia, es preferible el modeloStepBIC_int

# Pruebo con todas las transf y las variables originales
fullT<-lm(varObjCont~., data=data_train)

modeloStepAIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both")
summary(modeloStepAIC_trans)
Rsq(modeloStepAIC_trans,"varObjCont",data_test)

modeloStepBIC_trans<-step(null, scope=list(lower=null, upper=fullT), direction="both",k=log(nrow(data_train)))
summary(modeloStepBIC_trans)
Rsq(modeloStepBIC_trans,"varObjCont",data_test) 

modeloStepAIC_trans$rank 
modeloStepBIC_trans$rank

#No está claro cuál es preferible.

#Trans e interacciones
formIntT<-formulaInteracciones(datos,25)
fullIntT<-lm(formIntT, data=data_train)

modeloStepAIC_transInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both")
summary(modeloStepAIC_transInt)
Rsq(modeloStepAIC_transInt,"varObjCont",data_test) 

modeloStepBIC_transInt<-step(null, scope=list(lower=null, upper=fullIntT), direction="both",k=log(nrow(data_train)))
summary(modeloStepBIC_transInt)
Rsq(modeloStepBIC_transInt,"varObjCont",data_test)  

modeloStepAIC_transInt$rank 
modeloStepBIC_transInt$rank

#Por el principio de parsimonia, es preferible el modeloStepBIC_transInt

## Pruebo los mejores de cada con validacion cruzada repetida
total<-c()
modelos<-sapply(list(modeloManual,modeloStepAIC,modeloStepBIC,modeloStepBIC_int,modeloStepAIC_trans,modeloStepBIC_trans,modeloStepBIC_transInt),formula)
for (i in 1:length(modelos)){
  set.seed(1712)
  vcr<-train(as.formula(modelos[[i]]), data = data_train,
             method = "lm",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      returnResamp="all")
  )
  total<-rbind(total,cbind(vcr$resample[,1:2],modelo=rep(paste("Modelo",i),
                                                         nrow(vcr$resample))))
}
boxplot(Rsquared~modelo,data=total,main="R-Square") 
aggregate(Rsquared~modelo, data = total, mean) #el 4 y el 7 son mejores
aggregate(Rsquared~modelo, data = total, sd) #su variabilidad es algo más alta tb
#vemos el número de parametros
length(coef(modeloStepBIC_int))
length(coef(modeloStepBIC_transInt))
#Igual número de parámetros
formula(modeloStepBIC_int)
formula(modeloStepBIC_transInt)

## Seleccion aleatoria (se coge la submuestra de los datos de entrenamiento)
rep<-100
prop<-0.7 # se realiza con el 70% de los datos de entrenamiento por velocidad. El resultado es el mismo.
modelosGenerados<-c()
for (i in 1:rep){
  set.seed(12345+i)
  subsample<-data_train[sample(1:nrow(data_train),prop*nrow(data_train),replace = T),]
  full<-lm(formIntT,data=subsample)
  null<-lm(varObjCont~1,data=subsample)
  modeloAux<-step(null,scope=list(lower=null,upper=full),direction="both",trace=0,k=log(nrow(subsample)))
  modelosGenerados<-c(modelosGenerados,paste(sort(unlist(strsplit(as.character(formula(modeloAux))[3]," [+] "))),collapse = "+"))
}
freq(modelosGenerados,sort="dec")

#De las 100 repeticiones, las 3 que más se repiten son:

#CalifProductor+Clasificacion+Clasificacion:Etiqueta+Etiqueta                                        31
#Acidez+CalifProductor+Clasificacion+Clasificacion:Etiqueta+Etiqueta                                 25
#Acidez+Alcohol+CalifProductor+Clasificacion+Clasificacion:Etiqueta+Etiqueta  

## Comparación final, tomo el ganador de antes y los nuevos candidatos
total2<-c()

modelos2<-c("varObjCont ~ Clasificacion + Etiqueta + CalifProductor + Acidez + 
    Clasificacion:Etiqueta",
            "varObjCont ~CalifProductor+Clasificacion+Clasificacion:Etiqueta+Etiqueta",
            "varObjCont ~Acidez+CalifProductor+Clasificacion+Clasificacion:Etiqueta+Etiqueta",
            "varObjCont ~Acidez+Alcohol+CalifProductor+Clasificacion+Clasificacion:Etiqueta+Etiqueta")

for (i in 1:length(modelos2)){
  set.seed(1712)
  vcr<-train(as.formula(modelos2[[i]]), data = data_train,
             method = "lm",
             trControl = trainControl(method="repeatedcv", number=5, repeats=20,
                                      returnResamp="all")
  )
  total2<-rbind(total2,cbind(vcr$resample[,1:2],modelo=rep(paste("Modelo",i),
                                                         nrow(vcr$resample))))
}

boxplot(Rsquared~modelo,data=total2,main="R-Square") 
aggregate(Rsquared~modelo, data = total2, mean) 
aggregate(Rsquared~modelo, data = total2, sd) 

#los otros 4 son similares en R2.Tomaremos como ganador el segundo pues tiene menos variables.

#Una vez decidido el mejor modelo, hay que evaluarlo 
ModeloGanador<-lm(varObjCont ~CalifProductor+Clasificacion+Clasificacion:Etiqueta+Etiqueta,data=data_train)


# Vemos los coeficientes del modelo ganador
coef(ModeloGanador)

#Evaluamos la estabilidad del modelo a partir de las diferencias en train y test:
Rsq(ModeloGanador,"varObjCont",data_train)
Rsq(ModeloGanador,"varObjCont",data_test) 

#Este código se puede aplicar a regresión logística con pequeñas modificaciones (básicamente cambiar lm() por glm(), con la opción family=binomial, RSQ por Pseudo-R)
# Para ello, ver código del pdf RegresiónLogísticaconR


