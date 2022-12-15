## minería de datos y modelización predictiva
## Manuel Amaral



# Cargo las funciones que voy a utilizar despu?s
source("C:/Users/manue/OneDrive - NOVAIMS/Desktop/Master/Mineria - Rosa/FuncionesRosa.R")

install.packages("readxl")
install.packages('questionr')
install.packages('psych')
install.packages('car')
install.packages('corrplot')
install.packages('caret')
install.packages('ggplot2')
install.packages('lmSupport')
install.packages('unmarked')
install.packages('VGAM')
install.packages('pROC')
install.packages('glmnet')
install.packages('tidyverse')


library('readxl')
library('questionr')
library('psych')
library('car')
library('corrplot')
library('caret')
library('ggplot2')
library('lmSupport')
library('unmarked')
library('VGAM')
library('pROC')
library('glmnet')
library('tidyverse') #DataCleansing & Pipe





datosimp <- read_excel('C:/Users/manue/OneDrive - NOVAIMS/Desktop/Master/Mineria - Rosa/DatosImpuestos_Tarea.xlsx', sheet = 1)
datosimp

str(datosimp)
summary(datosimp)

#depuramiento

datosimp$Empresario <- replace(datosimp$Empresario, which((datosimp$Empresario < 0) | (datosimp$Empresario>100)), NA)
datosimp$DeudaSaldada <- replace(datosimp$DeudaSaldada, which((datosimp$DeudaSaldada < 0) | (datosimp$DeudaSaldada>100)), NA)
datosimp$Locales <- replace(datosimp$Locales, which(datosimp$Locales == 99999), NA)
datosimp$Tamano <- replace(datosimp$Tamano, which(datosimp$Tamano == '?'), NA)


datosimp$ActEconom <- NULL
datosimp$ActEconom_Cuali <- NULL
datosimp$Basura <- NULL
datosimp$Basura_Cuali <- NULL
datosimp$CCAA <- replace(datosimp$CCAA, which((datosimp$CCAA == 'Ceuta') | (datosimp$CCAA == 'Melilla')), 'Aragón')
datosimp$Distrito <- NULL


#vehiculo <- datosimp$Vehiculo
#vehiculocuali <- datosimp$Vehiculo_Cuali
datosimp$Actividad_Municipio


# Indico los factores
datosimp[,c('CCAA','Vehiculo_Cuali','Actividad_Municipio','Tamano')] <- lapply(datosimp[,c('CCAA','Vehiculo_Cuali','Actividad_Municipio','Tamano')], factor)
summary(datosimp)


# Ver el reparto de las categor?as de las variables cualitativas
freq(datosimp$CCAA)
freq(datosimp$Vehiculo_Cuali)
freq(datosimp$Actividad_Municipio)
freq(datosimp$Tamano)

# Cuento el n?mero de valores diferentes para las num?ricas
sapply(Filter(is.numeric, datosimp),function(x) length(unique(x))) #Si detecto alguna variable adicional, uso el c?digo de antes

# Para otros estad?sticos
describe(Filter(is.numeric, datosimp)) #hay otro describe en otra libreria

#Indico la variableObj, el ID y las Input (los at?picos y los missings se gestionan s?lo de las input)
varObjCont<-datosimp$Vehiculo
varObjBin<-datosimp$Vehiculo_Cuali
input<-select(datosimp,-c("Vehiculo","Vehiculo_Cuali"))


##At?picos
# Cuento el porcentaje de at?picos de cada variable. 
sapply(Filter(is.numeric, input),function(x) atipicosAmissing(x)[[2]])/nrow(input)
# Modifico los at?picos como missings
input[,as.vector(which(sapply(input, class)=="numeric"))]<-sapply(Filter(is.numeric, input),function(x) atipicosAmissing(x)[[1]])

## MISSINGS
#Busco si existe alg?n patr?n en los missings, que me pueda ayudar a entenderlos
corrplot(cor(is.na(input[colnames(input)[colSums(is.na(input))>0]])),method = "ellipse",type = "upper") #No se aprecia ning?n patr?n

#Proporci?n de missings por variable y observaci?n
input$prop_missings<-apply(is.na(input),1,mean)
summary(input$prop_missings)
(prop_missingsVars<-apply(is.na(input),2,mean))

#elimino las observaciones y las variables con m?s de la mitad de datos missings (No hay ninguna, no ejecuto este c?digo)
input <- subset(input, prop_missings< 0.5, select=names(prop_missingsVars)[prop_missingsVars<0.5])
varObjBin<-varObjBin[input$prop_missings<0.5] #Actualizar las observaciones de las variables objetivo
varObjCont<-varObjCont[input$prop_missings<0.5] 

## Imputaciones
# Imputo todas las cuantitativas, seleccionar el tipo de imputaci?n: media, mediana o aleatorio
input[,as.vector(which(sapply(input, class)=="numeric"))]<-sapply(Filter(is.numeric, input),function(x) ImputacionCuant(x,"aleatorio"))

# Imputo todas las cualitativas, seleccionar el tipo de imputaci?n: moda o aleatorio
# Si solo se quiere imputar una, variable<-ImputacionCuali(variable,"moda")
input[,as.vector(which(sapply(input, class)=="factor"))]<-sapply(Filter(is.factor, input),function(x) ImputacionCuali(x,"aleatorio"))
# A veces se cambia el tipo de factor a character al imputar, as? que hay que indicarle que es factor
input[,as.vector(which(sapply(input, class)=="character"))] <- lapply(input[,as.vector(which(sapply(input, class)=="character"))] , factor)

# Reviso que no queden datos missings
summary(input)

# Una vez finalizado este proceso, se puede considerar que los datos est?n depurados. Los guardamos
datosimputil <- cbind(varObjBin,varObjCont,input)



