#set workingdirectory
#seleccionar workingdirectory
setwd("C:/Users/user/Desktop/DataExploration")
#cargar las proyecciones de poblacion del 2017
dataPoblacionMunicipios<-read.csv(file="proyeccionesMunicipios2017.csv",sep=",")
colnames(dataPoblacionMunicipios)
sapply(dataPoblacionMunicipios, class)
head(dataPoblacionMunicipios)
tail(dataPoblacionMunicipios)
##Esto quita las tildes
dataPoblacionMunicipios$MPIO<-iconv(dataPoblacionMunicipios$MPIO,to="ASCII//TRANSLIT")
#Esto pone en mayuscula
dataPoblacionMunicipios$MPIO<-toupper(dataPoblacionMunicipios$MPIO)
#Esto quita la coma
dataPoblacionMunicipios$X2017<-gsub(",", '', dataPoblacionMunicipios$X2017)
#transformar la variable poblacion a numerica
dataPoblacionMunicipios$X2017<-as.numeric(dataPoblacionMunicipios$X2017)
sapply(dataPoblacionMunicipios, class)
##Esto quita el CD, espacios,(1) y (3) del nombre del municipio
dataPoblacionMunicipios$MPIO<-gsub("[:(:]CD[:):]|\\s|[:(:]1[:):]|[:(:]3[:):]", '', dataPoblacionMunicipios$MPIO)
#Eliminar registros inexistentes
dataPoblacionMunicipios <- dataPoblacionMunicipios[!apply(is.na(dataPoblacionMunicipios) | dataPoblacionMunicipios == "", 1, all),]
#cargar la informacion de hurto a personas de la sijin
dataHurtoPersonas<-read.csv(file="HurtoANSI.csv",sep=",")
head(dataHurtoPersonas)
colnames(dataHurtoPersonas)
summary(dataHurtoPersonas$Municipio)
sapply(dataHurtoPersonas, class)
##Esto quita puntos de los nombres de las columnas
colnames(dataHurtoPersonas) <- gsub("\\.", "", colnames(dataHurtoPersonas))
##Esto quita las tildes
colnames(dataHurtoPersonas)<-iconv(colnames(dataHurtoPersonas),to="ASCII//TRANSLIT")
##Esto quita el CT y espacios del nombre del municipio
dataHurtoPersonas$Municipio<-gsub("\\s|[:(:]CT[:):]", '', dataHurtoPersonas$Municipio)
##Esto quita las tildes
dataHurtoPersonas$Municipio<-iconv(dataHurtoPersonas$Municipio,to="ASCII//TRANSLIT")


#dataHurtoPersonas<-dataHurtoPersonas[,-c(dataHurtoPersonas$Codigo.DANE,dataHurtoPersonas$Cantidad)]
## este comando tumba R dataHurtoPersonas[ ,c(dataHurtoPersonas$Código.DANE,dataHurtoPersonas$Cantidad)] <- list(NULL)
## Tambien me desconecto de la red aunque no estoy seguro de como

#Esto elimina multiples columnas con nombre
dataHurtoPersonas[ ,c('Codigo.DANE','Cantidad')] <- list(NULL)
summary(dataHurtoPersonas$Barrio)
summary(dataHurtoPersonas$Municipio)
library(dplyr)
?select
attach(dataHurtoPersonas)
dataHurtoPersonas[1,]

a <- table(dataHurtoPersonas$Municipio)
a[names(a)=="MEDELLiN (CT)"]

?gsub

Municipios<-dataHurtoPersonas %>% select(Municipio)  %>%  filter(Municipio == "MEDELLiN")

summary(Municipios)
detach(dataHurtoPersonas)
dataCaracteristicasGenerales<-read.csv(file="CaracteristicasGenerales.txt",sep=" ")
head(dataCaracteristicasGenerales)
colnames(dataCaracteristicasGenerales)
summary(dataCaracteristicasGenerales$SECUENCIA_ENCUESTA)
sapply(dataCaracteristicasGenerales, class)

dataVivienda<-read.csv(file="DatosVivienda.txt",sep=" ")
head(dataVivienda)
colnames(dataVivienda)
summary(dataVivienda$SECUENCIA_ENCUESTA)
sapply(dataVivienda, class)
?merge
datacompleta<-merge(dataVivienda,dataCaracteristicasGenerales)
datacompleta<-merge(dataVivienda,dataCaracteristicasGenerales,by= intersect(dataCaracteristicasGenerales$SECUENCIA_ENCUESTA,dataVivienda$SECUENCIA_ENCUESTA))
datacompleta<-merge(dataVivienda,dataCaracteristicasGenerales,by.x = 'SECUENCIA_ENCUESTA',by.y = 'SECUENCIA_ENCUESTA')
