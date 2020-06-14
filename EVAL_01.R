#### Evaluacion 1 #### Domingo 06:00 a.m.
# Victor Raúl Lazo Pampa : Diciembre (2018-2019)

# La data proviene de:
# https://www.osinergmin.gob.pe/seccion/institucional/regulacion-tarifaria/publicaciones/regulacion-tarifaria

# Recordar que las variables son:
# Código de la Empresa Eléctrica Suministradora	COD_EMPRESA
# Código del Suministro del Usuario Libre				COD_SUMINISTRO_USUARIO
# Código del Punto de Suministro						    COD_PUNTO_SUMINISTRO
# Fecha (AAAAMMDDHHMM)								        	FECHA
# Fecha (AAAAMMDDHHMM)								        	FECHADATE
# Registro de Energía Activa en kW.h					  ENERG_ACTV
# Registro de Energía Reactiva en kVarh					ENERG_REAC
# Periodo												                PERIODO
# Año												      	            AÑO
# Mes													                  MES
# Dia													                  DIA
# Nombre del dia                                NAM_DIA

getwd()
setwd(dir = "c:/Users/Victor Lazo/Desktop/R4DS/04 Class/Evaluacion/")
rm(list = ls())
dir()

#### Diciembre 2018 ####
OsinergDIC2018 <- read.table("201812_TABLA04_SICLI.txt", header = TRUE,sep = "\t",
                             col.names = c("COD_EMPRESA","COD_SUMINISTRO_USUARIO", "COD_PUNTO_SUMINISTRO","FECHA","ENERG_ACTV","ENERG_REAC","PERIODO"),
                             colClasses = c("factor","factor","factor","character","numeric", "numeric", "character"))

#COD_EMPRESA COD_SUMINISTRO_USUARIO COD_PUNTO_SUMINISTRO FECHA ENERG_ACTV ENERG_REAC PERIODO AÑO MES DIA HORA MINUTO
class(OsinergDIC2018)
str(OsinergDIC2018)
dim(OsinergDIC2018)

### Conversion de la variable fecha ###
OsinergDIC2018$FECHA

# Creamos la variable FECHADATE para recuperar la cadena que se tiene en el
# data frame original
# Instalamos la libraria lubridade
library(lubridate)
OsinergDIC2018$FECHADATE <- ymd_hm(OsinergDIC2018$FECHA)
class(OsinergDIC2018$FECHADATE)

# Usando libreria base
OsinergDIC2018$AÑO <- format(OsinergDIC2018$FECHADATE, "%Y")
OsinergDIC2018$MES<- format(OsinergDIC2018$FECHADATE, "%m")
OsinergDIC2018$DIA <- format(OsinergDIC2018$FECHADATE, "%d")
OsinergDIC2018$HORA <- format(OsinergDIC2018$FECHADATE, "%H")
OsinergDIC2018$MINUTO <- format(OsinergDIC2018$FECHADATE, "%M")
# Obtener nombre del dia NOM_DIA
OsinergDIC2018$NOM_DIA <- wday(OsinergDIC2018$FECHADATE,label = TRUE)
OsinergDIC2018$NOM_DIA <- as.character.factor(OsinergDIC2018$NOM_DIA)

#Eliminando las filas con NA en algun campo de en la fila
OsinergDIC2018 <- na.omit(OsinergDIC2018)

str(OsinergDIC2018)

#### Diciembre 2019 ####

OsinergDIC2019 <- read.table("201912_TABLA4.txt", header = TRUE,sep = "\t",
                             col.names = c("AÑO","MES","COD_EMPRESA","COD_PUNTO_SUMINISTRO","COD_SUMINISTRO_USUARIO", "ENERG_ACTV","ENERG_REAC","FECHA"),
                             colClasses = c("character","character","factor","factor","factor","character", "character", "character"))

#COD_EMPRESA COD_SUMINISTRO_USUARIO COD_PUNTO_SUMINISTRO FECHA ENERG_ACTV ENERG_REAC PERIODO AÑO MES DIA HORA MINUTO

# Verificamos la clase de cada una de las variables (columnas)
lapply(OsinergDIC2019, class)

# Cambiamos las comas (decimales) por puntos decimales mediante la creacion de nuevas columnas
# Estas columas nuevas son la representacion numerica (con punto decimal) de las columnas ENERG_ACTV y ENERG_REAC
OsinergDIC2019$ENERG_ACTV_Numeric <- as.numeric(sub(",", ".", OsinergDIC2019$ENERG_ACTV, fixed = TRUE))
OsinergDIC2019$ENERG_REAC_Numeric <- as.numeric(sub(",", ".", OsinergDIC2019$ENERG_REAC, fixed = TRUE))

# Verificamos la clase de cada una de las columnas (variables)
lapply(OsinergDIC2019, class)

# Eliminamos dos variables que ya o sirven en el formato incial
OsinergDIC2019 <- OsinergDIC2019[, -c(6,7)]

# la cantidad de caracteres que forman la cadena de caracteres FECHA
nchar(OsinergDIC2019$FECHA[1])
# Esta funcion nos dice que cada elemento de la columna FECHA es una cadena
# de caracteres con 23 elementos. 

# Con la informacion anterior, procedemos a elegir las porciones adeciadas para
# definir nuestras variables de interes
OsinergDIC2019$PERIODO <- substr(x = OsinergDIC2019$FECHA,start = 1,stop = 7)
# Eliminamos el "-" en PERIODO
OsinergDIC2019$PERIODO <- gsub("-","",OsinergDIC2019$PERIODO)

OsinergDIC2019$DIA<- substr(x = OsinergDIC2019$FECHA,start = 9,stop = 10)
OsinergDIC2019$HORA<- substr(x = OsinergDIC2019$FECHA,start = 12,stop = 13)
OsinergDIC2019$MINUTO <- substr(x = OsinergDIC2019$FECHA,start = 15,stop = 16)
# Creando la variable fecha FECHA DATE
OsinergDIC2019$FECHADATE <- substr(x = OsinergDIC2019$FECHA,start = 1,stop = 19)

# Obtener nombre del dia NOM_DIA
OsinergDIC2019$NOM_DIA <- wday(OsinergDIC2019$FECHADATE,label = TRUE)
OsinergDIC2018$NOM_DIA <- as.character.factor(OsinergDIC2018$NOM_DIA)

#Eliminando las filas con NA en algun campo de en la fila
OsinergDIC2019 <- na.omit(OsinergDIC2019)

# Visualizando la data resultante y visualizando algunas variables
View(OsinergDIC2018)
View(OsinergDIC2019)
str(OsinergDIC2018$COD_EMPRESA)
str(OsinergDIC2019$COD_EMPRESA)

# Resumen de los datos ENERG_ACTV y ENERG_REACT
summary(OsinergDIC2018$ENERG_ACTV)
summary(OsinergDIC2018$ENERG_REAC)
summary(OsinergDIC2019$ENERG_ACTV)
summary(OsinergDIC2019$ENERG_REAC)

class(OsinergDIC2018)

