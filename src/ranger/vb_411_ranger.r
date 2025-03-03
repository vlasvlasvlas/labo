#Se utiliza el algoritmo Random Forest, creado por Leo Breiman en el año 2001
#Una libreria que implementa Rando Forest se llama  ranger
#La libreria esta implementada en lenguaje C y corre en paralelo, utiliza TODOS los nucleos del procesador
#Leo Breiman provenia de la estadistica y tenia "horror a los nulos", con lo cual el algoritmo necesita imputar nulos antes




#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require("data.table")
require("ranger")
require("randomForest")  #solo se usa para imputar nulos

#Aqui se debe poner la carpeta de la computadora local
setwd("F:\\labimp_1\\")   #Establezco el Working Directory

#cargo los datos donde entreno
dtrain  <- fread("datasets\\paquete_premium_202011.csv", stringsAsFactors= TRUE)

#imputo los nulos, ya que ranger no acepta nulos
#Leo Breiman, �por que le temias a los nulos?
dtrain  <- na.roughfix( dtrain )

#cargo los datos donde aplico el modelo
dapply  <- fread("datasets\\paquete_premium_202101.csv", stringsAsFactors= TRUE)
dapply[ , clase_ternaria := NULL ]  #Elimino esta columna que esta toda en NA
dapply  <- na.roughfix( dapply )  #tambien imputo los nulos en los datos donde voy a aplicar el modelo

#genero el modelo de Random Forest con la libreria ranger
#notar como la suma de muchos arboles contrarresta el efecto de min.node.size=1

# promediando las hojas de los arboles
param  <- list( "num.trees"=      2600,  # cantidad de arboles
                "mtry"=             20,  # cantidad de variables que evalua para hacer un split  sqrt(ncol(dtrain))
                "min.node.size"=   354,  # tamano minimo de las hojas
                "max.depth"=        15   # 0 significa profundidad infinita
              )

# promediando las hojas de los arbolesparam  <- list( "num.trees"=       300,  # cantidad de arboles                "mtry"=             30,  # cantidad de variables que evalua para hacer un split  sqrt(ncol(dtrain))                "min.node.size"=   900,  # tamaño minimo de las hojas                "max.depth"=        10

# primos
# 295873
# 527173
# 328789
# 825733
# 191519
# 341963
# 590771
# 765103
# 402263
# 584707

set.seed(295873) #Establezco la semilla aleatoria

#para preparar la posibilidad de asignar pesos a las clases
#la teoria de  Maite San Martin
setorder( dtrain, clase_ternaria )  #primero quedan los BAJA+1, BAJA+2, CONTINUA

#genero el modelo de Random Forest llamando a ranger()
modelo  <- ranger( formula= "clase_ternaria ~ .",
                   data=  dtrain, 
                   probability=   TRUE,  #para que devuelva las probabilidades
                   num.trees=     param$num.trees,
                   mtry=          param$mtry,
                   min.node.size= param$min.node.size,
                   max.depth=     param$max.depth
                   #,class.weights= c( 1,60, 1)  #siguiendo con la idea de Maite San Martin
                 )

#aplico el modelo recien creado a los datos del futuro
prediccion  <- predict( modelo, dapply )

#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= as.numeric(prediccion$predictions[ ,"BAJA+2" ] > 1/60) ) ) #genero la salida

#creo la carpeta donde va el experimento
# HT  representa  Hiperparameter Tuning
dir.create( "labo\\exp\\",  showWarnings = FALSE ) 
dir.create( "labo\\exp\\KA2411\\", showWarnings = FALSE )

archivo_salida  <- "labo\\exp\\KA2411\\KA_411_004.csv"

#genero el archivo para Kaggle
fwrite( entrega, 
        file= archivo_salida, 
        sep="," )
