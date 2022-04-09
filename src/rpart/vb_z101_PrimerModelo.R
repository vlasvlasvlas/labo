#Arbol elemental con libreria  rpart
#Debe tener instaladas las librerias  data.table  ,  rpart   y rpart.plot

#cargo las librerias que necesito
require("data.table")
require("rpart")
require("rpart.plot")

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


# #Aqui se debe poner la carpeta de SU computadora local
setwd("F:\\labimp_1\\")  #Establezco el Working Directory

# #cargo los datos de 202011 que es donde voy a ENTRENAR el modelo
dtrain  <- fread("datasets\\paquete_premium_202011.csv")



#genero el modelo,  aqui se construye el arbol
modelo  <- rpart("clase_ternaria ~ .",  
                 #quiero predecir clase_ternaria a partir de el resto de las variables
                 
                 #hyperparams
                 data = dtrain,

                 # xval      = -1, #esto significa no limitar la complejidad de los splits
                 # cp        = -1,
                 # minsplit  = 750, #minima cantidad de registros para que se haga el split
                 # minbucket = 375, #tamaño minimo de una hoja
                 # maxdepth  = 6

#
#cp	minsplit	minbucket	maxdepth	xval_folds
#-0.628273264	2279	937	13	5

#fecha	  cp 	minsplit	minbucket	maxdepth	xval_folds	 ganancia 	iteracion
#20220408 082113	-0.14 	1479	603	5	5	 10,833,000.00 	87


                 xval = -1,
                 cp= -0.03,
                 minsplit= 900,
                 minbucket=440,
                 maxdepth=5

#-1	8	1000	333	11018667

                 ) #profundidad maxima del arbol


#grafico el arbol
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)


#Ahora aplico al modelo  a los datos de 202101  y genero la salida para kaggle

#cargo los datos de 202011, que es donde voy a APLICAR el modelo
dapply  <- fread("datasets\\paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, dapply , type = "prob")

#prediccion es una matriz con TRES columnas, llamadas "BAJA+1", "BAJA+2"  y "CONTINUA"
#cada columna es el vector de probabilidades 

#agrego a dapply una columna nueva que es la probabilidad de BAJA+2
dapply[ , prob_baja2 := prediccion[, "BAJA+2"] ]

#solo le envio estimulo a los registros con probabilidad de BAJA+2 mayor  a  1/60
dapply[ , Predicted  := as.numeric(prob_baja2 > 1/60) ]

#genero un dataset con las dos columnas que me interesan
entrega  <- dapply[   , list(numero_de_cliente, Predicted) ] #genero la salida

#genero el archivo para Kaggle
#creo la carpeta donde va el experimento
dir.create( "labo\\exp\\" )
dir.create( "labo\\exp\\KA2001")

fwrite( entrega, 
        file= "labo\\exp\\KA2001/K101_016.csv", 
        sep= "," )
