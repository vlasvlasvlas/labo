# XGBoost  sabor tradicional
# ensemble de 400 arboles de APENAS altura 1  (decision stumps), la raiz y dos hojas

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("xgboost")

#Aqui se debe poner la carpeta de la computadora local
setwd("F:\\labimp_1\\")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
dataset  <- fread("datasets\\paquete_premium_202011.csv", stringsAsFactors= TRUE)

# agrego columnas

#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )


#dejo los datos en el formato que necesita XGBoost
dtrain  <- xgb.DMatrix( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )

#genero el modelo con los parametros por default
modelo  <- xgb.train( data= dtrain,
                      param= list( 
                                   objective=       "binary:logistic",
                                   max_depth=           1,    #arboles de altura 1, solo dos hojas ! si lo cambio empeora
                                   min_child_weight=   12,
                                   
                                   #test (probando otros parametros de internet, falta revisar, podriamos hacer una búsqueda optimización de estos params para xgboost?)
								   # busar mas opciones
                                   tree_method = "hist",
                                   grow_policy="lossguide",
                                   learning_rate = 0.05, 
                                   top_rate = 0.5, 
                                   other_rate = 0.1, 
                                   feature_fraction_bynode = 0.2, 
                                   boosting_type = "goss",
                                   eta=0.03
                                   
                    ),
                                   nrounds= 2500 #se subio el valor de nrounds
                    )

#aplico el modelo a los datos sin clase
dapply  <- fread("datasets\\paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ]) )


#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted" = prediccion ))

setorder(entrega, -Predicted)
entrega[, Predicted := 0]
entrega[1:11500, Predicted := 1]

dir.create( "labo\\exp\\",  showWarnings = FALSE ) 
dir.create( "labo\\exp\\KA2553/", showWarnings = FALSE )
archivo_salida  <- "labo\\exp\\KA2553/KA_553_ok_03_predicted_11500.csv"

#genero el archivo para Kaggle
fwrite( entrega, 
        file= archivo_salida, 
        sep= "," )
