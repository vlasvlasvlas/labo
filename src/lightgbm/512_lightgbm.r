# LightGBM  cambiando algunos de los parametros

#limpio la memoria
rm( list=ls() )  #remove all objects
gc()             #garbage collection

require("data.table")
require("lightgbm")

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

#Aqui se debe poner la carpeta de la computadora local
setwd("F:\\labimp_1\\")   #Establezco el Working Directory

#cargo el dataset donde voy a entrenar
dataset  <- fread("datasets\\paquete_premium_202011.csv", stringsAsFactors= TRUE)


#paso la clase a binaria que tome valores {0,1}  enteros
dataset[ , clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]

#los campos que se van a utilizar
campos_buenos  <- setdiff( colnames(dataset), c("clase_ternaria","clase01") )


#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )

#genero el modelo con los parametros por default
modelo  <- lgb.train( data= dtrain,
                      param= list( objective= "binary",
                                   metric= "custom",
                                   first_metric_only= TRUE,
                                   boost_from_average= TRUE,
                                   feature_pre_filter= FALSE,
                                   verbosity= -100,
                                   seed= 825733,
                                   max_depth=  -1,
                                   min_gain_to_split= 0.0,
                                   lambda_l1= 0.0,
                                   lambda_l2= 0.0,
                                   max_bin= 31,
                                   num_iterations= 9999,
                                   force_row_wise= TRUE )
                    )

#aplico el modelo a los datos sin clase
dapply  <- fread("datasets\\paquete_premium_202101.csv")

#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ]) )


#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                 "Predicted"= prediccion > 1/60)  ) #genero la salida

dir.create( "labo\\exp\\",  showWarnings = FALSE ) 
dir.create( "labo\\exp\\KA2512/", showWarnings = FALSE )
archivo_salida  <- "labo\\exp\\KA2512/KA_512_002.csv"

#genero el archivo para Kaggle
fwrite( entrega, 
        file= archivo_salida, 
        sep= "," )


#ahora imprimo la importancia de variables
tb_importancia  <-  as.data.table( lgb.importance(modelo) ) 
archivo_importancia  <- "./labo/exp/KA2512/512_importancia_001.txt"

fwrite( tb_importancia, 
        file= archivo_importancia, 
        sep= "\t" )

