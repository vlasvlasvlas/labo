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

#carga sets set y apply
dataset  <- fread("datasets\\paquete_premium_202011.csv", stringsAsFactors= TRUE)
dapply  <- fread("datasets\\paquete_premium_202101.csv")



#paso la clase a binaria que tome valores {0,1}  enteros

# prueba zulip
# rpart { SI, NO }
# Random Forest { SI, NO }
# xgboost, lightgbm { 0, 1 }

#dataset[ , clase01 := ifelse( clase_ternaria=="BAJA+2", 1L, 0L) ]
dataset[ , clase01:= ifelse( clase_ternaria=="CONTINUA", 0L, 1L ) ]


# los campos que se van a utilizar
# #https://en.wikipedia.org/wiki/Concept_drift

campos_malos  <- c(
                   "foto_mes"
                   )

campos_buenos  <- setdiff( colnames(dataset), 
                           c("clase_ternaria","clase01",campos_malos)
                           )



#dejo los datos en el formato que necesita LightGBM
dtrain  <- lgb.Dataset( data= data.matrix(  dataset[ , campos_buenos, with=FALSE]),
                        label= dataset$clase01 )



# params

params_probar  <- list( 

                       # Default:
                        objective= "binary",
                        metric= "custom",
                        first_metric_only= TRUE,
                        boost_from_average= TRUE,
                        feature_pre_filter= FALSE,
                        verbosity= -100,
                        max_depth=  -1,
                        min_gain_to_split= 0.0,
                        lambda_l1= 0.0,
                        lambda_l2= 0.0,
                        max_bin= 31,
                        num_iterations= 609,
                       force_row_wise= TRUE,
                       

                       # BO_nube
                       objective= "binary",
                       seed= 999983,
                       max_bin= 31,      
                       learning_rate= 0.010144836, 
                       feature_fraction=0.560971132, 
                       min_data_in_leaf=2709,
                       num_leaves=567

                                              
)


#genero el modelo con los parametros por default
modelo  <- lgb.train( data= dtrain,
                      param= params_probar
                      )
                      
                    



#aplico el modelo a los datos nuevos
prediccion  <- predict( modelo, 
                        data.matrix( dapply[, campos_buenos, with=FALSE ])                                 )


#Genero la entrega para Kaggle
entrega  <- as.data.table( list( "numero_de_cliente"= dapply[  , numero_de_cliente],
                                    #"Predicted" = prediccion > 1/60)
                                    #"Predicted" = prediccion > 1/30)
                                 
                                     "Predicted" = prediccion > 0.016106591 # 0.023 #mean(c(1/60, 1/50, 1/40, 1/30) )
                                 
                                                  # mean(c(1/60,1/30)
                                                  # 1/30 siguiendo zulip seria un numero entre esos dos
                                                  # probando con1/30 sino con mean de ambos
                                                  # 0.025?
                                 
                                                  ) #list
                                                  ) #genero la salida

dir.create( "labo\\exp\\",  showWarnings = FALSE ) 
dir.create( "labo\\exp\\KA2512/", showWarnings = FALSE )
archivo_salida  <- "labo\\exp\\KA2512/KA_512_038_el38estacargado.csv"

#genero el archivo para Kaggle
fwrite( entrega, 
        file= archivo_salida, 
        sep= "," )
