#limpio la memoria
rm( list=ls() )
gc()

library("data.table")
library("rpart")
library("rpart.plot")

setwd( "F:\\labimp_1\\" )  #establezco la carpeta donde voy a trabajar

#cargo el dataset
dataset  <- fread( "datasets\\paquete_premium_202011.csv")


#uso esta semilla para los canaritos
set.seed(825733)

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

#agrego una variable canarito, random distribucion uniforme en el intervalo [0,1]
dataset[ ,  canarito1 :=  runif( nrow(dataset) ) ]

#agrego los siguientes canaritos
for( i in 1:30 ) dataset[ , paste0("canarito", i ) :=  runif( nrow(dataset)) ]


#Primero  veo como quedan mis arboles
modelo  <- rpart(formula= "clase_ternaria ~ . ",
                 data= dataset[,],
                 model= TRUE,
                 xval= 0,
                 cp= 0, 
                 minsplit= 10, 
                 maxdepth= 10)

#creo la carepta donde guardo el resultado
dir.create( "labo\\exp\\",  showWarnings = FALSE ) 
dir.create( "labo\\exp\\ST5210\\", showWarnings = FALSE )
setwd("F:\\labimp_1\\labo\\exp\\ST5210\\")   #Establezco el Working Directory DEL EXPERIMENTO

#genero la imagen del arbol
pdf( file= "arbol_canaritos.pdf", width=20, height=4)
prp(modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0)
dev.off()
