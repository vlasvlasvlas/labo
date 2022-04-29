#Intento de Solucion del desafio  15k
#que NO logra solucionarlo, una que falta una idea fundamental, una chispa, un Momento Eureka
#pero crea estructura sobre la cual trabajar

#limpio la memoria
rm( list=ls() )
gc()

options(digits = 10) # fix

require("data.table")

ftirar  <- function( prob, qty )
{
  return( sum( runif(qty) < prob ) )
}


#variables globales que usan las funciones gimnasio_xxxx
GLOBAL_jugadores  <- c()
GLOBAL_tiros_total  <- 0

#Crea el juego
#a cada jugador se le pone un numero de 1 a 100 en la espalda
#debajo de ese numero esta el indice_de_enceste  que NO puede ser visto por el cazatalentos
gimnasio_init  <- function() 
{
  GLOBAL_jugadores  <<-  sample( c( (501:599 )/1000 , 0.7 ) )
  GLOBAL_tiros_total  <<- 0
}


#se le pasa un vector con los IDs de los jugadores y la cantidad de tiros a realizar
#devuelve en un vector cuantos aciertos tuvo cada jugador
gimnasio_tirar  <- function(  pids,  pcantidad )
{
  GLOBAL_tiros_total  <<-  GLOBAL_tiros_total + length( pids )*pcantidad
  res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )

  return( res )
}



#El cazatalentos decide a que jugador llevarse
#devuelve la cantidad de tiros libres y si le acerto al verdadero_mejor o no
gimnasio_veredicto  <- function( pid )
{
  return( list("tiros_total"= GLOBAL_tiros_total, 
               "acierto"=     as.integer( GLOBAL_jugadores[pid]==0.7) ))
}









#------------------------------------------------------------------------------



# Idea General de la propuesta (basada en skeleton de intento B):


# La propuesta es:
# - arrancar con el total de jugadores (100)
# - ir "achicando" la cantidad de jugadores en cada ronda:
#   - dejando a los mejores (arriba de la mediana) <- probar de ir jugando con el "umbral" de mediana * 1.00n
#   - y al ser menos jugadores, podemos sumar más tiradas por jugador (darles más tiradas a los mejores jugadores que vayan quedando de ronda en ronda)
#   - por cada ronda una idea era quedame con el porcentaje de aciertos (resultado / tiros)
#   - en cada vuelta, se acumulan los porcentajes de aciertos
#   - esto va a darme que: el BUENO va a ser siempre bueno (como Michael Jordan) y el regular, va a poder ser bueno en algunos %, y malo en otros %, cuando sumamos el total de estos, va a "sobresalir" el bueno y va el regular va a estar con el corazón dolido, pero al mismo tiempo va a esforzarse para ser mejor el próximo año.





# TO-DO:
# Podría "automatizarse" esto, creo, por ej:
#   - podría hacerse una funcion ronda() que la estrategía "itere" en rondas, y que "ejecute" una nueva ronda, siempre y cuando sea permitido (todavía tenemos tiros para tirar? tenemos más de un jugador?, buscar las reglas necesarias y los parámetros de entrada necesarios para "iterar" en la función de Rondas.






# pruebas realizadas

# median_ronda2 <- 1.000
# median_ronda3 <- 1.000
# median_ronda4 <- 1.000
# median_ronda5 <- 1.000
# median_ronda6 <- 1.000

# tirada_inicial <- 40
# suma_tiradas <- 5
# > cat("Tiros total y tasa eleccion correcta:")
# Tiros total y tasa eleccion correcta:> tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
# > tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]
# > tiros_total 
# [1] 12280
# > tasa_eleccion_correcta
# [1] 0.9646
# > cat("\n")


# median_ronda2 <- 1.001 #plus por encima de la mediana
# median_ronda3 <- 1.000
# median_ronda4 <- 1.000
# median_ronda5 <- 1.000
# median_ronda6 <- 1.000

# tirada_inicial <- 44
# suma_tiradas <- 7
# > cat("Tiros total y tasa eleccion correcta:")
# Tiros total y tasa eleccion correcta:> tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
# > tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]
# > tiros_total 
# [1] 11823
# > tasa_eleccion_correcta
# [1] 0.9656
# > cat("\n")





# median_ronda2 <- 1.002 #plus por encima de la mediana
# median_ronda3 <- 1.002 #plus por encima de la mediana
# median_ronda4 <- 1.002 #plus por encima de la mediana
# median_ronda5 <- 1.002 #plus por encima de la mediana
# median_ronda6 <- 1.000

# tirada_inicial <- 50
# suma_tiradas <- 15
# > cat("Tiros total y tasa eleccion correcta:")
# Tiros total y tasa eleccion correcta:> tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
# > tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]
# > tiros_total 
# [1] 13155
# > tasa_eleccion_correcta
# [1] 0.974
# > cat("\n")





# median_ronda2 <- 1.0040 #plus por encima de la mediana
# median_ronda3 <- 1.0035 #plus por encima de la mediana
# median_ronda4 <- 1.0030 #plus por encima de la mediana
# median_ronda5 <- 1.0025 #plus por encima de la mediana
# median_ronda6 <- 1.0020 #plus por encima de la mediana

# tirada_inicial <- 55
# suma_tiradas <- 22
# > cat("Tiros total y tasa eleccion correcta:")
# Tiros total y tasa eleccion correcta:> tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
# > tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]
# > tiros_total 
# [1] 14817
# > tasa_eleccion_correcta
# [1] 0.9882648785
# > cat("\n")





# median_ronda2 <- 1.0040 #plus por encima de la mediana
# median_ronda3 <- 1.0035 #plus por encima de la mediana
# median_ronda4 <- 1.0030 #plus por encima de la mediana
# median_ronda5 <- 1.0025 #plus por encima de la mediana
# median_ronda6 <- 1.0020 #plus por encima de la mediana
# 
# tirada_inicial <- 55
# suma_tiradas <- 21
# > tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]
# > tiros_total 
# [1] 14455
# > tasa_eleccion_correcta
# [1] 0.981
# > cat("\n")




median_ronda2 <- 1.0040 #plus por encima de la mediana
median_ronda3 <- 1.0035 #plus por encima de la mediana
median_ronda4 <- 1.0030 #plus por encima de la mediana
median_ronda5 <- 1.0025 #plus por encima de la mediana
median_ronda6 <- 1.0020 #plus por encima de la mediana
tirada_inicial <- 55
suma_tiradas <- 22
# > tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]
# > tiros_total 
# [1] 14630
# > tasa_eleccion_correcta
# [1] 0.983
# > cat("\n")



# --------------------------------
# check cantidades en ultima ronda
# esta idea era sumar los count()length por ronda, para tener una idea de cuanto achicaba en %, pero no lo suma, revisandolo.
GLOBAL_control_ronda2  <- 0
GLOBAL_control_ronda3  <- 0
GLOBAL_control_ronda4  <- 0
GLOBAL_control_ronda5  <- 0
GLOBAL_control_ronda6  <- 0



# estuve probando con experimentos más chicos para comparar resultados.

cant_experimentos <- 1  #si tengo un sweet spot entre tiradas y %, lo agrando a 10k 




Estrategia_VB001  <- function()
  
  
{
  
  
  
  
  #Estrategia original del script intento
  #Se juegan varias rondas
  #En cada ronda, los jugadores que participan, tiran 70 tiros
  #De una ronda a la otra, solo pasan los que tuvieron igual o mayor aciertos a la mediana de aciertos de la ronda anterior
  
  # Objetivo: elegir al mejor jugador de la 6ta ronda
  
  # Modificado: ajustar la cantidad de tiros por ronda.
  # Modificado: se ajusta una mediana modificada sumandole un %
  # TO-DO: probar con buckets y tomando los n buckets superiores
  

  gimnasio_init()

  #Esta el la planilla del cazatalentos
  #el id es el numero que tiene en la espalda cada jugador
  planilla_cazatalentos  <- data.table( "id"= 1:100 )


  
  
  
    
  # ------------------------------------------------------
  #Ronda 1: 
  #Juegan todos
  #tiran los 100 jugadores es decir 1:100   40 tiros libres cada uno
  
  ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,
  var_tiros_r1 <- tirada_inicial # en primera ronda les pedimos 40 tiros por jugador
  
  
  
  
  # registros:  
  planilla_cazatalentos[ ids_juegan1,  tiros1 := var_tiros_r1 ]  #registro en la planilla los tiros
  resultado1  <- gimnasio_tirar( ids_juegan1, var_tiros_r1) #cantidad de tiros 1

  planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla la cantidad de aciertos 
  
  planilla_cazatalentos[ ids_juegan1, 
                         aciertos1 := resultado1
                         ]
  
  #agrego porcentaje de aciertos (resultado / tiros) por separado
  planilla_cazatalentos[ ids_juegan1, 
                         p_res_tiros1 := resultado1 / var_tiros_r1 
  ]
  


  
  #
  #
  # TO-DO:
  # Armar una función de "rondas siguientes"! para no hardcodear cada ronda siguiente.
  #
  #
  
  
  
  
  #-------------------------------------------------------------
  #Ronda 2:
  #Pasa la mitad mejor


  # nos quedamos con los jugadores que pasan la mediana del % de p_res_tiros1:  
  mediana  <- planilla_cazatalentos[ ids_juegan1, (median(p_res_tiros1,na.rm = FALSE)*median_ronda2) ] # fix sobre p_res_tiros1
  
  
  # nuestros nuevos jugadores estan en ids_juegan2
  ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][ p_res_tiros1 > mediana, id ] #fix sobre p_res_tiros1
  GLOBAL_control_ronda2 <- GLOBAL_control_ronda2+length(ids_juegan2)
  
  
  # probamos con nueva variable de tiros2
  var_tiros_r2 <- var_tiros_r1 + suma_tiradas # sumamos las tiradas de rondas
  
  
  # registros:
  planilla_cazatalentos[ ids_juegan2,  tiros2 := var_tiros_r2 ]  #registro en la planilla la cantidad de tiros
  resultado2  <- gimnasio_tirar( ids_juegan2, var_tiros_r2) 
  

  
  # cargamos planilla con resultados
  planilla_cazatalentos[ ids_juegan2, 
                         aciertos2 := resultado2 #registro en la planilla la cantidad de aciertos
                       ]
  
  # cargamos planilla con resultados porcent
  planilla_cazatalentos[ ids_juegan2, 
                         # denuevo probamos con % nose si funcionara:
                         # SOLO le sumamos a los jugadores que tenemos filtrados en id_juegan
                         # NUEVO porcent:
                         # (aciertos1 + aciertos2) / (var_tiros_r1 + var_tiros_r2)
                         p_res_tiros2 := ( 
                           resultado2 + 
                           planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan2]$aciertos1
                           )
                         /
                           (var_tiros_r1+
                            var_tiros_r2)
                         
                         ] 

  
  
  

  

  
  
  
  
  
  
  #-------------------------------------------------------------
  #Ronda 3: 
  #A la mitad mejor la hago tirar

  
  # nos quedamos con los jugadores que pasan la mediana del % de p_res_tiros2:  
  mediana  <- planilla_cazatalentos[ ids_juegan2, (median(p_res_tiros2,na.rm = FALSE)*median_ronda3) ] #fix sobre porcent tiros 

  # nuestros nuevos jugadores estan en ids_juegan3
  ids_juegan3  <- planilla_cazatalentos[ ids_juegan2 ][ p_res_tiros2 > mediana, id ]
  GLOBAL_control_ronda3 <- GLOBAL_control_ronda3+length(ids_juegan3)
  
  
  # probamos con nueva variable de tiros3
  var_tiros_r3 <- var_tiros_r2 + suma_tiradas # sumamos las tiradas de rondas
  
  
  # registros
  planilla_cazatalentos[ ids_juegan3,  tiros3 := var_tiros_r3 ]  #registro en la planilla la cantidad de tiros
  resultado3  <- gimnasio_tirar( ids_juegan3, var_tiros_r3)
  
  # sumamos el cantidad de aciertos
  planilla_cazatalentos[ ids_juegan3,  aciertos3 := resultado3 ] #registro en la planilla la cantidad de aciertos
  
  # denuevo probamos con % nose si funcionara:
  # SOLO le sumamos a los jugadores que tenemos filtrados en id_juegan
  # NUEVO porcent:
  # (aciertos1 + aciertos2 + aciertos3) / (var_tiros_r1 + var_tiros_r2 + var_tiros_r3)
  
  planilla_cazatalentos[ ids_juegan3,  
                         p_res_tiros3 := 
                           (  resultado3 +
                              planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan3]$aciertos1+
                              planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan3]$aciertos2
                           )
                           /
                           (var_tiros_r1+
                            var_tiros_r2+
                            var_tiros_r3)
                        ] 
  

  
  
  
  

  
  
  #-------------------------------------------------------------
  #Ronda 4: 
  #A la mitad mejor la hago tirar

  
  # nos quedamos con los jugadores que pasan la mediana del % de p_res_tiros2:  
  mediana  <- planilla_cazatalentos[ ids_juegan3, (median(p_res_tiros3,na.rm = FALSE)*median_ronda4) ] #fix sobre porcent tiros 
  
  # nuestros nuevos jugadores estan en ids_juegan4
  ids_juegan4  <- planilla_cazatalentos[ ids_juegan3 ][ p_res_tiros3 > mediana, id ]
  GLOBAL_control_ronda4 <- GLOBAL_control_ronda4+length(ids_juegan4)
  
  # probamos con nueva variable de tiros4
  var_tiros_r4 <- var_tiros_r3 + suma_tiradas # sumamos las tiradas de rondas
  
  
  planilla_cazatalentos[ ids_juegan4,  tiros4 := var_tiros_r4 ]  #registro en la planilla la cantidad de tiros
  resultado4  <- gimnasio_tirar( ids_juegan4, var_tiros_r4)
  
  # sumamos el cantidad de aciertos
  planilla_cazatalentos[ ids_juegan4,  aciertos4 := resultado4 ] #registro en la planilla la cantidad de aciertos
  
  # denuevo probamos con % nose si funcionara:
  # SOLO le sumamos a los jugadores que tenemos filtrados en id_juegan
  # NUEVO porcent:
  # (aciertos1 + aciertos2 + aciertos3 + aciertos4) / (var_tiros_r1 + var_tiros_r2 + var_tiros_r3 + var_tiros_r4)
  
  planilla_cazatalentos[ ids_juegan4,  
                         p_res_tiros4 := 
                           (  resultado4 +
                                planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan4]$aciertos1+
                                planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan4]$aciertos2+
                                planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan4]$aciertos3
                           )
                         /
                           (var_tiros_r1+
                            var_tiros_r2+
                            var_tiros_r3+
                            var_tiros_r3)
  ] 
  
  
  

  
  
  
  
  
  
  
  
  
  #-------------------------------------------------------------
  #Ronda 5: subiendo de a 10
  #A la mitad mejor la hago tirar

  
  # nos quedamos con los jugadores que pasan la mediana del % de p_res_tiros2:  
  mediana  <- planilla_cazatalentos[ ids_juegan4, (median(p_res_tiros4,na.rm = FALSE)*median_ronda5) ] #fix sobre porcent tiros
  
  # nuestros nuevos jugadores estan en ids_juegan5
  ids_juegan5  <- planilla_cazatalentos[ ids_juegan4 ][ p_res_tiros4 > mediana, id ]
  GLOBAL_control_ronda5 <- GLOBAL_control_ronda5+length(ids_juegan5)
  
  
  # probamos con nueva variable de tiros5
  var_tiros_r5 <- var_tiros_r4 + suma_tiradas # sumamos las tiradas de rondas
  
  
  planilla_cazatalentos[ ids_juegan5,  tiros5 := var_tiros_r5 ]  #registro en la planilla la cantidad de tiros
  resultado5  <- gimnasio_tirar( ids_juegan5, var_tiros_r5)
  
  # sumamos el cantidad de aciertos
  planilla_cazatalentos[ ids_juegan5,  aciertos5 := resultado5 ]  #registro en la planilla la cantidad de aciertos
  
  # denuevo probamos con % nose si funcionara:
  # SOLO le sumamos a los jugadores que tenemos filtrados en id_juegan
  # NUEVO porcent:
  # (aciertos1 + aciertos2 + aciertos3 + aciertos4 + aciertos5) / (var_tiros_r1 + var_tiros_r2 + var_tiros_r3 + var_tiros_r4 + var_tiros_r5)
  
  planilla_cazatalentos[ ids_juegan5,  
                         p_res_tiros5 := 
                           (  resultado5 +
                                planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan5]$aciertos1+
                                planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan5]$aciertos2+
                                planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan5]$aciertos3+
                                planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan5]$aciertos4
                           )
                         /
                           (var_tiros_r1+
                              var_tiros_r2+
                              var_tiros_r3+
                              var_tiros_r3+
                              var_tiros_r4)
  ] 
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #-------------------------------------------------------------
  #Ronda 6: 
  #A la mitad mejor la hago tirar 

  
  # nos quedamos con los jugadores que pasan la mediana del % de p_res_tiros2:  
  mediana  <- planilla_cazatalentos[ ids_juegan5, (median(p_res_tiros5,na.rm = FALSE)*median_ronda6) ] #fix sobre porcent tiros
  
  # nuestros nuevos jugadores estan en ids_juegan6 
  ids_juegan6  <- planilla_cazatalentos[ ids_juegan5 ][ p_res_tiros5 > mediana, id ]
  
  
  GLOBAL_control_ronda6 <- GLOBAL_control_ronda6+length(ids_juegan6)
  

  
  
  
  
  
  
  
  
  # el script base inicial tiene 6rondas... pero ¿es necesaria esta ronda 6? ¿o me esta quitando tiradas de forma innecesaria?
  
  if (length(ids_juegan6) < 1  ){
    
    
    # si NO es necesaria: corto en ronda5, porque ya tengo a un solo jugador
    
    
    
    
    
    #Epilogo con ronda5
    #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda5
    pos_mejor <-  planilla_cazatalentos[ , which.max(p_res_tiros5) ]
    id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]
    
    
    #Finalmente, la hora de la verdadero_mejor
    #Termino el juego
    veredicto  <- gimnasio_veredicto( id_mejor )    
    
    
    
  } else {
    
    
  # si ES necesaria, ejecuto la ronda6
    
  # START ELSE ronda 6
    


  # probamos con nueva variable de tiros6
  var_tiros_r6 <- var_tiros_r5 + suma_tiradas  # sumamos las tiradas de rondas
  
  
  planilla_cazatalentos[ ids_juegan6,  tiros6 := var_tiros_r6 ]  #registro en la planilla la cantidad de tiros
  resultado6  <- gimnasio_tirar( ids_juegan6, var_tiros_r6)
  
  # sumamos el cantidad de aciertos
  planilla_cazatalentos[ ids_juegan6,  aciertos6 := resultado6 ] #registro en la planilla la cantidad de aciertos
  
  # denuevo probamos con % nose si funcionara:
  # SOLO le sumamos a los jugadores que tenemos filtrados en id_juegan
  # NUEVO porcent:
  # (aciertos1 + aciertos2 + aciertos3 + aciertos4 + aciertos5 + aciertos6) / (var_tiros_r1 + var_tiros_r2 + var_tiros_r3 + var_tiros_r4 + var_tiros_r5 + var_tiros_r6)
  
  planilla_cazatalentos[ ids_juegan6,  
                         p_res_tiros6 := 
                           (  resultado6 +
                                planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan6]$aciertos1+
                                planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan6]$aciertos2+
                                planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan6]$aciertos3+
                                planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan6]$aciertos4+
                                planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan6]$aciertos5
                           )
                         /
                           (var_tiros_r1+
                              var_tiros_r2+
                              var_tiros_r3+
                              var_tiros_r3+
                              var_tiros_r4+
                              var_tiros_r5)
  ] 
  
  
  
  
  
  #Epilogo con ronda6
  #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda2
  pos_mejor <-  planilla_cazatalentos[ , which.max(p_res_tiros6) ] # fix, reviso el que tenga el mayor porcentaje de aciertos acumulados
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]
  
  
  
  
  
  
  
  } # END ELSE para ronda6
  
  
  
  
  
  
  
  # TO-DO: es necesaria una ronda7?
  # ¿Cómo determinar si es necesaria una nueva ronda?
  
  
  
  
  
  
  
  
  
  
  #Finalmente, la hora de la verdadero_mejor
  #Termino el juego
  veredicto  <- gimnasio_veredicto( id_mejor )

  return( veredicto )
  
  }
  
#------------------------------------------------------------------------------

#Aqui hago la Estimacion Montecarlo del porcentaje de aciertos que tiene la estrategia VB001!

set.seed( 295873 )  #debe ir una sola vez, ANTES de los experimentos

# primos
# 295873
# 527173 - 2do seed
# 328789
# 825733
# 191519
# 341963
# 590771
# 765103
# 402263
# 584707




tabla_veredictos  <- data.table(  tiros_total=integer(),  acierto=integer() )


for( experimento  in  1:cant_experimentos ) #n experimentos
{
  
  
  # hace un cat del numero de experimento cada mil experimentos:
  if( experimento %% 1000 == 0 )  cat( experimento, ", ")  #desprolijo, pero es para saber por donde voy

  #por cada iteración de las 10k iteraciones, genera la variable veredicto, y pone el resultado de las estrategias:
  veredicto  <- Estrategia_VB001() # guarda el resultado
  
  # rbind: unir agregando filas, cuando sabemos que los nombres de las columnas (variables), son iguales y están en el mismo orden
  tabla_veredictos  <- rbind( tabla_veredictos, veredicto ) #agrega filas de los resultados de veredicto al datatable tabla_veredictos
  
  
}





# esto lo usaba para revisar el id jugador de a 1 ronda
# cat(" Cual es el jugador (sabiendo el id para tener un check):\n")
# as.integer( GLOBAL_jugadores == 0.7)
# which(GLOBAL_jugadores == 0.7)
# cat("\n")



#cat("Planilla en 1ra ronda:")
#View(planilla_cazatalentos)
#cat("\n")






cat("Tabla veredictos:")
tabla_veredictos
cat("\n")

cat("Cantidad tabla Veredictos:")
dim(tabla_veredictos)
cat("\n")

cat("Tiros total y tasa eleccion correcta:")
tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]
tiros_total 
tasa_eleccion_correcta
cat("\n")





