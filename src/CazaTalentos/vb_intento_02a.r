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
  
  
  
  
  
  if ( length( pids ) >  0 ) {

    
    
    # generar una tabla con la cantidad de jugadores por vuelta?
    # reviso los datos
    # cat("->pcantidad")
    # cat(pcantidad)
    # cat("\n")
    # cat("->length( pids )")
    # cat( length( pids ) )
    # cat("\n")
    # cat("->GLOBAL_jugadores[pids]")
    # cat(GLOBAL_jugadores[pids])
    # cat("\n")

    
    
    # mapply: Apply a Function to Multiple List or Vector Arguments
    res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )    
    
    
    
    return( res )
    
  }
  

  
  
  


  
  
  

  
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
#   - dejando a los mejores (arriba del avg)
#   - y al ser menos jugadores, podemos sumar más tiradas por jugador (darles más tiradas a los mejores jugadores que vayan quedando de ronda en ronda)
#   - por cada ronda una idea era quedame con el porcentaje de aciertos (resultado / tiros)
#   - en cada vuelta, se acumulan los porcentajes de aciertos
#   - esto va a darme que: el BUENO va a ser siempre bueno (como Michael Jordan) y el regular, va a poder ser bueno en algunos %, y malo en otros %, cuando sumamos el total de estos, va a "sobresalir" el bueno y va el regular va a estar con el corazón dolido, pero al mismo tiempo va a esforzarse para ser mejor el próximo año.


# Este script: incluye una ronda 7 siempre y cuando valga la pena, y usa multiplicador para iteración de jugadas por ronda en lugar de suma


# TO-DO:
# Podría "automatizarse" eso, creo, por ej:
#   - podría hacerse una funcion ronda() que la estrategía "itere" en rondas, y que "ejecute" una nueva ronda, siempre y cuando sea permitido (todavía tenemos tiros para tirar? tenemos más de un jugador?, buscar las reglas necesarias y los parámetros de entrada necesarios para "iterar" en la función de Rondas.






# pruebas realizadas:



# median_ronda2  <- 1.005  #uso el mean
# median_ronda3  <- 1.005  #uso el mean
# median_ronda4  <- 1.004  #uso el mean
# median_ronda5  <- 1.004  #uso el mean
# median_ronda6  <- 1.0    #uso el mean
# median_ronda7  <- 1.0    #uso el mean
# tirada_inicial <- 65
# mult_tiradas   <- 1.1   #se multiplican las tiradas por ronda en base al valor de tirada anterior
# > cat("Tiros total y tasa eleccion correcta:")
# Tiros total y tasa eleccion correcta:> tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
# > tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]
# > tiros_total 
# [1] 14846
# > tasa_eleccion_correcta
# [1] 0.9905
# > cat("\n")
#cant_experimentos <- 2000



median_ronda2     <- 1.0045  # probando uso el mean
median_ronda3     <- 1.0045  # probando uso el mean
median_ronda4     <- 1.0037  # probando uso el mean
median_ronda5     <- 1.0037  # probando uso el mean
median_ronda6     <- 1.00  # probando uso el mean
median_ronda7     <- 1.00  # probando uso el mean
tirada_inicial    <- 40     # vueltas en la primera ronda
mult_tiradas      <- 1.1    # se multiplican las tiradas por ronda en base al valor de tirada anterior
suma_tiradas      <- 25     # se suman a las tiradas por ronda por sobre el valor de ronda anterior
cant_experimentos <- 2000   # cantidad de experimentos, la achico por tiempos de demora y voy subiendolos a 10k







Estrategia_VB001  <- function()



{

  
  
  
  #Estrategia original del script intento
  #Se juegan varias rondas
  #De una ronda a la otra, solo pasan los que tuvieron igual o mayor aciertos a la mediana de aciertos de la ronda anterior
  


  gimnasio_init()
  
  

  #Esta el la planilla del cazatalentos
  #el id es el numero que tiene en la espalda cada jugador
  planilla_cazatalentos  <- data.table( "id"= 1:100 )


  
  
  

  # ------------------------------------------------------
  #Ronda 1: 
  #tiran los 100 jugadores es decir 1:100
  
  ids_juegan1  <- 1:100   #los jugadores que participan en la ronda,
  
  var_tiros_r1 <- tirada_inicial # en primera ronda
  
  
  # registros:  
  planilla_cazatalentos[ ids_juegan1,  tiros1 := var_tiros_r1 ]  #registro en la planilla
  resultado1  <- gimnasio_tirar( ids_juegan1, var_tiros_r1) # cantidad de tiros
  
  

  
  planilla_cazatalentos[ ids_juegan1,  aciertos1 := resultado1 ]  #registro en la planilla la cantidad de aciertos 
  
  #prueba: sumar el porcentaje de aciertos (resultado / tiros)
  
  planilla_cazatalentos[ ids_juegan1, 
                         aciertos1 := resultado1
                         ]
  
  # pongo los := por separado
  planilla_cazatalentos[ ids_juegan1, 
                         #probamos con los % nose si funcionara
                         p_res_tiros1 := resultado1 / var_tiros_r1 
  ]
  


  
  
  
  # Rondas siguientes
  # TO-DO: Funcion de ronda siguiente
  
  
  
  
  
  #-------------------------------------------------------------
  #Ronda 2: 


  # nos quedamos con los jugadores que pasan

  mediana  <- planilla_cazatalentos[ ids_juegan1, (mean(p_res_tiros1,na.rm = TRUE)*median_ronda2) ]
  #mediana  <-planilla_cazatalentos[ ids_juegan1, p_res_tiros1[{q<-rank(p_res_tiros1)/length(p_res_tiros1);q<0.1 | q>=0.9}] ] 
  
  
  # nuestros nuevos jugadores estan en ids_juegan2
  ids_juegan2  <- planilla_cazatalentos[ ids_juegan1 ][ p_res_tiros1 >= mediana, id ] 
  
  
  # probamos con nueva variable de tiros
  var_tiros_r2 <- round(var_tiros_r1 * mult_tiradas) + suma_tiradas
  
  
  # registros:
  
  planilla_cazatalentos[ ids_juegan2,  tiros2 := var_tiros_r2 ] 
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
                         # (aciertos2 + aciertos 1) / (var_tiros_r1 + var_tiros_r2)
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

  
  # nos quedamos con los jugadores que pasan
  mediana  <- planilla_cazatalentos[ ids_juegan2, (mean(p_res_tiros2,na.rm = TRUE)*median_ronda3) ]
  #mediana  <-planilla_cazatalentos[ ids_juegan2, p_res_tiros2[{q<-rank(p_res_tiros2)/length(p_res_tiros2);q<0.1 | q>=0.9}] ] 
  
  
  # nuestros nuevos jugadores estan en ids_juegan3
  ids_juegan3  <- planilla_cazatalentos[ ids_juegan2 ][ p_res_tiros2 >= mediana, id ] 
  
  
  # probamos con nueva variable de tiros
  var_tiros_r3 <- round(var_tiros_r2 * mult_tiradas) + suma_tiradas
  
  
  # registros
  planilla_cazatalentos[ ids_juegan3,  tiros3 := var_tiros_r3 ]  
  resultado3  <- gimnasio_tirar( ids_juegan3, var_tiros_r3)
  
  # sumamos el cantidad de aciertos
  planilla_cazatalentos[ ids_juegan3,  aciertos3 := resultado3 ]  
  
  # denuevo probamos con % nose si funcionara:
  # SOLO le sumamos a los jugadores que tenemos filtrados en id_juegan
  # NUEVO porcent:
  # (aciertos3 + aciertos2 + aciertos 1) / (var_tiros_r1 + var_tiros_r2 + var_tiros_r3)
  
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

  
  # nos quedamos con los jugadores que pasan
  mediana  <- planilla_cazatalentos[ ids_juegan3, (mean(p_res_tiros3,na.rm = TRUE)*median_ronda4) ]
  #mediana  <-planilla_cazatalentos[ ids_juegan3, p_res_tiros3[{q<-rank(p_res_tiros3)/length(p_res_tiros3);q<0.1 | q>=0.9}] ] 

    
  # nuestros nuevos jugadores estan en ids_juegan4
  ids_juegan4  <- planilla_cazatalentos[ ids_juegan3 ][ p_res_tiros3 >= mediana, id ] 
  
  # probamos con nueva variable de tiros
  var_tiros_r4 <- round(var_tiros_r3 + mult_tiradas) + suma_tiradas
  
  
  planilla_cazatalentos[ ids_juegan4,  tiros4 := var_tiros_r4 ]  
  resultado4  <- gimnasio_tirar( ids_juegan4, var_tiros_r4)
  
  # sumamos el cantidad de aciertos
  planilla_cazatalentos[ ids_juegan4,  aciertos4 := resultado4 ]  
  
  # denuevo probamos con % nose si funcionara:
  # SOLO le sumamos a los jugadores que tenemos filtrados en id_juegan
  # NUEVO porcent:
  # (aciertos 1 + aciertos2 + aciertos3 + aciertos4) / (var_tiros_r1 + var_tiros_r2 + var_tiros_r3 + var_tiros_r4)
  
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
  #Ronda 5: 
  #A la mitad mejor la hago tirar 

  # nos quedamos con los jugadores que pasan
  mediana  <- planilla_cazatalentos[ ids_juegan4, (mean(p_res_tiros4,na.rm = TRUE)*median_ronda5) ] 
  #mediana  <-planilla_cazatalentos[ ids_juegan4, p_res_tiros4[{q<-rank(p_res_tiros4)/length(p_res_tiros4);q<0.1 | q>=0.9}] ] 
  
  
  # nuestros nuevos jugadores estan en ids_juegan5
  ids_juegan5  <- planilla_cazatalentos[ ids_juegan4 ][ p_res_tiros4 >= mediana, id ] 
  
  
  # probamos con nueva variable de tiros
  var_tiros_r5 <- round(var_tiros_r4 * mult_tiradas) + suma_tiradas
  
  planilla_cazatalentos[ ids_juegan5,  tiros5 := var_tiros_r5 ]  
  resultado5  <- gimnasio_tirar( ids_juegan5, var_tiros_r5)
  
  # sumamos el cantidad de aciertos
  planilla_cazatalentos[ ids_juegan5, aciertos5 := resultado5 ] 
  
  # denuevo probamos con % nose si funcionara:
  # SOLO le sumamos a los jugadores que tenemos filtrados en id_juegan
  # NUEVO porcent:
  # (aciertos 1 + aciertos2 + aciertos3 + aciertos4 + aciertos5) / (var_tiros_r1 + var_tiros_r2 + var_tiros_r3 + var_tiros_r4 + var_tiros_r5)
  
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

  # nos quedamos con los jugadores que pasan
  mediana  <- planilla_cazatalentos[ ids_juegan5, (mean(p_res_tiros5,na.rm = TRUE)*median_ronda6) ] 
  #mediana  <-planilla_cazatalentos[ ids_juegan5, p_res_tiros5[{q<-rank(p_res_tiros5)/length(p_res_tiros5);q<0.1 | q>=0.9}] ] 

    
  # nuestros nuevos jugadores estan en ids_juegan6
  ids_juegan6  <- planilla_cazatalentos[ ids_juegan5 ][ p_res_tiros5 >= mediana, id ] 
  
  
  # probamos con nueva variable de tiros
  var_tiros_r6 <- round(var_tiros_r5 * mult_tiradas) + suma_tiradas
  
  
  planilla_cazatalentos[ ids_juegan6,  tiros6 := var_tiros_r6 ] 
  resultado6  <- gimnasio_tirar( ids_juegan6, var_tiros_r6)
  
  # sumamos el cantidad de aciertos
  planilla_cazatalentos[ ids_juegan6,  aciertos6 := resultado6 ] 
  
  # denuevo probamos con % nose si funcionara:
  # SOLO le sumamos a los jugadores que tenemos filtrados en id_juegan
  # NUEVO porcent:
  # (aciertos 1 + aciertos2 + aciertos3 + aciertos4 + aciertos5) / (var_tiros_r1 + var_tiros_r2 + var_tiros_r3 + var_tiros_r4 + var_tiros_r5)
  
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  #-------------------------------------------------------------
  #Ronda 7: subiendo de a 10
  #A la mitad mejor la hago tirar 

  
  # nos quedamos con los jugadores que pasan
  mediana  <- planilla_cazatalentos[ ids_juegan6, (mean(p_res_tiros6,na.rm = TRUE)*median_ronda7) ] 
  #mediana  <-planilla_cazatalentos[ ids_juegan6, p_res_tiros6[{q<-rank(p_res_tiros6)/length(p_res_tiros6);q<0.1 | q>=0.9}] ] 

    
  # nuestros nuevos jugadores estan en ids_juegan7 
  ids_juegan7  <- planilla_cazatalentos[ ids_juegan6 ][ p_res_tiros6 >= mediana, id ] 
  

  
  
  
  
  
  
  
  
  # esto es parecido al script anterior, pero reviso si tengo chance de jugar otra ronda mas, es necesaria una nueva ronda? o me quita tiradas de forma innecesaria?
  
  if (length(ids_juegan7) <= 3  ){
    
    # hay al menos n candidatos? tomar el maximo y cerrar rondas
    
    #Epilogo
    #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda6
    pos_mejor <-  planilla_cazatalentos[ , which.max(p_res_tiros6) ]
    
    
    id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]
    
    
    #Finalmente, la hora de la verdadero_mejor
    #Termino el juego
    veredicto  <- gimnasio_veredicto( id_mejor )    
    
    
    
  } else {
    
  
  # start else ronda7
    
  # hay mas de 3 candidatos
    


  # probamos con nueva variable de tiros
  var_tiros_r7 <- round(var_tiros_r6 * mult_tiradas) + suma_tiradas
  
  
  planilla_cazatalentos[ ids_juegan7,  tiros7 := var_tiros_r7 ]  
  resultado7  <- gimnasio_tirar( ids_juegan7, var_tiros_r7)
  
  # sumamos el cantidad de aciertos
  planilla_cazatalentos[ ids_juegan7,  aciertos7 := resultado7 ] 
  
  # denuevo probamos con % nose si funcionara:
  # SOLO le sumamos a los jugadores que tenemos filtrados en id_juegan
  # NUEVO porcent:
  # (aciertos 1 + aciertos2 + aciertos3 + aciertos4 + aciertos5 + aciertos6) / (var_tiros_r1 + var_tiros_r2 + var_tiros_r3 + var_tiros_r4 + var_tiros_r5 + var_tiros_r6)
  
  planilla_cazatalentos[ ids_juegan7,  
                         p_res_tiros7 := 
                           (  resultado7 +
                                planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan7]$aciertos1+
                                planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan7]$aciertos2+
                                planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan7]$aciertos3+
                                planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan7]$aciertos4+
                                planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan7]$aciertos5+
                                planilla_cazatalentos[planilla_cazatalentos$id %in% ids_juegan7]$aciertos6
                           )
                         /
                           (var_tiros_r1+
                              var_tiros_r2+
                              var_tiros_r3+
                              var_tiros_r3+
                              var_tiros_r4+
                              var_tiros_r5+
                              var_tiros_r6)
  ] 
  
  
  
  
  
  
  
  #Epilogo para ronda7
  #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda7
  pos_mejor <-  planilla_cazatalentos[ , which.max(p_res_tiros7) ] # fix, reviso el que tenga el mayor porcentaje de aciertos acumulados
  
  
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]
  
  
  
  # end else ronda7
  
  } 
  
  
  
  
  
  
  
  

  
    #Epilogo final
  #El cazatalentos toma una decision, elige al que mas aciertos tuvo en la ronda6
  pos_mejor <-  planilla_cazatalentos[ , which.max(p_res_tiros6) ] # fix, reviso el que tenga el mayor porcentaje de aciertos acumulados
  
  
  id_mejor  <-  planilla_cazatalentos[ pos_mejor, id ]
  
  
  

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


for( experimento  in  1:cant_experimentos ) #10k experimentos
{
  
  
  # hace un cat del numero de experimento cada mil experimentos:
  if( experimento %% 1000 == 0 )  cat( experimento, " ")  #desprolijo, pero es para saber por donde voy

  #por cada iteración de las 10k iteraciones, genera la variable veredicto, y pone el resultado de las estrategias:
  
  veredicto  <- Estrategia_VB001() # guarda el resultado
  
  # rbind: unir agregando filas, cuando sabemos que los nombres de las columnas (variables), son iguales y están en el mismo orden
  tabla_veredictos  <- rbind( tabla_veredictos, veredicto ) #agrega filas de los resultados de veredicto al datatable tabla_veredictos
  
  
}





# esto lo usaba para verificar el id jugador de a 1 ronda
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



cat("Tiros total y tasa eleccion correcta:")
tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]
tiros_total 
tasa_eleccion_correcta
cat("\n")





