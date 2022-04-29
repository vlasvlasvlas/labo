


# -- idea ---------------------------------------------------------------------
# 1tiro1ronda:


# Objetivo:
# "estrategia sin rondas" , que no existan las rondas
# los 100 jugadores tiran 1 sola vez 
# al finalizar la ronda, con un calculo reviso cuales de esos son descartados
# podria pasar que justo en ese tiro ninguno es descartado

# idea:
# sumar un robot que tire como un 0.7, acumulando 1tiro2ronda y tenerlo de espia/control de desviación 0.7 
# teniendo asi el umbral de nuestro mejor jugador 0.7, y controlando los 100 jugadores contre ese umbral
# para comparar y buscar al bueno (0.7 real ) dentro de los 100
# si el jugador cae "dentro de una varianza similar" o "dentro de la varianza de mi robot a 0.7" entonces sigue jugando, sino se va


# Standard deviation is the measure of the dispersion of the values
# The higher the standard deviation, the wider the spread of values
# The lower the standard deviation, the narrower the spread of values.
# A simple words the formula is defined as - Standard deviation is the square root of the 'variance'
# Standard deviation converts the negative number to a positive number by squaring it.
# It shows the larger deviations so that you can particularly look over them
# It shows the central tendency, which is a very useful function in the analysis
# Variance - It is defined as the squared differences between the observed value and expected value.
# In general, The values will be so close to the average value in low standard deviation and the values will be far spread from the average value in the high standard deviation.

# referencias: 
# https://www.journaldev.com/38162/find-standard-deviation-in-r
# https://precisionrifleblog.com/2015/04/18/how-much-does-sd-matter/
# https://www.theonlycolors.com/2020/4/27/21226073/the-variance-of-college-basketball-how-big-is-it-and-where-does-it-come-from
# https://rpubs.com/mpfoley73/458411





#limpio la memoria
rm( list=ls() )
gc()

options(digits = 10) # fix
require("data.table")






# -- funciones globales --------------------------------------------------------


ftirar  <- function( prob, qty )
{
  return( sum( runif(qty) < prob ) )
}



#variables globales que usan las funciones gimnasio_xxxx
GLOBAL_jugadores  <- c()
GLOBAL_tiros_total  <- 0


#Crea el juego.
#a cada jugador se le pone un numero de 1 a 100 en la espalda
#debajo de ese numero esta el indice_de_enceste  que NO puede ser visto por el cazatalentos
gimnasio_init  <- function() 
{
  GLOBAL_jugadores  <<-  sample( c( (501:599 )/1000.0 , 0.7 ) )
  GLOBAL_tiros_total  <<- 0
  
}




#se le pasa un vector con los IDs de los jugadores y la cantidad de tiros a realizar
#devuelve en un vector cuantos aciertos tuvo cada jugador
gimnasio_tirar  <- function( pids,  pcantidad ) {
  
  GLOBAL_tiros_total  <<-  GLOBAL_tiros_total + length( pids )*pcantidad
  
  res  <- mapply(  ftirar, GLOBAL_jugadores[pids], pcantidad )   
  return( res )
  
}


#El cazatalentos decide a que jugador llevarse
#devuelve la cantidad de tiros libres y si le acerto al verdadero_mejor o no
gimnasio_veredicto  <<- function( nroexperimento, pid ) {
  return( list("nroexperimento"= nroexperimento,
               "tiros_total"= GLOBAL_tiros_total, 
               "acierto"=     as.integer( GLOBAL_jugadores[pid]==0.7) ))
}















rondastat  <<- data.table()

# -- estrategia ---------------------------------------------------------------

Estrategia_intento006  <- function(nroexperimento)
  
{
  
  
  gimnasio_init()
  # parametros iniciales
  ids_juegan  <- 1:100
  var_tiros <- 1
  cant_rondas <- 800
  
  
  rondas  <- c(1:cant_rondas)
  
  localstats           <- data.table()
  revision_x_jugador   <- data.table() 
  
  for (ronda in rondas) {
    
    if ( GLOBAL_tiros_total + length(ids_juegan) < 15000 ) { # fix
      
      if ( length(ids_juegan) > 1) {
	  
        # umbral de corte
        # 3 standard deviations from the expected value: n * pi - 3 * sqrt(n * pi * (1 - pi))
        umbral <- ((1:ronda) * 0.7) - (3.15*1.09) * sqrt( (1:ronda) * 0.7 * (1 - 0.7) )  
      
        resultado <- gimnasio_tirar( ids_juegan, var_tiros)  
        
        localstats <- rbindlist(list(localstats, data.table(
          ronda=ronda,
          jugador=ids_juegan, 
          tiros=var_tiros,
          aciertos=resultado
        )))

        revision_x_jugador <- localstats[order(ronda)][jugador %in% ids_juegan, .(
          nroexperimento=nroexperimento, 
          jugador=jugador, 
          ronda=ronda,
          aciertos=aciertos,
          acumulasum = cumsum(aciertos),
          desviacion = sd(cumsum(aciertos)),
          umbral=umbral,
          jugador_target=which(GLOBAL_jugadores == 0.7)
          
        ), by = .(jugador)]
        
        
        
        if ( ronda > 1 ) {
          
          ids_ronda  <- revision_x_jugador[ ronda == max(ronda)] 
          ids_juegan <- unique(ids_ronda[ ceiling(acumulasum) >= trunc(umbral), jugador]) #fix
          
        }
       
      } # fin length
    } #fin max tiros      
  } # fin rondas
  
  
  
  finalista <- revision_x_jugador[order(-(acumulasum*desviacion))] #fix
  id_mejor  <- finalista[,jugador][1:1]
  veredicto <- gimnasio_veredicto( nroexperimento, id_mejor )     
  
  
  # check
  if ( veredicto$acierto == 0 ) {
    cat("\n -->",veredicto$tiros_total,"-",veredicto$acierto,"-",max( revision_x_jugador$ronda),"\n")  
  }
  
  return( veredicto )
  
} # fin funcion


# -- seeds --------------------------------------------------------------------
set.seed( 295873 ) #debe ir una sola vez, ANTES de los experimentos
# primos
# 295873
# 527173
# 328789 
# 825733
# 191519




# -- inicio de experimentos ---------------------------------------------------

tabla_veredictos  <- data.table( nroexperimento=integer(), tiros_total=integer(), acierto=integer() )

cant_experimentos  <- 10000
ver_resultadoscada <- 250

cat("Resultados iterados cada ",ver_resultadoscada," experimentos: \n")
for( experimento  in  1:cant_experimentos )
{
  
  if( experimento %% ver_resultadoscada == 0 ) {
    cat( "\n cant. exp. ",experimento, " (tiros: ", tabla_veredictos[  , max( tiros_total) ], ", prob:",tabla_veredictos[  , mean( acierto) ],") \n")
  } 

  # if( experimento %% 1 == 0 ) {
  #   cat( ".")
  # } 
  
  veredicto  <- Estrategia_intento006(experimento) 
  
  tabla_veredictos  <- rbind( tabla_veredictos, veredicto )
  
}

# -- fin de experimentos ------------------------------------------------------




# -- resultados ---------------------------------------------------------------
tiros_total  <-  tabla_veredictos[  , max( tiros_total) ]
tasa_eleccion_correcta  <-  tabla_veredictos[  , mean( acierto) ]
cat("Tiros total y tasa eleccion correcta:")
tiros_total
tasa_eleccion_correcta


# cant. exp.  10000  (tiros:  14999 , prob: 0.99589959 ) 
# Tiros total y tasa eleccion correcta:
#   > 
#   > tiros_total
# [1] 14999
# > tasa_eleccion_correcta
# [1] 0.9959


