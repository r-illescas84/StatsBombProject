# En este archivo encontraremos las funciones para calcular el xGI para cada evento.
# Para usar estas funciones utiliza esta línea de código source("C:/Users/rille/OneDrive/Documentos/Hackathon2024/StatsBombProject/funciones_xGI.R")

tipos_soportados <- c("2","3","9","10","16","20","25","30")

#Calcular_angulo
#Esta función calcula los metros de la posición respecto a la portería. La distancia se usa para la aproximación del expected goal
calcular_distancia <- function(coords){
  coords <- as.numeric(unlist(coords))
  sqrt((coords[1] - 120)^2 + (coords[2] - 40)^2)
}

#Calcular_angulo
  #Esta función calcula los grados de la posición respecto a la portería. El ángulo se usa para la aproximación del expected goal
calcular_angulo <- function(coords){
  coords <- as.numeric(unlist(coords))
  ang1 <- atan2(40-3.66 - coords[2], 120 - coords[1])
  ang2 <- atan2(40+3.66 - coords[2], 120 - coords[1])
  
  angulo <- abs(ang1 - ang2)
  grados <- angulo*(180/pi)
  
  return(grados)
}

#Calcular_xg
  #Esta función lo que hace es calcular un aproximado del statsbombxg con coeficientes obtenidos de una regresión de torneos gratuitos de Europa
calcular_xg <- function(dist, ang){
  xg <- 0.033877 - 0.0027603*dist + 0.0055795*ang
  if (xg<0){
    xg <- 0
  }
  return(xg)
}

#Define_funcion
  #Esta función llama a una función específica según el tipo de jugada que sea el evento.
define_funcion <- function(datos, playID){
  jugada <- datos %>%
    filter(id == playID)
  tipo_ID <- jugada %>%
    pull(type.id)
  nextPlays <- datos %>%
    filter(id == playID) %>%
    pull(related_events)
  switch(as.character(tipo_ID),
         "2" = xGI_BallRecovery(datos, playID,nextPlays), 
         "3" = xGI_Dispossessed(datos, playID,nextPlays), 
         "4" = xGI_Duel(datos, playID,nextPlays), #Podría quitarse
         "6" = xGI_Block(datos, playID,nextPlays), #Podría quitarse
         "9" = xGI_Clearance(datos, playID,nextPlays), 
         "10" = xGI_Interception(datos, playID,nextPlays),
         "14" = xGI_Dribble(datos, playID,nextPlays), #Podría quitarse
         "16" = xGI_Shot(datos, playID,nextPlays),
         "20" = xGI_OwnGoalAgainst(datos, playID,nextPlays),
         "25" = xGI_OwnGoalFor(datos, playID,nextPlays), #Podría quitarse
         "30" = xGI_Pass(datos, playID,nextPlays), 
         "33" = xGI_fifty(datos, playID,nextPlays), #Podría quitarse
         "37" = xGI_Error(datos, playID,nextPlays), #Podría quitarse
         "38" = xGI_MisControl(datos, playID,nextPlays), #Podría quitarse
         "39" = xGI_DribbledPast(datos, playID,nextPlays), #Podría quitarse
         "default" = 0
  )
}
tipos_soportados <- c("2","3","9","10","16","20","30")
#Ball Recovery
  # Esta función unicamente puede aumentar xGI, se tomará como 0 si no recupera el balón
  # Se toma el valor del oponente para xG
xGI_BallRecovery <- function(datos, playID,nextPlays){
  jugada <- datos %>%
    filter(id == playID)
  fallita <- jugada %>%
    pull(ball_recovery.recovery_failure)
  if(is.na(fallita)){
    coords <- as.numeric(unlist(jugada %>%
      pull(location)))
    dist <- calcular_distancia(coords)
    ang <- calcular_angulo(coords)
    xg <- calcular_xg(dist, ang)
    coords[1] <- 120-coords[1]
    coords[2] <- 80-coords[2]
    dist_R <- calcular_distancia(coords)
    ang_R <- calcular_angulo(coords)
    xg_R <- calcular_xg(dist_R, ang_R)
    return(xg + xg_R)
  }
  else{
    return(0)
  }
}

#Dispossessed
  # Esta función evaluará qué está peor, si el xG que tenía o el que le dio al rival y resta eso
xGI_Dispossessed <- function(datos, playID, nextPlays){
  jugada <- datos %>%
    filter(id == playID)
  coords <- as.numeric(unlist(jugada%>%
    pull(location)))
  distP <- calcular_distancia(coords) #P se refiere a Propio
  angP <- calcular_angulo(coords)
  xgPropio <- calcular_xg(distP,angP)
  coordsR <- c(120-coords[1], 80-coords[2]) #R se refiere a Rival
  distR <- calcular_distancia(coordsR) 
  angR <- calcular_angulo(coordsR)
  xgRival <- calcular_xg(distR, angR)
  return(-xgPropio -xgRival)
}

#Clearance
  #Esta función solo sumará los xG que se le están quitando al rival
xGI_Clearance <- function(datos, playID, nextPlays){
  jugada <- datos %>%
    filter(id == playID)
  coords <- as.numeric(unlist(jugada%>%
    pull(location)))
  coordsR <- c(120-coords[1], 80-coords[2]) #R se refiere a Rival
  distR <- calcular_distancia(coordsR) 
  angR <- calcular_angulo(coordsR)
  xgRival <- calcular_xg(distR, angR)
  return(xgRival)
}

#Interception
  #En esta función vemos si al hacer la intercepción mantiene posesión, si eso es cierto entonces suma xG.
xGI_Interception <- function(datos, playID, nextPlays){
  jugada <- datos %>%
    filter(id == playID)
  exitoID <- jugada %>%
    pull(interception.outcome.id)
  if(exitoID==15 | exitoID==16 | exitoID==17 | exitoID == 4){
    coords <- as.numeric(unlist(jugada%>%
      pull(location)))
    coordsR <- c(120-coords[1], 80-coords[2]) #R se refiere a Rival
    distR <- calcular_distancia(coordsR) 
    angR <- calcular_angulo(coordsR)
    xgRival <- calcular_xg(distR, angR)
    distP <- calcular_distancia(coords)
    angP <- calcular_angulo(coords)
    xgPropio <- calcular_xg(distP, angP)
    return(xgRival + xgPropio)
  }else{
    return(0)
  }
}

#Shot
  #En esta función haremos un cálculo más complicado. Si es gol, aumentará 1-xG.
  #Aquí lo ideal es que si no es gol. Calculamos xGOT - xG, de manera que si es un buen tiro beneficie al jugador. Si es un mal tiro le resta 
  #Como no tenemos xGOT y es un trabajo más complicado calcularlo, si el tiro va fuera de portería se le resta xG y si es un tiro bloqueado o se restará la mitad del xG del tiro.
xGI_Shot <- function(datos, playID, nextPlays){
  jugada <- datos %>%
    filter(id == playID)
  sb_xG <- as.numeric(unlist(jugada %>%
    pull(shot.statsbomb_xg)))
  outcome_id <- jugada %>%
    pull(shot.outcome.id)
  mitades <- c("96","100","116")
  if(outcome_id == "97"){
    return(1-sb_xG)
  }else if(outcome_id %in% mitades){
    return(-0.5*sb_xG)
  }else{
    return(-sb_xG)
  }
}

#Own Goal Against
  #En esta función calculamos  el xG del rival, Liego hacemos xG del rival menos 1 que es lo que perjudicó el autogol.
xGI_OwnGoalAgainst <- function(datos, playID, nextPlays){
  jugada <- datos %>%
    filter(id == playID)
  coords <- as.numeric(unlist(jugada %>%
    pull(location)))
  coords_r <- c(120-coords[1], 80-coords[2])
  distR <- calcular_distancia(coords_r)
  angR <- calcular_angulo(coords_r)
  xG_r <- calcular_xg(distR, angR)
  return(xG_r-1)
}

#Pass
  #en esta función vamos a calcular el xG inicial y el xG final.
  #Para el expected goal final revisamos si es un tiro, pase completo o pase incompleto.
  #Se calcula el xG final y el xGI
xGI_Pass <- function(datos, playID, nextPlays){
  jugada <- datos %>%
    filter(id == playID)
  coords <- as.numeric(unlist(jugada %>%
    pull(location)))
  dist_inicial <- calcular_distancia(coords)
  ang_inicial <- calcular_angulo(coords)
  xG_inicial <- calcular_xg(dist_inicial, ang_inicial)
  if(!is.na(jugada%>%pull(pass.shot_assist)) & (jugada%>%pull(pass.shot_assist) | jugada%>%pull(pass.goal_assist))){
    id_tiro <- jugada %>%
      pull(pass.assisted_shot_id)
    tiro <- datos %>%
      filter(id == id_tiro)
    xG_tiro <- tiro %>% pull(shot.statsbomb_xg)
    return(xG_tiro - xG_inicial)
  }else if(is.na(jugada%>%pull(pass.outcome.name))){
    #Fue pase completo
    coords_final <- jugada %>% pull(pass.end_location)
    dist_final <- calcular_distancia(coords_final)
    ang_final <- calcular_angulo(coords_final)
    xG_final <- calcular_xg(dist_final, ang_final)
    return(xG_final - xG_inicial)
  }else{
    #Fue pase incompleto
    return(-xG_inicial)
  }
}