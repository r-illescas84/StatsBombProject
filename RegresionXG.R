library(tidyverse)
library(StatsBombR)
library(ggplot2)
library(SBpitch)
library(grid)

torneo <- FreeCompetitions() %>%
  filter(competition_gender == "male") %>%
  filter(country_name == "International" | competition_name == "UEFA Euro" | country_name == "South America")
partidos <- FreeMatches(torneo)
datos_tiros <- free_allevents(partidos)%>%
  filter(type.name == "Shot")
outcome_id = unique(datos_tiros[, c("shot.outcome.id", "shot.outcome.name")])
datos_tiros$gol <- datos_tiros$shot.outcome.id == 97
calcular_distancia <- function(coords){
  dist <- sqrt((coords[1] - 120)^2 + (coords[2] - 40)^2)
  return(dist)
}
convertir_coords <- function(coords_string) {
  coords_num <- as.numeric(unlist(coords_string))
  return(coords_num)
}
calcular_angulo <- function(coords){
  typeof(coords)
  ang1 <- atan2(40-3.66 - coords[2], 120 - coords[1])
  ang2 <- atan2(40+3.66 - coords[2], 120 - coords[1])
  
  angulo <- abs(ang1 - ang2)
  grados <- angulo*(180/pi)
  
  return(grados)
}
datos_tiros <- datos_tiros %>%
  rowwise() %>%
  mutate(
    coords_num = list(convertir_coords(location)),  
    distancia = calcular_distancia(coords_num),     
    angulo = calcular_angulo(coords_num)            
  ) %>%
  select(-coords_num)

columnas_tiros <- datos_tiros %>%
  select(distancia, angulo)

modelo_xG <- lm(shot.statsbomb_xg ~ distancia + angulo, data = datos_tiros)

summary(modelo_xG)

nuevo_tiro <- data.frame(distancia = distancia_tiro, angulo = angulo_tiro)
xG_predicho <- predict(modelo_xG, nuevo_tiro, type = "response")

print(paste("xG Predicho:", xG_predicho))