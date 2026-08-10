# Pacotes ----

library(sf)

library(tidyverse)

library(terra)

library(tidyterra)

library(landscapemetrics)

library(ggview)

# Shapefile da unidade de conservação ----

## Importar ----

uc <- sf::st_read("./uc_municipal_ceuc_outros_construcao.shp") |>
  dplyr::filter(nome |> stringr::str_detect("Capivara"))

## Visualizar ----

uc

ggplot() +
  geom_sf(data = uc, color = "black")
