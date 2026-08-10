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

## Área bbox da uc ----

uc_bbox <- uc |>
  sf::st_bbox() |>
  sf::st_as_sfc() |>
  sf::st_as_sf()

uc_bbox

ggplot() +
  geom_sf(data = uc_bbox, color = "red") +
  geom_sf(data = uc, color = "black")

# Raster de uso e cobertura do solo ----

## Importar ----

uso_solo <- terra::rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_10/lulc/coverage/brazil_coverage_2024.tif") |>
  terra::crop(uc_bbox) |>
  terra::mask(uc_bbox) |>
  terra::as.factor()
