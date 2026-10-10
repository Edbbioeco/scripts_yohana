# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(parzer)

library(geosphere)

library(sf)

library(terra)

library(tidyterra)

library(adehabitatHR)

library(ggnewscale)

library(ggspatial)

library(ggview)

# Dados ----

## Coordenadas ----

### Importar ----

dados_rhinellas <- purrr::map_dfr(
  c("Rhinella icterica", "Rhinella crucifer"),
  \(rhinella){

    readxl::read_xlsx(paste0("Dados ", rhinella, ".xlsx")) |>
      dplyr::mutate(Espécie = rhinella)

    },
  .progress = TRUE)

### Visualizar ----

dados_rhinellas

dados_rhinellas |> dplyr::glimpse()

### Converter coordenadas ----

dados_rhinellas %<>%
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

dados_rhinellas

### Completar coordenadas ----

dados_rhinellas <- purrr::reduce(
  .x = 1:nrow(dados_rhinellas),
  .f = \(dados, x){

    if(is.na(dados$Longitude[x]) || is.na(dados$Latitude[x])){

      dest <- geosphere::destPoint(
        p = c(dados$Longitude[x - 1], dados$Latitude[x - 1]),
        b = dados$`Angulo de virada`[x - 1],
        d = dados$`Distância (m)`[x - 1])

      dados$Longitude[x] <- dest[1]

      dados$Latitude[x]  <- dest[2]

      }

    dados

  },

  .init = dados_rhinellas)

dados_rhinellas

## Imagem de satélite ----

### Importar ----

sat <- terra::rast("imagem_satelite_esec.tif")

### Visualizar ----

sat

ggplot() +
  tidyterra::geom_spatraster_rgb(data = sat)

# Análise de área de vida ----

## Transformas as coordenadas em shapefiles ----

dados_rhinellas_sf <- dados_rhinellas |>
  sf::st_as_sf(coords = c(2:3),
               crs = 4674) |>
  sf::st_transform(crs = 31985)

dados_rhinellas_sf
