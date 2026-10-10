# Pacotes ----

library(tidyverse)

library(readxl)

library(magrittr)

library(parzer)

library(geosphere)

library(terra)

library(tidyterra)

library(sf)

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

ggplot() +
  tidyterra::geom_spatraster_rgb(data = sat) +
  geom_sf(data = dados_rhinellas_sf, aes(color = Espécie))

## Calcular área de vida ----

area_rhinella <- purrr::map_dfr(
  c("Rhinella icterica", "Rhinella crucifer"),
  \(rhinella){

    id <- dados_rhinellas_sf |>
      dplyr::filter(Espécie == rhinella) |>
      dplyr::pull(ID) |>
      unique()

    purrr::map(
      id,
      \(ind){

        mcp <- dados_rhinellas_sf |>
          dplyr::filter(Espécie == rhinella & ID == ind) |>
          sf::as_Spatial() |>
          adehabitatHR::mcp(percent = 100) |>
          sf::st_as_sf() |>
          dplyr::mutate(Espécie = rhinella,
                        Indivíduo = ind)

        kde <- dados_rhinellas_sf |>
          dplyr::filter(Espécie == rhinella & ID == ind) |>
          sf::as_Spatial() |>
          adehabitatHR::kernelUD(h = "href") |>
          adehabitatHR::getverticeshr(percent = 50) |>
          sf::st_as_sf() |>
          dplyr::mutate(Espécie = rhinella,
                        Indivíduo = ind)

        dplyr::bind_rows(mcp, kde)

        }
      )

    },
  .progress = TRUE)

area_rhinella

## Calcular os centroides de cada indivíduo ----

centroides <- area_rhinella |>
  dplyr::group_by(Indivíduo) |>
  sf::st_centroid()

centroides
