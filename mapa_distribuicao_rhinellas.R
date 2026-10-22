# Pacotes ----

library(googledrive)

library(readxl)

library(tidyverse)

library(magrittr)

library(parzer)

library(geosphere)

library(sf)

library(terra)

library(tidyterra)

library(ggmagnify)

# Dados ----

## Dados de ocorrência ----

### Baixar do Google Drive ----

googledrive::drive_auth("edson.nilton@ufpe.br")

Sys.getenv("pasta_artigo_rhinella") |>
  googledrive::drive_get() |>
  googledrive::drive_ls() |>
  dplyr::filter(name |> stringr::str_detect("Dados Rhinella")) |>
  dplyr::pull(name) |>
  purrr::map(
    \(arquivo){

      googledrive::drive_download(file = arquivo,
                                  path = paste0("./", arquivo),
                                  overwrite = TRUE)

    },
    .progress = TRUE)

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
  .init = dados_rhinellas,
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

  })

dados_rhinellas

## ESEC ----

### Importando ----

uc_capivara <- sf::st_read("uc_municipal_ceuc_outros_construcao.shp") |>
  dplyr::filter(nome |> stringr::str_detect("Capivara"))

### Visualizar ----

uc_capivara

ggplot() +
  geom_sf(data = uc_capivara, color = "black")

## Imagem de satélite ----

### Importando ----

img_sat <- terra::rast("imagem_uc_capivara.tif")

### Visualizando ----

img_sat

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  geom_sf(data = uc_capivara, color = "red", fill = NA, linewidth = 1)
