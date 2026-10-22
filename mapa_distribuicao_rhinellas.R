# Pacotes ----

library(googledrive)

library(readxl)

library(tidyverse)

library(geobr)

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

## Importar ----

dados_rhinellas <- purrr::map_dfr(
  c("Rhinella icterica", "Rhinella crucifer"),
  \(rhinella){

    readxl::read_xlsx(paste0("Dados ", rhinella, ".xlsx")) |>
      dplyr::mutate(Espécie = rhinella)

  },
  .progress = TRUE)

## Visualizar ----

dados_rhinellas

dados_rhinellas |> dplyr::glimpse()

## Converter coordenadas ----

dados_rhinellas %<>%
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

dados_rhinellas
