# Pacotes ----

library(tidyverse)

library(readxl)

library(magrittr)

library(parzer)

library(geosphere)

library(sf)

library(adehabitatHR)

library(writexl)

library(flextable)

# Dados ----

## Importar ----

especies <- c("Boana faber",
              "Phyllomedusa tetraploidea",
              "Rhinella crucifer",
              "Rhinella icterica",
              "Odontophrynus asper",
              "Leptodactylus mystacinus")

sps_mov_dados <- purrr::map_dfr(
  especies,
  ~readxl::read_xlsx(paste0("Dados ", .x, ".xlsx")) |>
    dplyr::mutate(Espécie = .x),
  .progress = TRUE)

## Visualizar ----

sps_mov_dados

sps_mov_dados |> dplyr::glimpse()

# Tratar coordenadas ----

## Converter as coordenadas para decimal ----

sps_mov_dados %<>%
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

sps_mov_dados
