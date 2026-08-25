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

## Completar coordenadas ----

purrr::map(
  1:nrow(sps_mov_dados),
  \(x){

    if(sps_mov_dados$Longitude[x] |> is.na() | sps_mov_dados$Latitude[x] |> is.na()){

      dest <- geosphere::destPoint(
        c(sps_mov_dados$Longitude[x-1],
          sps_mov_dados$Latitude[x-1]),
        sps_mov_dados$`Angulo de virada`[x-1],
        sps_mov_dados$`Distância (m)`[x-1])

      sps_mov_dados$Longitude[x] <<- dest[1]

      sps_mov_dados$Latitude[x] <<- dest[2]

    }

  },
  .progress = TRUE)

sps_mov_dados |> as.data.frame()
