# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(parzer)

library(geosphere)

library(sf)

library(maptiles)

library(tidyterra)

library(adehabitatHR)

library(ggnewscale)

library(ggspatial)

library(ggview)

# Dados ----

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

## Completar coordenadas ----

dados_rhinellas <- purrr::reduce(
  .x = 1:nrow(dados_rhinellas),
  .f = \(dados, x){

    if(is.na(dados$Longitude[x]) || is.na(dados$Latitude[x])){

      dest <- geosphere::destPoint(
        p = c(dados$Longitude[x - 1], dados$Latitude[x - 1]),
        b = dados$`Ângulo de virada`[x - 1],
        d = dados$`Distancia (m)`[x - 1])

      dados$Longitude[x] <- dest[1]

      dados$Latitude[x]  <- dest[2]

      }

    dados

  },

  .init = dados_rhinellas)
