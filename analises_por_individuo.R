# Pacotes ----

library(tidyverse)

library(readxl)

library(parzer)

library(sf)

library(geosphere)

library(adehabitatHR)

library(ggview)

library(flextable)

# Dados ----

## Registros de espécies ----

### Importar ----

reg <- purrr::map_dfr(
  c("Boana faber",
    "Phyllomedusa tetraploidea",
    "Rhinella crucifer",
    "Rhinella icterica",
    "Odontophrynus asper",
    "Leptodactylus mystacinus"),
  \(sps){

    readxl::read_xlsx(paste0("Dados ", sps, ".xlsx")) |>
      dplyr::mutate(Espécie = sps)

    }

  )

### Visualizar ----

reg

reg |> dplyr::glimpse()

### Converter dados de longitude e latitude ----

reg <- reg |>
  dplyr::mutate(Longitude = parzer::parse_lon(Longitude),
                Latitude  = parzer::parse_lat(Latitude))

reg

### Completar as coordenadas ----

reg_trat <- purrr::reduce(
  .x = 1:nrow(reg),
  .f = \(dados, x){

    if(is.na(dados$Longitude[x]) || is.na(dados$Latitude[x])){

      dest <- geosphere::destPoint(
        c(dados$Longitude[x - 1], dados$Latitude[x - 1]),
        dados$`Angulo de virada`[x - 1],
        dados$`Distância (m)`[x - 1])

      dados$Longitude[x] <- dest[1]

      dados$Latitude[x]  <- dest[2]

      }

    dados

    },

  .init = reg)

reg_trat
