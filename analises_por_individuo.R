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

