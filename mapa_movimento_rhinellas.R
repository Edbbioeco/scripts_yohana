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
