# Pacotes ----

library(readxl)

library(tidyverse)

library(parzer)

library(geosphere)

library(sf)

library(adehabitatHR)

library(janitor)

library(reshape2)

library(ggview)

library(lmerTest)

library(performance)

library(lme4)

library(ggeffects)

library(DHARMa)

# Dados ----

## Dados de movimentação ----

### Importar ----

mov <- purrr::map_dfr(
  c("Rhinella crucifer",
    "Rhinella icterica"),
  \(sps){

    readxl::read_xlsx(paste0("Dados ",
                             sps,
                             ".xlsx")) |>
      dplyr::mutate(Espécie = sps)

    },
  .progress = TRUE)

mov

### Visualizar ----

mov

mov |> dplyr::glimpse()
