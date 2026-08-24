# Pacotes ----

library(readxl)

library(tidyverse)

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
