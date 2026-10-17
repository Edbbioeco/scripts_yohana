# Pacotes ----

library(googledrive)

library(readxl)

library(tidyverse)

library(parzer)

library(geosphere)

library(sf)

library(adehabitatHR)

library(janitor)

library(reshape2)

library(lmerTest)

library(performance)

library(lme4)

library(ggeffects)

library(DHARMa)

# Dados ----

## Dados de movimentação ----

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
