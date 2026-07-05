# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(ggview)

library(ggdendro)

# Dados ----

## Importar ----

dados <- purrr::map(list.files(path = "./consultoria_04.07.2026/",
                               full.names = TRUE),
                    readxl::read_xlsx) |>
  setNames(list.files(path = "./consultoria_04.07.2026/") |>
             stringr::str_remove_all("dados_|.xlsx"))

dados

purrr::map(dados, dplyr::glimpse)
