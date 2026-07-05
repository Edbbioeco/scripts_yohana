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

## Visualizar ----

dados

purrr::map(dados, dplyr::glimpse)

## Tratar para calcular os dados de abundância ----

dados_trat <- purrr::map(dados,
                         purrr::in_parallel(

                           ~.x |>
                             dplyr::summarise(Abundância = dplyr::n(),
                                              .by = c(Campanha,
                                                      `Nome científico`,
                                                      Grupo))
                         ),
                         .progress = TRUE) |>
  setNames(list.files(path = "./consultoria_04.07.2026/") |>
             stringr::str_remove_all("dados_|.xlsx"))

dados_trat
