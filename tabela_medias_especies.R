# Pacotes ----

library(tidyverse)

library(readxl)

library(flextable)

# Dados ----

## Listando arquivos ----

arquivos <- list.files(pattern = "^Dados ")

arquivos <- arquivos[!arquivos |> stringr::str_detect("discos|movimentação|.csv")]

arquivos

## Vetor com o nome das espécies ----

sps <- arquivos |>
  stringr::str_split(pattern = " ",
                     simplify = TRUE) |>
  tibble::as_tibble() |>
  tidyr::unite(col = "especie",
               2:3,
               sep = "_") |>
  dplyr::pull(especie) |>
  stringr::str_remove_all(".xlsx")

sps

## Importando ----

imp_arq <- function(arquivos, sps){

  arquivo <- readxl::read_xlsx(arquivos)

  assign(paste0("dados_", sps),
         arquivo,
         envir = globalenv())

}

purrr::map2(arquivos,
            sps,
            imp_arq)

## Visualizando ----

ls(pattern = "dados_") |>
  mget(envir = globalenv())

ls(pattern = "dados_") |>
  mget(envir = globalenv()) |>
  dplyr::glimpse()

# Tabela ----

## Unindo os dados ----
