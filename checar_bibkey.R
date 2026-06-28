# Pacotes ----

library(bib2df)

library(tidyverse)

# Dados ---

# Importar -----

bib <- bib2df::bib2df("C:/Users/LENOVO/OneDrive/Documentos/bib/export.bib")

## Visualizar ----

bib

bib |> dplyr::glimpse()

# Checar padrão ----

## Por ano ----

bib |>
  dplyr::select(BIBTEXKEY, AUTHOR, YEAR, TITLE) |>
  dplyr::filter(YEAR == 2020) |> as.data.frame()

## Por bibtextkey ----

bib |>
  dplyr::select(BIBTEXKEY, AUTHOR, YEAR, TITLE) |>
  dplyr::filter(BIBTEXKEY |> stringr::str_detect(""))

## Por autor ----

bib |>
  dplyr::select(BIBTEXKEY, AUTHOR, YEAR, TITLE) |>
  dplyr::filter(AUTHOR |> stringr::str_detect(""))

## Por autor ----

bib |>
  dplyr::select(BIBTEXKEY, AUTHOR, YEAR, TITLE) |>
  dplyr::filter(TITLE |> stringr::str_detect("mamm"))

