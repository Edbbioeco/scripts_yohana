# Pacotes ----

library(bib2df)

library(tidyverse)

# Importar, unir e exportar o .bibs ----

purrr::map(list.files(pattern = ".bib$"),
           ~bib2df::bib2df(.x)) |>
  dplyr::bind_rows() |>
  dplyr::distinct(BIBTEXKEY, .keep_all = TRUE) |>
  bib2df::df2bib("bib_unido.bib")

# Liostar autores e Bibtextkeys ---

bib2df::bib2df("bib_unido.bib") |>
  dplyr::select(BIBTEXKEY, AUTHOR, YEAR, TITLE) |>
  dplyr::mutate(AUTHOR = purrr::map_chr(AUTHOR, \(x){

    paste(x, collapse = "; ")

    })) |>
  as.data.frame()
