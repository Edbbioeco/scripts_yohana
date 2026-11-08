# Pacotes ----

library(googledrive)

library(tidyverse)

library(readxl)

library(vegan)

library(patchwork)

# Dados ----

## Autenticar Google Drive ----

googledrive::drive_auth("edson.nilton@ufpe.br")

## Immportar ----

dados <- purrr::map(
  "Recitech/CGH Cacatu" |>
    googledrive::drive_get() |>
    googledrive::drive_ls() |>
    dplyr::filter(name %in% c("indices cgh Cacatu - mastofauna- 1 a 2- L.O",
                              "Dados Mastofauna - CGH Cacatu")) |>
    dplyr::pull(id),
  \(arquivo){

    arquivo |>
      googledrive::drive_download(file = _,
                                  path = tempfile(fileext = ".xlsx")) |>
      dplyr::pull(local_path) |>
      readxl::read_xlsx()

    },
  .progress = TRUE) |>
  purrr::set_names(
    "Recitech/CGH Cacatu" |>
      googledrive::drive_get() |>
      googledrive::drive_ls() |>
      dplyr::filter(name %in% c("indices cgh Cacatu - mastofauna- 1 a 2- L.O",
                                "Dados Mastofauna - CGH Cacatu")) |>
      dplyr::pull(name) |>
      stringr::str_remove(".xlsx"))

## Visualizar ----

dados

dados |> dplyr::glimpse()

## Tratar ----

dados <- dados |>
  purrr::pluck(1) |>
  dplyr::select(Campanha, Área, Espécie:Quantidade) |>
  dplyr::mutate(Campanha = Campanha |>
                  stringr::str_remove("2- ") |>
                  stringr::str_to_title()) |>
  dplyr::bind_rows(
    dados |>
      purrr::pluck(2) |>
      dplyr::select(Campanha, Área, Espécie,
                    `Nome comum` = `Nome Vulgar`,
                    "Quantidade" = `Nº Ind.2`) |>
      dplyr::mutate(Campanha = Campanha |>
                      stringr::str_replace(stringr::fixed("/"),
                                           " "),
                    Área = Área |>
                      stringr::str_remove("TR."))
    )

dados

dados |> dplyr::glimpse()

# Curva de rarefação ----

## Calcular a riqueza ----

riq <- dados |>
  dplyr::select(1, 3, 5) |>
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Quantidade,
                     values_fill = 0,
                     values_fn = max) |>
  tibble::column_to_rownames(var = "Campanha") |>
  vegan::specaccum(method = "rarefaction") |>
  purrr::pluck("richness")

riq

## Calcular desvio padrão ----

sd <- dados |>
  dplyr::select(1, 3, 5) |>
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Quantidade,
                     values_fill = 0,
                     values_fn = max) |>
  tibble::column_to_rownames(var = "Campanha") |>
  vegan::specaccum(method = "rarefaction") |>
  purrr::pluck("sd")

sd

## Calcular jackknife 1 ----

jackk <- dados |>
  dplyr::select(1, 3, 5) |>
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Quantidade,
                     values_fill = 0,
                     values_fn = max) |>
  tibble::column_to_rownames(var = "Campanha") |>
  vegan::specpool() |>
  purrr::pluck("jack1")

jackk

## Criar data frame com as informações ----

df_rare <- tibble::tibble(Campanhas = 0:3,
                          Espécie = c(0, riq),
                          sd = c(0, sd[1] |> rep(3)))

df_rare
