# Pacotes ----

library(googledrive)

library(tidyverse)

library(readxl)

library(janitor)

library(vegan)

library(flextable)

library(ggdendro)

# Dados ----

## Ativar Google Drive ----

googledrive::drive_auth("edson.nilton@ufpe.br")

## Importar ----

dados <- "Consultoria ciclo ambiental/Turvo, PR/Dados" |>
  googledrive::drive_get() |>
  googledrive::drive_ls() |>
  dplyr::filter(name |> stringr::str_detect("Planilha")) |>
  dplyr::pull(id) |>
  googledrive::drive_download(file = _,
                              path = tempfile(fileext = ".xlsx")) |>
  dplyr::pull(local_path) |>
  readxl::read_xlsx()

## Visualizar ----

dados

dados |> dplyr::glimpse()

## Tratar ----

dados <- dados |>
  dplyr::select(-1) |>
  dplyr::slice(-1) |>
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::everything(),
      .fns = ~dplyr::if_else(dplyr::row_number() == 2 & is.na(.x),
                             dplyr::lag(.x),
                             .x)
      )
    ) |>
  dplyr::slice(-1) |>
  janitor::row_to_names(row_number = 1) |>
  dplyr::rename("Área" = `ÁREA  DE REGISTRO`)

dados

dados |> dplyr::glimpse()

# Diversidade alfa ----

## Tratar os dados ----

dados_div <- dados |>
  dplyr::summarise(abundancia = dplyr::n(),
                   .by = c(`Fase de licenciamento`,
                           Área,
                           `Nome Científico`)) |>
  tidyr::pivot_wider(names_from = `Nome Científico`,
                     values_from = abundancia,
                     values_fill = 0)

dados_div

## Calcular a diversidade ----

div_alfa <- purrr::map_dfr(
  dados_div$`Fase de licenciamento`,
  \(fase){

    riq <- dados_div |>
      dplyr::filter(`Fase de licenciamento` == fase) |>
      dplyr::select(-`Fase de licenciamento`) |>
      tibble::column_to_rownames(var = Área) |>
      vegan::specnumber()

    shannon <-  dados_div |>
      dplyr::filter(`Fase de licenciamento` == fase) |>
      dplyr::select(-`Fase de licenciamento`) |>
      tibble::column_to_rownames(var = Área) |>
      vegan::diversity()

    tibble::tibble(`Fase de licenciamento` = fase,
                   Riqueza = riq,
                   `H'` = shannon)

  },
  .progress = TRUE)

div_alfa
