# Pacotes ----

library(googledrive)

library(readxl)

library(tidyverse)

library(ggimage)

# Dados ----

## Ativar o Google Drive ----

googledrive::drive_auth("edson.nilton@ufpe.br")

## Importar ----

uso <- "Artigo pererecas" |>
  googledrive::drive_get() |>
  googledrive::drive_ls() |>
  dplyr::filter(name |> stringr::str_detect("Uso")) |>
  dplyr::pull(id) |>
  googledrive::drive_download(file = _,
                              path = tempfile(fileext = ".xlsx")) |>
  dplyr::pull(local_path) |>
  readxl::read_xlsx() |>
  dplyr::filter(Espécie %in% c("Phyllomedusa tetraploidea",
                               "Boana faber"))

## Visualizar ----

uso

uso |> dplyr::glimpse()

# Gráfico ----

## Tratar ----

uso_trat <- uso |>
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      names_to = "Habitat",
                      values_to = "Presença") |>
  dplyr::summarise(`Número de indivíduos` = Presença |> sum(),
                   .by = c(Espécie, Habitat)) |>
  dplyr::mutate(Espécie = dplyr::if_else(
    Espécie |> stringr::str_detect("americanus"),
    "Odontophrynus asper",
    Espécie) |>
      forcats::fct_relevel(c("Phyllomedusa tetraploidea",
                             "Boana faber")),
    Habitat = dplyr::case_match(

      Habitat,
      "Árvore" ~ "Tree",
      "Bambu" ~ "Bamboo",
      "Borda de brejo" ~ "Marsh edge",
      "Chão" ~ "Clean ground",
      "Embaixo da serrapilheira" ~ "Under leaflitter",
      "Embaixo de tronco" ~ "Under fallen tree",
      "Enterrado" ~ "Buried",
      "Toca no solo" ~ "Burrowed in the ground"

    ) |>
      forcats::fct_relevel(c(

        "Buried",
        "Burrowed in the ground",
        "Clean ground",
        "Under leaflitter",
        "Under fallen tree",
        "Tree",
        "Bamboo",
        "Marsh edge"

      ))) |>
  as.data.frame()

uso_trat

## Imagens transparentes ----

imagens_sps <- tibble::tibble(x = 6,
                              y = 7.75,
                              sps = paste0(c("phyllomedusa tetraploidea",
                                             "boana faber"),
                                           ".png"),
                              Espécie = c("Phyllomedusa tetraploidea",
                                          "Boana faber"))

imagens_sps
