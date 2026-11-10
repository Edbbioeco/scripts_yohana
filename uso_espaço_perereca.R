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
