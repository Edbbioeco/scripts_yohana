# Pacotes ----

library(googledrive)

library(tidyverse)

library(terra)

library(tidyterra)

library(patchwork)

# Dados ----

## Autenticar Google Drive ----

googledrive::drive_auth("edson.nilton@ufpe.br")

## Importar imagens ----

imagens <- purrr::map(
  "Recitech/CGH Cacatu/Fotos" |>
    googledrive::drive_get() |>
    googledrive::drive_ls() |>
    dplyr::pull(id),
  \(foto){

    foto |>
      googledrive::drive_download(file = _,
                                  path = tempfile(fileext = ".jpg")) |>
      dplyr::pull(local_path) |>
      terra::rast() |>
      terra::flip(direction = "vertical")

    },
  .progress = TRUE) |>
  purrr::set_names("Recitech/CGH Cacatu/Fotos" |>
                     googledrive::drive_get() |>
                     googledrive::drive_ls() |>
                     dplyr::pull(name))
