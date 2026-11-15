# Pacotes ----

library(googledrive)

library(tidyverse)

library(terra)

library(exifr)

library(tidyterra)

library(patchwork)

# Dados ----

## Autenticar Google Drive ----

googledrive::drive_auth("edson.nilton@ufpe.br")

## Importar imagens ----

imagens <- purrr::map(
  "Consultoria ciclo ambiental/Turvo, PR/Fotos" |>
    googledrive::drive_get() |>
    googledrive::drive_ls() |>
    dplyr::filter(name  |> stringr::str_detect("^prancha_herp_") &
                    !name |> stringr::str_detect("_[2-8]\\.jpg")) |>
    dplyr::pull(id),
  \(foto){

    imagem <- foto |>
      googledrive::drive_download(file = _,
                                  path = tempfile(fileext = ".jpg")) |>
      dplyr::pull(local_path) |>
      terra::rast()

    orientacao <- foto |>
      googledrive::drive_download(file = _,
                                  path = tempfile(fileext = ".jpg")) |>
      dplyr::pull(local_path) |>
      exifr::read_exif() |>
      purrr::pluck("Orientation")

    if(isTRUE(orientacao == 3)){

      imagem <- imagem |>
        terra::flip(direction = "vertical") |>
        terra::flip(direction = "horizontal")

    }

    imagem

  },
  .progress = TRUE) |>
  purrr::set_names("Consultoria ciclo ambiental/Turvo, PR/Fotos" |>
                     googledrive::drive_get() |>
                     googledrive::drive_ls() |>
                     dplyr::filter(name  |> stringr::str_detect("^prancha_herp_") &
                                     !name |> stringr::str_detect("_[2-8]\\.jpg")) |>
                     dplyr::pull(name) |>
                     stringr::str_remove_all("prancha_herp_|.jpg") |>
                     stringr::str_replace("_", " "))

## Visualizar imagens ----

imagens

purrr::imap(
  imagens,
  \(imagem, sps){

    ggplot() +
      tidyterra::geom_spatraster_rgb(data = imagem) +
      labs(title = sps)

    },
  .progress = TRUE)
