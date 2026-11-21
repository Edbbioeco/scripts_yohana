# Pacotes ----

library(googledrive)

library(tidyverse)

library(terra)

library(exifr)

library(tidyterra)

library(readxl)

library(shadowtext)

library(patchwork)

# Dados ----

## Autenticar Google Drive ----

googledrive::drive_auth("edson.nilton@ufpe.br")

## Importar imagens ----

imagens <- purrr::map(
  "Consultoria ciclo ambiental/Turvo, PR/Fotos abêia" |>
    googledrive::drive_get() |>
    googledrive::drive_ls() |>
    dplyr::pull(id),
  \(pasta){

    pasta |>
      googledrive::drive_ls() |>
      dplyr::filter(!name |> stringr::str_detect("_[2-5].jpeg$")) |>
      dplyr::pull(id) |>
      purrr::map(
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
        .progress = TRUE)

    },
  .progress = TRUE) |>
  purrr::flatten() |>
  purrr::set_names(
    purrr::map(
      "Consultoria ciclo ambiental/Turvo, PR/Fotos abêia" |>
        googledrive::drive_get() |>
        googledrive::drive_ls() |>
        dplyr::pull(id),
      \(pasta){

        pasta |>
          googledrive::drive_ls() |>
          dplyr::filter(!name |> stringr::str_detect("_[2-5].jpeg$")) |>
          dplyr::pull(name) |>
          stringr::str_remove_all("prancha_meli_|.jpeg") |>
          stringr::str_replace("_", " ")

        }
      ) |>
      purrr::flatten() |>
      purrr::list_c()
    )

imagens

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

## Alterar a ordem dos elementos ----

### Importar dados da taxonomia das espécies ----

tax <- "Consultoria ciclo ambiental/Turvo, PR" |>
  googledrive::drive_get() |>
  googledrive::drive_ls() |>
  dplyr::filter(name == "tax_meli.xlsx") |>
  dplyr::pull(id) |>
  googledrive::drive_download(file = _,
                              path = tempfile(fileext = ".xlsx")) |>
  dplyr::pull(local_path) |>
  readxl::read_xlsx() |>
  dplyr::mutate(especie = especie |>
                  stringr::str_remove_all("\\*"))

tax

tax |> dplyr::glimpse()
