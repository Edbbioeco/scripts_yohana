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
    dplyr::filter(name != "1794275042960.jpg") |>
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
  purrr::set_names("Recitech/CGH Cacatu/Fotos" |>
                     googledrive::drive_get() |>
                     googledrive::drive_ls() |>
                     dplyr::filter(name != "1794275042960.jpg") |>
                     dplyr::pull(name))

## Visualizar imagens ----

imagens

purrr::imap(
  imagens,
  \(imagem, nome){

    ggplot() +
      tidyterra::geom_spatraster_rgb(data = imagem) +
      labs(title = nome)

    },
  .progress = TRUE)

# Prancha ----

prancha <- purrr::map(
  imagens,
  \(imagem){

    ggplot() +
      tidyterra::geom_spatraster_rgb(data = imagem) +
      coord_cartesian(expand = FALSE) +
      theme_void()

  },
  .progress = TRUE) |>
  patchwork::wrap_plots() +
  patchwork::plot_annotation(tag_levels = "A") &
  theme(plot.tag.location = "panel",
        plot.tag.position = "topleft",
        plot.tag = element_text(color = "gold",
                                size = 30,
                                face = "bold"))

prancha
