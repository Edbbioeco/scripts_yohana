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
                     stringr::str_replace_all("_", " "))

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
  dplyr::filter(name == "lista_sps.xlsx") |>
  dplyr::pull(id) |>
  googledrive::drive_download(file = _,
                              path = tempfile(fileext = ".xlsx")) |>
  dplyr::pull(local_path) |>
  readxl::read_xlsx()

tax

tax |> dplyr::glimpse()

### Filtrar espécies ----

sps_lista <- tax |>
  dplyr::filter(especie %in% names(imagens)) |>
  dplyr::pull(especie) |>
  c("ovos Boana faber", "ninho Leptodactylidae")

sps_lista

### Alterar a ordem da lista ----

imagens <- sps_lista |>
  purrr::set_names() |>
  purrr::map(\(sps){

    imagens[[sps]]

    },
    .progress = TRUE)

imagens

# Prancha ----

pranchas <- 1:length(imagens) |>
  split(ceiling(1:length(imagens) / 4)) |>
  purrr::map(
    \(conjunto){

      purrr::map2(
        conjunto,
        LETTERS[1:length(conjunto)],
        \(id, letra){

            ggplot() +
              tidyterra::geom_spatraster_rgb(data = imagens[[id]]) +
              shadowtext::geom_shadowtext(data = data.frame(x = -Inf,
                                                            y = Inf,
                                                            label = letra),
                                          mapping = aes(x = x,
                                                        y = y,
                                                        label = label),
                                          hjust = -0.4,
                                          vjust = 1.4,
                                          size = 15,
                                          fontface = "bold",
                                          color = "gold",
                                          bg.color = "black",
                                          bg.r = 0.1) +
              coord_cartesian(expand = FALSE) +
              theme_void() +
              theme(aspect.ratio = 1)

            },
          .progress = TRUE) |>
        patchwork::wrap_plots(ncol = 2)

      },
    .progress = TRUE) |>
  purrr::set_names(paste0("prancha_",
                          1:4,
                          "_consultoria_12.11.2026"))

pranchas

# prancha <- purrr::map2(
#   imagens,
#   1:length(imagens),
#   \(imagem, id){
#
#     ggplot() +
#       tidyterra::geom_spatraster_rgb(data = imagem) +
#       shadowtext::geom_shadowtext(data = data.frame(x = -Inf,
#                                                     y = Inf,
#                                                     label = LETTERS[id]),
#                                   mapping = aes(x = x,
#                                                 y = y,
#                                                 label = label),
#                                   hjust = -0.4,
#                                   vjust = 1.4,
#                                   size = 15,
#                                   fontface = "bold",
#                                   color = "gold",
#                                   bg.color = "black",
#                                   bg.r = 0.1) +
#       coord_cartesian(expand = FALSE) +
#       theme_void() +
#       theme(aspect.ratio = 1)
#
#   },
#   .progress = TRUE) |>
#   patchwork::wrap_plots(ncol = 4)
#
# prancha

purrr::imap(
  pranchas,
  \(prancha, nome){

    ggsave(plot = prancha,
           filename = paste0(nome, ".png"),
           height = 16,
           width = 16)

    },
  .progress = TRUE)

purrr::map(
  list.files(pattern = "prancha_[1-4]_consultoria_12.11.2026.png"),
  \(arquivo){

    arquivo |>
      googledrive::drive_upload(path = "Consultoria ciclo ambiental/Turvo, PR",
                                name = arquivo,
                                overwrite = TRUE)

    },
  .progress = TRUE)

## Legenda das figuras ----

legendas <- 1:length(imagens) |>
  split(ceiling(1:length(imagens) / 4)) |>
  purrr::map(
    \(conjunto){

      purrr::map2_chr(
        LETTERS[1:length(conjunto)],
        conjunto,
        \(letra, id){

         stringr::str_glue("<b>[{letra}]</b> = <i>{imagens[id] |> names()}</i>;")

          }
        ) |>
        stringr::str_flatten(collapse = " ") %>%
        paste0("Espécies amostradas no estudo: ", .)

      },
    .progress = TRUE)

legendas

arquivo <- base::tempfile(fileext = ".html")

c('<meta charset="UTF-8">',
  legendas |>
    stringr::str_c("<p>", ... = _, "</p>")) |>
  base::enc2utf8() |>
  writeLines(arquivo, useBytes = TRUE)

rstudioapi::viewer(arquivo)
