# Pacotes ----

library(tidyverse)

library(terra)

library(patchwork)

library(ggview)

# Imagens ----

## Importar ----

imagens <- purrr::map(list.files(path = "./imagens_consultoria_08.06.2026/",
                                 full.names = TRUE),
                      ~terra::rast(.x) |>
                        terra::flip(direction = "vertical"),
                      .progress = TRUE) |>
  setNames(list.files(path = "./imagens_consultoria_08.06.2026/") |>
             stringr::str_replace_all("_", " ") |>
             stringr::str_remove_all(".jpeg|.png") |>
             stringr::word(start = 3,
                           end = 4))

imagens

## Visualizar ----

purrr::imap(imagens,
            ~ggplot() +
              tidyterra::geom_spatraster_rgb(data = .x) +
              labs(title = .y) +
              coord_equal(),
            .progress = TRUE)

# Prancha ----

## Data frame dos valores ----

df_letras <- purrr::pmap(list(imagens,
                              list.files(path = "./imagens_consultoria_08.06.2026/") |>
                                stringr::str_replace_all("_", " ") |>
                                stringr::str_remove_all(".jpeg|.png") |>
                                stringr::word(start = 3,
                                              end = 4),
                              LETTERS[1:length(imagens)]),
                         \(fotos_unidas, nome_especies, letras){

                           extenc <- fotos_unidas |> terra::ext()

                           x_pos <- extenc[2]/8

                           y_pos <- (extenc[2]/8)*7

                           tibble::tibble(x = x_pos,
                                          y = y_pos,
                                          letra = letras,
                                          id = nome_especies)

                         },
                         .progress = TRUE) |>
  dplyr::bind_rows()

df_letras

## Adicionar as letras às imagens ----

imagens_letras <- purrr::imap(imagens,
                              ~ggplot() +
                                tidyterra::geom_spatraster_rgb(data = .x) +
                                coord_sf(expand = FALSE) +
                                coord_equal() +
                                geom_text(data = df_letras |>
                                            dplyr::filter(id == .y),
                                          aes(x, y, label = letra),
                                          size = 10,
                                          color = "white",
                                          fontface = "bold") +
                                theme_void(),
                              .progress = TRUE) |>
  setNames(paste0("ggplot_", imagens |> names()))

imagens_letras

## Unir figuras ----

prancha <- imagens_letras |>
  patchwork::wrap_plots() +
  theme(plot.tag = element_text(face = "bold", size = 20, hjust = -0.5),
        plot.tag.position = c(0, 1))

prancha

prancha + ggview::canvas(height = 8, width = 12)

ggsave(filename = "consultoria_08.06.2026/prancha_especies.png",
       height = 8, width = 12)
