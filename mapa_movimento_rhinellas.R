# Pacotes ----

library(tidyverse)

library(readxl)

library(magrittr)

library(parzer)

library(geosphere)

library(terra)

library(tidyterra)

library(sf)

library(adehabitatHR)

library(ggspatial)

library(ggmagnify)

library(ggview)

# Dados ----

## Coordenadas ----

### Importar ----

dados_rhinellas <- purrr::map_dfr(
  c("Rhinella icterica", "Rhinella crucifer"),
  \(rhinella){

    readxl::read_xlsx(paste0("Dados ", rhinella, ".xlsx")) |>
      dplyr::mutate(Espécie = rhinella)

    },
  .progress = TRUE)

### Visualizar ----

dados_rhinellas

dados_rhinellas |> dplyr::glimpse()

### Converter coordenadas ----

dados_rhinellas %<>%
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

dados_rhinellas

### Completar coordenadas ----

dados_rhinellas <- purrr::reduce(
  .x = 1:nrow(dados_rhinellas),
  .f = \(dados, x){

    if(is.na(dados$Longitude[x]) || is.na(dados$Latitude[x])){

      dest <- geosphere::destPoint(
        p = c(dados$Longitude[x - 1], dados$Latitude[x - 1]),
        b = dados$`Angulo de virada`[x - 1],
        d = dados$`Distância (m)`[x - 1])

      dados$Longitude[x] <- dest[1]

      dados$Latitude[x]  <- dest[2]

      }

    dados

  },

  .init = dados_rhinellas)

dados_rhinellas

## Imagem de satélite ----

### Importar ----

sat <- terra::rast("imagem_satelite_esec.tif")

### Visualizar ----

sat

ggplot() +
  tidyterra::geom_spatraster_rgb(data = sat)

# Análise de área de vida ----

## Transformas as coordenadas em shapefiles ----

dados_rhinellas_sf <- dados_rhinellas |>
  sf::st_as_sf(coords = c(2:3),
               crs = 4674)

dados_rhinellas_sf

ggplot() +
  tidyterra::geom_spatraster_rgb(data = sat) +
  geom_sf(data = dados_rhinellas_sf, aes(color = Espécie))

## Calcular área de vida ----

area_rhinella <- purrr::map_dfr(
  c("Rhinella icterica", "Rhinella crucifer"),
  \(rhinella){

    id <- dados_rhinellas_sf |>
      dplyr::filter(Espécie == rhinella) |>
      dplyr::pull(ID) |>
      unique()

    purrr::map(
      id,
      \(ind){

        KDE95 <- dados_rhinellas_sf |>
          dplyr::filter(Espécie == rhinella & ID == ind) |>
          sf::as_Spatial() |>
          adehabitatHR::kernelUD(h = "href",
                                      extent = 3) |>
          adehabitatHR::getverticeshr(percent = 90) |>
          sf::st_as_sf() |>
          dplyr::mutate(Tipo = "KDE 95%",
                        Espécie = rhinella,
                        Indivíduo = ind)

        kde50 <- dados_rhinellas_sf |>
          dplyr::filter(Espécie == rhinella & ID == ind) |>
          sf::as_Spatial() |>
          adehabitatHR::kernelUD(h = "href",
                                      extent = 3) |>
          adehabitatHR::getverticeshr(percent = 50) |>
          sf::st_as_sf() |>
          dplyr::mutate(Tipo = "KDE 50%",
                        Espécie = rhinella,
                        Indivíduo = ind)

        dplyr::bind_rows(kde50, KDE95)

        }
      )

    },
  .progress = TRUE)

area_rhinella

## Calcular os centroides de cada indivíduo ----

centroides <- area_rhinella |>
  sf::st_make_valid() |>
  dplyr::group_by(Espécie, Indivíduo) |>
  dplyr::summarise(.groups = "drop") |>
  sf::st_centroid()

centroides

## Mapas ----

### Rhinella icterica ----

zoom_icterica <- tibble::tibble(ids = list(c("RI4", "RI6"), "RI5"),
                         x0 = c(-51.828, -51.8282),
                         y0 = c(-25.0975, -25.0939),
                         largura = c(0.003, 0.0008)) |>
  purrr::pmap(
    \(ids, x0, y0, largura){

      bb <- area_rhinella |>
        dplyr::filter(Indivíduo %in% ids) |>
        sf::st_bbox()

      from <- c(xmin = bb[["xmin"]],
                xmax = bb[["xmax"]],
                ymin = bb[["ymin"]],
                ymax = bb[["ymax"]])

      altura <- largura * (from[["ymax"]] - from[["ymin"]]) / (from[["xmax"]] - from[["xmin"]])

    ggmagnify::geom_magnify(from = from,
                            to = c(xmin = x0,
                                   xmax = x0 + largura,
                                   ymin = y0 - altura,
                                   ymax = y0))

    },
    .progress = TRUE)

mapa_icterica <- purrr::map(
  dados_rhinellas_sf |>
    dplyr::filter(Espécie == "Rhinella icterica") |>
    dplyr::pull(ID) |>
    unique(),
  \(ind){

    ggplot() +
      geom_sf(data = dados_rhinellas_sf |>
                dplyr::rename("Indivíduo" = ID) |>
                dplyr::filter(Indivíduo == ind),
              aes(color = "Individuals")) +
      geom_sf(data = area_rhinella |>
                dplyr::filter(Indivíduo == ind),
              aes(color = Tipo),
              fill = "transparent",
              linewidth = 1) +
      scale_color_manual(values = c("black", "royalblue", "orange")) +
      scale_fill_viridis_d(guide = guide_legend(
        title = "Individuals",
        title.position = "top",
        title.hjust = 0.5
        )) +
      labs(x = NULL,
           y = NULL,
           color = NULL) +
      ggspatial::annotation_scale(location = "br",
                                  text_col = "black",
                                  text_cex = 1.5,
                                  bar_cols = c("black", "gold")) +
      facet_wrap(~Indivíduo) +
      theme_bw() +
      theme(axis.text = element_text(color = "black", size = 10),
            axis.text.x = element_text(angle = 90,
                                       hjust = 1, vjust = 0.5),
            legend.text = element_text(color = "black", size = 20),
            legend.title = element_text(color = "black", size = 20),
            strip.text = element_text(color = "black", size = 20),
            strip.background = element_rect(color = "black",
                                            linewidth = 1.5),
            panel.border = element_rect(color = "black", linewidth = 1.5),
            plot.title = element_text(color = "black", size = 20,
                                      face = "italic"))

    },
  .progress = TRUE) |>
  patchwork::wrap_plots(guides = "collect") &
  theme(legend.position = "bottom")

mapa_icterica &
  ggview::canvas(height = 10, width = 16)

ggsave(plot = mapa_icterica,
       filename = "mapa_area_vida_rhinella_icterica.png",
       height = 10, width = 16)

### Rhinella crucifer ----

zoom_crucifer <- tibble::tibble(ids = list(c("RC1", "RC2"),
                                           c("RC3", "RC4")),
                                x0 = c(-51.832, -51.832),
                                y0 = c(-25.075, -25.0815),
                                largura = c(0.006, 0.006)) |>
  purrr::pmap(
    \(ids, x0, y0, largura){

      bb <- area_rhinella |>
        dplyr::filter(Indivíduo %in% ids) |>
        sf::st_bbox()

      from <- c(xmin = bb[["xmin"]],
                xmax = bb[["xmax"]],
                ymin = bb[["ymin"]],
                ymax = bb[["ymax"]])

      altura <- largura * (from[["ymax"]] - from[["ymin"]]) / (from[["xmax"]] - from[["xmin"]])

      ggmagnify::geom_magnify(from = from,
                              to = c(xmin = x0,
                                     xmax = x0 + largura,
                                     ymin = y0 - altura,
                                     ymax = y0))

    },
    .progress = TRUE)

mapa_crucifer <- purrr::map(
  dados_rhinellas_sf |>
    dplyr::filter(Espécie == "Rhinella crucifer") |>
    dplyr::pull(ID) |>
    unique(),
  \(ind){

    ggplot() +
      geom_sf(data = dados_rhinellas_sf |>
                dplyr::rename("Indivíduo" = ID) |>
                dplyr::filter(Indivíduo == ind),
              aes(color = "Individuals")) +
      geom_sf(data = area_rhinella |>
                dplyr::filter(Indivíduo == ind),
              aes(color = Tipo),
              fill = "transparent",
              linewidth = 1) +
      scale_color_manual(values = c("black", "royalblue", "orange")) +
      scale_fill_viridis_d(guide = guide_legend(
        title = "Individuals",
        title.position = "top",
        title.hjust = 0.5
      )) +
      labs(x = NULL,
           y = NULL,
           color = NULL) +
      ggspatial::annotation_scale(location = "br",
                                  text_col = "black",
                                  text_cex = 1.5,
                                  bar_cols = c("black", "gold")) +
      facet_wrap(~Indivíduo) +
      theme_bw() +
      theme(axis.text = element_text(color = "black", size = 10),
            axis.text.x = element_text(angle = 90,
                                       hjust = 1, vjust = 0.5),
            legend.text = element_text(color = "black", size = 20),
            legend.title = element_text(color = "black", size = 20),
            strip.text = element_text(color = "black", size = 20),
            strip.background = element_rect(color = "black",
                                            linewidth = 1.5),
            panel.border = element_rect(color = "black", linewidth = 1.5),
            plot.title = element_text(color = "black", size = 20,
                                      face = "italic"))

  },
  .progress = TRUE) |>
  patchwork::wrap_plots(guides = "collect") &
  theme(legend.position = "bottom")

mapa_crucifer &
  ggview::canvas(height = 10, width = 16)

ggsave(plot = mapa_crucifer,
       filename = "mapa_area_vida_rhinella_crucifer.png",
       height = 10, width = 16)
