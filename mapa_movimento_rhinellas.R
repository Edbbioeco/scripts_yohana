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

library(ggnewscale)

library(ggspatial)

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
  dplyr::group_by(Espécie, Indivíduo) |>
  dplyr::summarise(area = sum(area),
                   .groups = "drop") |>
  sf::st_centroid()

centroides

## Mapas ----

insets <- tibble::tibble(ids = list(c("RI4", "RI6"), "RI5"),
                         x0 = c(-51.828, -51.8282),
                         y0 = c(-25.0975, -25.0939),
                         largura = c(0.003, 0.0008)) |>
  purrr::pmap(
    \(ids, x0, y0, largura){

      bb <- area_rhinella |>
        dplyr::filter(Indivíduo %in% ids) |>
        sf::st_bbox()

      margem <- 0.1 * max(bb[["xmax"]] - bb[["xmin"]], bb[["ymax"]] - bb[["ymin"]])

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

mapa_icterica <- area_rhinella %>%
  ggplot() +
  geom_sf(data = ~. |>
            dplyr::filter(Espécie == "Rhinella icterica"),
          aes(color = Tipo),
          fill = "transparent",
          linewidth = 1) +
  geom_sf(data = dados_rhinellas_sf |>
            dplyr::filter(Espécie == "Rhinella icterica"),
          aes(color = "Individuals")) +
  scale_color_manual(values = c("red", "royalblue", "orange")) +
  geom_sf_text(data = centroides |>
                 dplyr::filter(Espécie == "Rhinella icterica"),
                aes(label = Indivíduo),
               fontface = "bold") +
  labs(x = NULL,
       y = NULL,
       color = NULL,
       title = "Rhinella icterica") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 1.5),
        plot.title = element_text(color = "black", size = 20,
                                  face = "italic")) +
  insets +
  ggview::canvas(height = 10, width = 12)

mapa_icterica

ggsave(filename = "mapa_area_vida_rhinella_icterica.png",
       height = 10, width = 12)
