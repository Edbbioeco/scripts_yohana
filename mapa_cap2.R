# Pacotes ----

library(geobr)

library(tidyverse)

library(sf)

library(terra)

library(tidyterra)

library(ggview)

library(ggspatial)

library(cowplot)

library(patchwork)

# Dados -----

## Brasil ----

### Importando ----

br <- geobr::read_state(year = 2019)

### Visualizando ----

br

ggplot() +
  geom_sf(data = br, color = "black", fill = "white")

## Paraná ----

### Importando ----

parana <- br |>
  dplyr::filter(name_state == "Paraná")

### Visualizando ----

parana

ggplot() +
  geom_sf(data = br, color = "black", fill = "white") +
  geom_sf(data = parana, color = "black", fill = "gray")

## Campina do Simão ----

### Importando ----

camp_sim <- geobr::read_municipality(year = 2019) |>
  dplyr::filter(name_muni == "Campina Do Simão")

### Visualizando ----

camp_sim

ggplot() +
  geom_sf(data = br, color = "black", fill = "white") +
  geom_sf(data = parana, color = "black", fill = "gray") +
  geom_sf(data = camp_sim, color = "darkgreen")

## ESEC ----

### Importando ----

uc_capivara <- sf::st_read("uc_municipal_ceuc_outros_construcao.shp") |>
  dplyr::filter(nome |> stringr::str_detect("Capivara"))

### Visualizando ----

uc_capivara

ggplot() +
  geom_sf(data = camp_sim, color = "darkgreen") +
  geom_sf(data = uc_capivara)

## Imagem de satélite ----

### Importando ----

img_sat <- terra::rast("imagem_satelite_esec.tif")

### Visualizando ----

img_sat

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  geom_sf(data = uc_capivara, color = "red", fill = NA, linewidth = 1)

## Pontos de coleta ----

### Lista de arquivos ----

arquivos <- list.files(pattern = "^Dados ")

arquivos <- arquivos[!arquivos |> stringr::str_detect("discos|movimentação|.csv")]

arquivos

### Importando ----

importar_coord <- function(arquivos){

  coordenadas <- readxl::read_xlsx(arquivos) |>
    dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                  Latitude = Latitude |> parzer::parse_lat())

  convertendo_coords <- function(x){

    if(coordenadas$Longitude[x] |> is.na() | coordenadas$Latitude[x] |> is.na()){

      dest <- geosphere::destPoint(c(coordenadas$Longitude[x-1],
                                     coordenadas$Latitude[x-1]),
                                   coordenadas$`Angulo de virada`[x-1],
                                   coordenadas$`Distância (m)`[x-1])

      coordenadas$Longitude[x] <<- dest[1]

      coordenadas$Latitude[x] <<- dest[2]

    }

  }

  purrr::map(1:nrow(coordenadas), convertendo_coords)

  assign(paste0("coord_", arquivos |> stringr::word(2)),
         coordenadas,
         envir = globalenv())

}

purrr::map(arquivos, importar_coord)

sps_coord <- ls(pattern = "coord_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

sps_coord

### Criando um shapefile de centroide por área ----

pontos_coleta <- sps_coord |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674) |>
  sf::st_join(uc_capivara)|>
  dplyr::group_by(nome) |>
  dplyr::summarise(geometry = geometry |>
                     sf::st_union() |>
                     sf::st_centroid()) |>
  tidyr::drop_na()

### Visualizando ----

pontos_coleta

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  geom_sf(data = uc_capivara, color = "red", fill = NA, linewidth = 1) +
  geom_sf(data = pontos_coleta, color = "gold")

# Mapa ----

## Mapa 1 ----

mapa1 <- ggplot() +
  geom_sf(data = br, fill = "white",
          color = "black", linewidth = 0.5) +
  geom_sf(data = parana, fill = "grey",
          color = "black", linewidth = 0.5) +
  geom_sf(data = camp_sim, fill = "forestgreen",
          color = "darkgreen", linewidth = 0.5) +
  scale_fill_manual(values = c("Brazil" = "white",
                               "Paraná" = "grey",
                               "Campina do Simão" = "forestgreen"),
                    breaks = c("Brazil", "Paraná", "Campina do Simão")) +
  labs(fill = NULL) +
  theme_void() +
  ggview::canvas(height = 10, width = 10)

mapa1

## Mapa 2 ----

mapa2 <- ggplot() +
  geom_sf(data = br, fill = "white",
          color = "black", linewidth = 0.5) +
  geom_sf(data = parana, fill = "grey",
          color = "black", linewidth = 0.5) +
  geom_sf(data = camp_sim, fill = "forestgreen",
          color = "darkgreen", linewidth = 0.5) +
  geom_sf(data = uc_capivara, color = "red", fill = NA, linewidth = 1) +
  coord_sf(xlim = c(-51.8951, -51.6376),
           ylim = c(-25.23296, -24.9872),
           label_graticule = "NWS") +
  scale_x_continuous(breaks = seq(-51.8951, -51.6376, 0.1)) +
  labs(fill = NULL) +
  theme_void() +
  ggview::canvas(height = 10, width = 9.5)

mapa2

## Mapa 3 ----

mapa3 <- ggplot() +
  geom_sf(data = br, aes(fill = "Brazil"),
          color = "black", linewidth = 1) +
  geom_sf(data = parana, aes(fill = "Paraná"),
          color = "black", linewidth = 1) +
  geom_sf(data = camp_sim, aes(fill = "Campina do Simão"),
          color = "darkgreen", linewidth = 1) +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  geom_sf(data = uc_capivara, aes(fill = "ESEC Municipal Capivara"),
          color = "red", linewidth = 1) +
  geom_sf(data = pontos_coleta, aes(fill = "Sampling locality"),
          color = "black", shape = 21, size = 7.5, stroke = 1) +
  scale_fill_manual(values = c("Brazil" = "white",
                               "Paraná" = "grey",
                               "Campina do Simão" = "forestgreen",
                               "ESEC Municipal Capivara" = NA,
                               "Sampling locality" = "gold"),
                    breaks = c("Brazil", "Paraná", "Campina do Simão",
                               "ESEC Municipal Capivara", "Sampling locality")) +
  coord_sf(expand = FALSE,
           xlim = c(-51.85547, -51.81153),
           ylim = c(-25.10551, -25.05574),
           label_graticule = "NWSE") +
  scale_x_continuous(breaks = seq(-51.85, -51.81458, 0.01)) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              text_col = "gold",
                              bar_cols = c("black", "gold"),
                              height = unit(0.5, "cm")) +
  labs(fill = NULL) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        plot.margin = grid::unit(c(0, 0, 0, 0), "cm"),
        legend.text = element_text(color = "black", size = 20),
        legend.position = "bottom") +
  ggview::canvas(height = 10, width = 10)

mapa3

## Inset map ----

cowplot::ggdraw(mapa3) +
  cowplot::draw_plot(mapa1,
                     x = 0.57,
                     y = 0.69,
                     height = 0.3,
                     width = 0.3) +
  cowplot::draw_plot(mapa2,
                     x = 0.17,
                     y = 0.19,
                     height = 0.25,
                     width = 0.25) +
  ggview::canvas(height = 14, width = 16)

ggsave(filename = "mapa_cap2.png",
       height = 14, width = 16)
