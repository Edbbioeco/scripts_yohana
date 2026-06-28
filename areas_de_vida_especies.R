# Pacotes ----

library(readxl)

library(tidyverse)

library(parzer)

library(geosphere)

library(sf)

library(maptiles)

library(tidyterra)

library(adehabitatHR)

library(ggview)

library(ggspatial)

# Dados ----

## Dados de movimento ----

### Importando ----

importar_dados <- function(especies){

  dados <- readxl::read_xlsx(paste0("Dados ", especies, ".xlsx")) |>
    dplyr::mutate(Espécie = especies)

  assign(paste0("dados_", especies |> stringr::word(2)),
         dados,
         envir = globalenv())

}

especies <- c("Boana faber",
              "Phyllomedusa tetraploidea",
              "Rhinella crucifer",
              "Rhinella icterica",
              "Odontophrynus asper",
              "Leptodactylus mystacinus")

purrr::map(especies, importar_dados)

### Visualizando ----

ls(pattern = "dados_") |>
  mget(envir = globalenv())

### Unindo os dados ----

sps_dados <- ls(pattern = "dados_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

sps_dados

#### Convertendo as coordenadas ----

sps_dados <- sps_dados |>
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

sps_dados

### Completando as coordenadas ----

convertendo_coords <- function(x){

  if(sps_dados$Longitude[x] |> is.na() | sps_dados$Latitude[x] |> is.na()){

    dest <- geosphere::destPoint(c(sps_dados$Longitude[x-1],
                                   sps_dados$Latitude[x-1]),
                                 sps_dados$`Angulo de virada`[x-1],
                                 sps_dados$`Distância (m)`[x-1])

    sps_dados$Longitude[x] <<- dest[1]

    sps_dados$Latitude[x] <<- dest[2]

  }

}

purrr::map(1:nrow(sps_dados), convertendo_coords)

sps_dados |> as.data.frame()

### Shapefile do trajeto total ----

sf_dados <- sps_dados |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674)

sf_dados

ggplot() +
  geom_sf(data = sf_dados, aes(color = Espécie))

## Imagem de satélite ----

### Criando um shapefile da área que abrange os pontos ----

area_sf <- c(xmin = -51.83960,
             ymin = -25.10522,
             xmax = -51.81667,
             ymax = -25.06429) |>
  sf::st_bbox(crs = 4674) |>
  sf::st_as_sfc()

area_sf

ggplot() +
  geom_sf(data = area_sf) +
  geom_sf(data = sf_dados, aes(color = Espécie), linewidth = 1) +
  coord_sf(expand = TRUE)

### Importando ----

maptiles::get_providers()

img_sat <- maptiles::get_tiles(area_sf,
                               provider = "Esri.WorldImagery",
                               zoom = 16)

img_sat

### Visualizando ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  geom_sf(data = sf_dados, aes(color = Espécie), linewidth = 1) +
  coord_sf(expand = TRUE)

# Mínimo Polígono convexo ----

## Área do mínimo polígono convexo ----

mcp_sps <- function(sps){

  mcp_100 <- sf_dados |>
    dplyr::filter(Espécie == sps) |>
    sf::as_Spatial() |>
    adehabitatHR::mcp(percent = 100) |>
    sf::st_as_sf() |>
    dplyr::mutate(Percent = "100%",
                  Espécie = sps)

  mcp_095 <- sf_dados |>
    dplyr::filter(Espécie == sps) |>
    sf::as_Spatial() |>
    adehabitatHR::mcp(percent = 95) |>
    sf::st_as_sf() |>
    dplyr::mutate(Percent = "095%",
                  Espécie = sps)

  mcps <- dplyr::bind_rows(mcp_100,
                           mcp_095)

  assign(paste0("mcp_sps_", sps),
         mcps,
         envir = globalenv())

}

sps <- sf_dados |>
  dplyr::pull(Espécie) |>
  unique()

sps

purrr::map(sps, mcp_sps)

sps_mcp <- ls(pattern = "mcp_sps_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

sps_mcp

## Mapa ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  geom_sf(data = sps_mcp, aes(color = Percent), fill = NA,
          size = 0.75) +
  facet_wrap(~Espécie) +
  coord_sf(expand = FALSE) +
  scale_color_manual(values = c("royalblue", "orange"),
                     name = NULL) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              text_col = "gold",
                              bar_cols = c("black", "gold"),
                              height = unit(0.25, "cm"),
                              width_hint = 0.35) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        strip.text = element_text(color = "black", size = 15, face = "italic"),
        legend.text = element_text(color = "black", size = 15)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "prancha_mcp.png", height = 10, width = 12)

# Kernel Density Estimator ----

## Área do mínimo polígono convexo ----

kde_sps <- function(sps){

  kde_90 <- sf_dados |>
    dplyr::filter(Espécie == sps) |>
    sf::as_Spatial() |>
    adehabitatHR::kernelUD(h = "href",
                           extent = 3.75) |>
    adehabitatHR::getverticeshr(percent = 95) |>
    sf::st_as_sf() |>
    dplyr::mutate(Percent = "95%",
                  Espécie = sps[1])

  kde_50 <- sf_dados |>
    dplyr::filter(Espécie == sps) |>
    sf::as_Spatial() |>
    adehabitatHR::kernelUD(h = "href",
                           extent = 3.75) |>
    adehabitatHR::getverticeshr(percent = 50) |>
    sf::st_as_sf() |>
    dplyr::mutate(Percent = "50%",
                  Espécie = sps)

 kdes <- dplyr::bind_rows(kde_90,
                          kde_50)

  assign(paste0("kde_sps_", sps),
         kdes,
         envir = globalenv())

}

sps <- sf_dados |>
  dplyr::pull(Espécie) |>
  unique()

sps

purrr::map(sps, kde_sps)

sps_kde <- ls(pattern = "kde_sps_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

sps_kde |>
  as.data.frame() |>
  dplyr::select(2:4) |>
  dplyr::mutate(area = area |> round(10))

## Mapa ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  geom_sf(data = sps_kde, aes(color = Percent), fill = NA,
          size = 0.75) +
  facet_wrap(~Espécie) +
  coord_sf(expand = FALSE) +
  scale_color_manual(values = c("royalblue", "orange"),
                     name = NULL) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              text_col = "gold",
                              bar_cols = c("black", "gold"),
                              height = unit(0.25, "cm"),
                              width_hint = 0.35) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        strip.text = element_text(color = "black", size = 15, face = "italic"),
        legend.text = element_text(color = "black", size = 15)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "prancha_kde.png", height = 10, width = 12)
