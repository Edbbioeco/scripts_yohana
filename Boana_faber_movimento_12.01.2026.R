# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(parzer)

library(geosphere)

library(sf)

library(maptiles)

library(tidyterra)

library(adehabitatHR)

library(ggnewscale)

library(ggspatial)

library(ggview)

# Dados ----

## Coordenadas ----

### Importando ----

dados_boana <- readxl::read_xlsx("Dados Boana faber.xlsx")

### Visualizando ----

dados_boana

dados_boana |> dplyr::glimpse()

### Tratando as coordenadas ----

dados_boana %<>%
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

dados_boana |> as.data.frame()

### Completando as coordenadas ----

convertendo_coords <- function(x){

  if(dados_boana$Longitude[x] |> is.na() | dados_boana$Latitude[x] |> is.na()){

    dest <- geosphere::destPoint(c(dados_boana$Longitude[x-1],
                                   dados_boana$Latitude[x-1]),
                                 dados_boana$`Angulo de virada`[x-1],
                                 dados_boana$`Distância (m)`[x-1])

    dados_boana$Longitude[x] <<- dest[1]

    dados_boana$Latitude[x] <<- dest[2]

  }

}

purrr::walk(1:nrow(dados_boana), convertendo_coords)

dados_boana

### Transformando em shapefile ----

dados_boana_sf <- dados_boana |>
  dplyr::group_by(ID) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674) |>
  dplyr:::ungroup()

dados_boana_sf

dados_boana_sfline <- dados_boana_sf |>
  dplyr::group_by(ID) |>
  dplyr::summarise(do_union = FALSE) |>
  sf::st_cast("LINESTRING") |>
  dplyr:::ungroup()

ggplot() +
  geom_sf(data = dados_boana_sfline, aes(color = ID), linewidth = 1) +
  coord_sf(expand = TRUE)

## Imagem de satélite ----

### Criando um shapefile da área que abrange os pontos ----

area_sf <- dados_boana_sf |>
  sf::st_bbox() |>
  sf::st_as_sfc()

area_sf

ggplot() +
  geom_sf(data = area_sf) +
  geom_sf(data = dados_boana_sfline, aes(color = ID), linewidth = 1) +
  coord_sf(expand = TRUE)

### Importando ----

maptiles::get_providers()

img_sat <- maptiles::get_tiles(area_sf,
                               provider = "Esri.WorldImagery",
                               zoom = 18)

img_sat

### Visualizando ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  geom_sf(data = dados_boana_sfline, aes(color = ID), linewidth = 1) +
  coord_sf(expand = TRUE)

# Mínimo Polígono Convexo ----

## Por indivíduo ----

### Mínimo polígono convexo para cada espécie ----

mcp_boana <- function(id){

  mcp_100 <- dados_boana_sf |>
    dplyr::filter(ID == id) |>
    sf::st_transform(crs = 32725) |>
    sf::as_Spatial() |>
    adehabitatHR::mcp(percent = 100) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = 4674) |>
    dplyr::select(-id) |>
    dplyr::mutate(Percent = "100%",
                  ID = id)

  mcp_095 <- dados_boana_sf |>
    dplyr::filter(ID == id) |>
    sf::st_transform(crs = 32725) |>
    sf::as_Spatial() |>
    adehabitatHR::mcp(percent = 95) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = 4674) |>
    dplyr::select(-id) |>
    dplyr::mutate(Percent = "95%",
                  ID = id)

  assign(paste0("mcp_", id, "_100"),
         mcp_100,
         envir = globalenv())

  assign(paste0("mcp_", id, "_095"),
         mcp_095,
         envir = globalenv())

}

id <- paste0("B", 1:16)

id

purrr::walk(id, mcp_boana)

### Unindo os sahpefiles ----

mcp_boana <- ls(pattern = "mcp_B") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::mutate(ID = ID |> forcats::fct_relevel(c(paste0("B", 1:9),
                                                  paste0("B", 10:16))))

mcp_boana

### Mapa ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  geom_sf(data = mcp_boana, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 0.5) +
  scale_fill_manual(values = c("gold", "royalblue")) +
  scale_color_manual(values = c("gold", "royalblue")) +
  facet_wrap(~ID) +
  coord_sf(expand = FALSE,
           ylim = c(-25.07474, -25.07251),
           xlim = c(-51.83215, -51.83067)) +
  scale_x_continuous(breaks = seq(-51.83215, -51.83067, 0.001)) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              text_col = "gold",
                              bar_cols = c("black", "gold"),
                              height = unit(0.5, "cm"),
                              location = "tl") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 20),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "bottom") +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mcp_individuo_boana_faber_06.01.2026.png",
       height = 10, width = 12)

## Total para a espécie ----

### Mínimo polígono total da espécie ----

mcp_total_100 <- dados_boana_sf |>
  sf::st_transform(crs = 32725) |>
  sf::as_Spatial() |>
  adehabitatHR::mcp(percent = 100) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(Percent = "100%")

mcp_total_100

mcp_total_095 <- dados_boana_sf |>
  sf::st_transform(crs = 32725) |>
  sf::as_Spatial() |>
  adehabitatHR::mcp(percent = 95) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(Percent = "95%")

mcp_total_095

### Unindo os MCPs ----

mcp_especie <- ls(pattern = "mcp_total") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

mcp_especie

### Mapa ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  geom_sf(data = mcp_especie, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  scale_fill_manual(values = c("gold", "royalblue")) +
  scale_color_manual(values = c("gold", "royalblue")) +
  coord_sf(expand = FALSE,
           ylim = c(-25.07484, -25.07241),
           xlim = c(-51.83225, -51.83057)) +
  scale_x_continuous(breaks = seq(-51.83215, -51.83067, 0.001)) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              text_col = "gold",
                              bar_cols = c("black", "gold"),
                              height = unit(0.5, "cm")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.position = "bottom") +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mcp_especie_boana_faber_06.01.2026.png",
       height = 10, width = 12)

# Área de Vida ----

## Por indivíduo ----

### Mínimo polígono convexo para cada espécie ----

area_vida_boana <- function(id){

  kde_95 <- dados_boana_sf |>
    dplyr::filter(ID == id) |>
    sf::st_transform(crs = 32725) |>
    sf::as_Spatial() |>
    adehabitatHR::kernelUD(h = "href",
                           extent = 3) |>
    adehabitatHR::getverticeshr(percent = 95) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = 4674) |>
    dplyr::select(-id) |>
    dplyr::mutate(Percent = "095%",
                  ID = id)

  kde_50 <- dados_boana_sf |>
    dplyr::filter(ID == id) |>
    sf::st_transform(crs = 32725) |>
    sf::as_Spatial() |>
    adehabitatHR::kernelUD(h = "href",
                           extent = 3) |>
    adehabitatHR::getverticeshr(percent = 50) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = 4674) |>
    dplyr::select(-id) |>
    dplyr::mutate(Percent = "050%",
                  ID = id)

  assign(paste0("kde_", id, "_095"),
         kde_95,
         envir = globalenv())

  assign(paste0("kde_", id, "_050"),
         kde_50,
         envir = globalenv())

}

id <- paste0("B", 1:16)

id

purrr::walk(id, area_vida_boana)

### Unindo os sahpefiles ----

kde_boana <- ls(pattern = "kde_B") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

kde_boana

### Mapa ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  geom_sf(data = kde_boana, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  scale_fill_manual(values = c("gold", "royalblue")) +
  scale_color_manual(values = c("gold", "royalblue")) +
  facet_wrap(~ID) +
  coord_sf(expand = FALSE,
           ylim = c(-25.07484, -25.07241),
           xlim = c(-51.83225, -51.83057)) +
  scale_x_continuous(breaks = seq(-51.83215, -51.83067, 0.001)) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              text_col = "gold",
                              bar_cols = c("black", "gold"),
                              height = unit(0.5, "cm")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 20),
        strip.background = element_rect(color = "black", linewidth = 1),
        legend.position = "bottom") +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "kde_individuo_boana_faber_06.01.2026.png",
       height = 10, width = 12)

## Total para a espécie ----

### Mínimo polígono total da espécie ----

kde_total_095 <- dados_boana_sf |>
  sf::st_transform(crs = 32725) |>
  sf::as_Spatial() |>
  adehabitatHR::kernelUD(h = "href") |>
  adehabitatHR::getverticeshr(percent = 95) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(Percent = "100%")

kde_total_095

kde_total_050 <- dados_boana_sf |>
  sf::st_transform(crs = 32725) |>
  sf::as_Spatial() |>
  adehabitatHR::kernelUD(h = "href") |>
  adehabitatHR::getverticeshr(percent = 50) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(Percent = "95%")

kde_total_050

### Unindo os MCPs ----

kde_especie <- ls(pattern = "kde_total") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

kde_especie

### Mapa ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  geom_sf(data = kde_especie, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  scale_fill_manual(values = c("gold", "royalblue")) +
  scale_color_manual(values = c("gold", "royalblue")) +
  coord_sf(expand = FALSE,
           ylim = c(-25.07498, -25.07241),
           xlim = c(-51.832435, -51.83037)) +
  scale_x_continuous(breaks = seq(-51.83215, -51.83067, 0.001)) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              text_col = "gold",
                              bar_cols = c("black", "gold"),
                              height = unit(0.5, "cm")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.position = "bottom") +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "kde_especie_boana_faber_06.01.2026.png",
       height = 10, width = 12)
