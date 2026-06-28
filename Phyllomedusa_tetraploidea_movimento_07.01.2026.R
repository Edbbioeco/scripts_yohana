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

dados_phyllo <- readxl::read_xlsx("Dados Phyllomedusa tetraploidea.xlsx")

### Visualizando ----

dados_phyllo

dados_phyllo |> dplyr::glimpse()

### Tratando as coordenadas ----

dados_phyllo %<>%
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

dados_phyllo |> as.data.frame()

### Completando as coordenadas ----

convertendo_coords <- function(x){

  if(dados_phyllo$Longitude[x] |> is.na() | dados_phyllo$Latitude[x] |> is.na()){

    dest <- geosphere::destPoint(c(dados_phyllo$Longitude[x-1],
                                   dados_phyllo$Latitude[x-1]),
                                 dados_phyllo$`Angulo de virada`[x-1],
                                 dados_phyllo$`Distância (m)`[x-1])

    dados_phyllo$Longitude[x] <<- dest[1]

    dados_phyllo$Latitude[x] <<- dest[2]

  }

}

purrr::walk(1:nrow(dados_phyllo), convertendo_coords)

dados_phyllo

### Transformando em shapefile ----

dados_phyllo_sf <- dados_phyllo |>
  dplyr::group_by(ID) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674) |>
  dplyr:::ungroup()

dados_phyllo_sf

dados_phyllo_sfline <- dados_phyllo_sf |>
  dplyr::group_by(ID) |>
  dplyr::summaPTse(do_union = FALSE) |>
  sf::st_cast("LINESTPTNG") |>
  dplyr:::ungroup()

ggplot() +
  geom_sf(data = dados_phyllo_sfline, aes(color = ID), linewidth = 1) +
  coord_sf(expand = TRUE)

## Imagem de satélite ----

### CPTando um shapefile da área que abrange os pontos ----

area_sf <- dados_phyllo_sf |>
  sf::st_bbox() |>
  sf::st_as_sfc()

area_sf

ggplot() +
  geom_sf(data = area_sf) +
  geom_sf(data = dados_phyllo_sfline, aes(color = ID), linewidth = 1) +
  coord_sf(expand = TRUE)

### Importando ----

maptiles::get_providers()

img_sat <- maptiles::get_tiles(area_sf,
                               provider = "Esri.WorldImagery",
                               zoom = 15)

img_sat

### Visualizando ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  geom_sf(data = dados_phyllo_sfline, aes(color = ID), linewidth = 1) +
  coord_sf(expand = TRUE)

# Mínimo Polígono Convexo ----

## Por indivíduo ----

### Mínimo polígono convexo para cada espécie ----

mcp_phyllo <- function(id){

  mcp_100 <- dados_phyllo_sf |>
    dplyr::filter(ID == id) |>
    sf::st_transform(crs = 32725) |>
    sf::as_Spatial() |>
    adehabitatHR::mcp(percent = 100) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = 4674) |>
    dplyr::select(-id) |>
    dplyr::mutate(Percent = "100%",
                  ID = id)

  mcp_095 <- dados_phyllo_sf |>
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

id <- paste0("PT", 1:7)

id

purrr::walk(id, mcp_phyllo)

### Unindo os sahpefiles ----

mcp_phyllo <- ls(pattern = "mcp_PT") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

mcp_phyllo

mcp_phyllo_PT1 <- ls(pattern = "mcp_PT1") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

mcp_phyllo_PT1

mcp_phyllo_PT2 <- ls(pattern = "mcp_PT2") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

mcp_phyllo_PT2

mcp_phyllo_PT3 <- ls(pattern = "mcp_PT3") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

mcp_phyllo_PT3

mcp_phyllo_PT4 <- ls(pattern = "mcp_PT4") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

mcp_phyllo_PT4

### Mapa ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  geom_sf(data = mcp_phyllo_PT1, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  geom_sf(data = mcp_phyllo_PT2, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  geom_sf(data = mcp_phyllo_PT3, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  geom_sf(data = mcp_phyllo_PT4, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  scale_fill_manual(values = c("gold", "royalblue")) +
  scale_color_manual(values = c("gold", "royalblue")) +
  coord_sf(expand = FALSE) +
  #scale_x_continuous(breaks = seq(-51.829, -51.823, 0.003)) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              text_col = "gold",
                              bar_cols = c("black", "gold"),
                              height = unit(0.5, "cm")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.position = "bottom") +
  ggview::canvas(height = 10, width = 12)

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  geom_sf(data = mcp_phyllo, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  scale_fill_manual(values = c("gold", "royalblue")) +
  scale_color_manual(values = c("gold", "royalblue")) +
  facet_wrap(~ID) +
  coord_sf(expand = FALSE) +
  #scale_x_continuous(breaks = seq(-51.829, -51.823, 0.003)) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              text_col = "gold",
                              bar_cols = c("black", "gold"),
                              height = unit(0.5, "cm")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        stPTp.text = element_text(color = "black", size = 20),
        stPTp.background = element_rect(color = "black", linewidth = 1),
        legend.position = "bottom") +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mcp_individuo_phyllomedusa_tetraploidea_07.01.2026.png",
       height = 10, width = 12)

## Total para a espécie ----

### Mínimo polígono total da espécie ----

mcp_total_100 <- dados_phyllo_sf |>
  sf::st_transform(crs = 32725) |>
  sf::as_Spatial() |>
  adehabitatHR::mcp(percent = 100) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(Percent = "100%")

mcp_total_100

mcp_total_095 <- dados_phyllo_sf |>
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
  coord_sf(expand = FALSE) +
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

ggsave(filename = "mcp_especie_phyllomedusa_tereaploidea_07.01.2026.png",
       height = 10, width = 12)

# Área de Vida ----

## Por indivíduo ----

### Mínimo polígono convexo para cada espécie ----

area_vida_phyllo <- function(id){

  kde_95 <- dados_phyllo_sf |>
    dplyr::filter(ID == id) |>
    sf::st_transform(crs = 32725) |>
    sf::as_Spatial() |>
    adehabitatHR::kernelUD(h = "href") |>
    adehabitatHR::getverticeshr(percent = 95) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = 4674) |>
    dplyr::select(-id) |>
    dplyr::mutate(Percent = "095%",
                  ID = id)

  kde_50 <- dados_phyllo_sf |>
    dplyr::filter(ID == id) |>
    sf::st_transform(crs = 32725) |>
    sf::as_Spatial() |>
    adehabitatHR::kernelUD(h = "href") |>
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

id <- paste0("PT", 1:7)

id

purrr::walk(id, area_vida_phyllo)

### Unindo os sahpefiles ----

kde_phyllo <- ls(pattern = "kde_PT") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

kde_phyllo

kde_phyllo_PT1 <- ls(pattern = "kde_PT1") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

kde_phyllo_PT1

kde_phyllo_PT2 <- ls(pattern = "kde_PT2") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

kde_phyllo_PT2

kde_phyllo_PT3 <- ls(pattern = "kde_PT3") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

kde_phyllo_PT3

kde_phyllo_PT4 <- ls(pattern = "kde_PT4") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

kde_phyllo_PT4

### Mapa ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  geom_sf(data = kde_phyllo_PT1, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  geom_sf(data = kde_phyllo_PT2, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  geom_sf(data = kde_phyllo_PT3, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  geom_sf(data = kde_phyllo_PT4, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  scale_fill_manual(values = c("gold", "royalblue")) +
  scale_color_manual(values = c("gold", "royalblue")) +
  coord_sf(expand = FALSE) +
  scale_x_continuous(breaks = seq(-51.829, -51.823, 0.003)) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              height = unit(0.5, "cm")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.position = "bottom") +
  ggview::canvas(height = 10, width = 12)

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  geom_sf(data = kde_phyllo, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  scale_fill_manual(values = c("gold", "royalblue")) +
  scale_color_manual(values = c("gold", "royalblue")) +
  facet_wrap(~ID) +
  coord_sf(expand = FALSE) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              text_col = "gold",
                              bar_cols = c("black", "gold"),
                              height = unit(0.5, "cm")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        stPTp.text = element_text(color = "black", size = 20),
        stPTp.background = element_rect(color = "black", linewidth = 1),
        legend.position = "bottom") +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "kde_individuo_phyllomedusa_tetraploidea_07.01.2026.png",
       height = 10, width = 12)

## Total para a espécie ----

### Mínimo polígono total da espécie ----

kde_total_095 <- dados_phyllo_sf |>
  sf::st_transform(crs = 32725) |>
  sf::as_Spatial() |>
  adehabitatHR::kernelUD(h = "href",
                         extent = 3) |>
  adehabitatHR::getverticeshr(percent = 95) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(Percent = "95%")

kde_total_095

kde_total_050 <- dados_phyllo_sf |>
  sf::st_transform(crs = 32725) |>
  sf::as_Spatial() |>
  adehabitatHR::kernelUD(h = "href") |>
  adehabitatHR::getverticeshr(percent = 50) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(Percent = "50%")

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
  coord_sf(expand = FALSE) +
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

### Mapa com a imagme de satélite ajustada ----

maptiles::get_providers()

img_sat_aju <- maptiles::get_tiles(kde_total_095 |>
                                     sf::st_bbox() |>
                                     sf::st_as_sfc(),
                                   provider = "Esri.WorldImagery",
                                   zoom = 16)

img_sat_aju

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat_aju) +
  geom_sf(data = kde_especie, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  scale_fill_manual(values = c("gold", "royalblue")) +
  scale_color_manual(values = c("gold", "royalblue")) +
  coord_sf(expand = FALSE) +
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

ggsave(filename = "kde_especie_phyllomedusa_tetraploidea_07.01.2026.png",
       height = 10, width = 12)
