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

dados_rhinellas <- readxl::read_xlsx("Dados Rhinella icterica.xlsx")

### Visualizando ----

dados_rhinellas

dados_rhinellas |> dplyr::glimpse()

### Tratando as coordenadas ----

dados_rhinellas %<>%
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

dados_rhinellas |> as.data.frame()

### Completando as coordenadas ----

convertendo_coords <- function(x){

  if(dados_rhinellas$Longitude[x] |> is.na() | dados_rhinellas$Latitude[x] |> is.na()){

    dest <- geosphere::destPoint(c(dados_rhinellas$Longitude[x-1],
                                   dados_rhinellas$Latitude[x-1]),
                                 dados_rhinellas$`Angulo de virada`[x-1],
                                 dados_rhinellas$`Distância (m)`[x-1])

    dados_rhinellas$Longitude[x] <<- dest[1]

    dados_rhinellas$Latitude[x] <<- dest[2]

  }

}

purrr::walk(1:nrow(dados_rhinellas), convertendo_coords)

dados_rhinellas

### Transformando em shapefile ----

dados_rhinellas_sf <- dados_rhinellas |>
  dplyr::group_by(ID) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674) |>
    dplyr:::ungroup()

dados_rhinellas_sf

dados_rhinellas_sfline <- dados_rhinellas_sf |>
  dplyr::group_by(ID) |>
  dplyr::summarise(do_union = FALSE) |>
  sf::st_cast("LINESTRING") |>
  dplyr:::ungroup()

ggplot() +
  geom_sf(data = dados_rhinellas_sfline, aes(color = ID), linewidth = 1) +
  coord_sf(expand = TRUE)

## Imagem de satélite ----

### Criando um shapefile da área que abrange os pontos ----

area_sf <- dados_rhinellas_sf |>
  sf::st_bbox() |>
  sf::st_as_sfc()

area_sf

ggplot() +
  geom_sf(data = area_sf) +
  geom_sf(data = dados_rhinellas_sfline, aes(color = ID), linewidth = 1) +
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
  geom_sf(data = dados_rhinellas_sfline, aes(color = ID), linewidth = 1) +
  coord_sf(expand = TRUE)

# Mínimo Polígono Convexo ----

## Por indivíduo ----

### Mínimo polígono convexo para cada espécie ----

mcp_rhinellas <- function(id){

  mcp_100 <- dados_rhinellas_sf |>
    dplyr::filter(ID == id) |>
    sf::st_transform(crs = 32725) |>
    sf::as_Spatial() |>
    adehabitatHR::mcp(percent = 100) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = 4674) |>
    dplyr::select(-id) |>
    dplyr::mutate(Percent = "100%",
                  ID = id)

  mcp_095 <- dados_rhinellas_sf |>
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

id <- paste0("RI", 1:4)

id

purrr::walk(id, mcp_rhinellas)

### Unindo os sahpefiles ----

mcp_rhinella <- ls(pattern = "mcp_RI") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

mcp_rhinella

mcp_rhinella_ri1 <- ls(pattern = "mcp_RI1") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

mcp_rhinella_ri1

mcp_rhinella_ri2 <- ls(pattern = "mcp_RI2") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

mcp_rhinella_ri2

mcp_rhinella_ri3 <- ls(pattern = "mcp_RI3") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

mcp_rhinella_ri3

mcp_rhinella_ri4 <- ls(pattern = "mcp_RI4") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

mcp_rhinella_ri4

### Mapa ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  geom_sf(data = mcp_rhinella_ri1, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  geom_sf(data = mcp_rhinella_ri2, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  geom_sf(data = mcp_rhinella_ri3, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  geom_sf(data = mcp_rhinella_ri4, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  scale_fill_manual(values = c("gold", "royalblue")) +
  scale_color_manual(values = c("gold", "royalblue")) +
  coord_sf(expand = FALSE,
           ylim = c(-25.101, -25.0925)) +
  scale_x_continuous(breaks = seq(-51.829, -51.823, 0.003)) +
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
  geom_sf(data = mcp_rhinella, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  scale_fill_manual(values = c("gold", "royalblue")) +
  scale_color_manual(values = c("gold", "royalblue")) +
  facet_wrap(~ID) +
  coord_sf(expand = FALSE,
           ylim = c(-25.101, -25.0925)) +
  scale_x_continuous(breaks = seq(-51.829, -51.823, 0.003)) +
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

ggsave(filename = "mcp_individuo_rhinella_icterica_06.01.2026.png",
       height = 10, width = 12)

## Total para a espécie ----

### Mínimo polígono total da espécie ----

mcp_total_100 <- dados_rhinellas_sf |>
  sf::st_transform(crs = 32725) |>
  sf::as_Spatial() |>
  adehabitatHR::mcp(percent = 100) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(Percent = "100%")

mcp_total_100

mcp_total_095 <- dados_rhinellas_sf |>
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
           ylim = c(-25.101, -25.0925)) +
  scale_x_continuous(breaks = seq(-51.829, -51.823, 0.003)) +
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

ggsave(filename = "mcp_especie_rhinella_icterica_06.01.2026.png",
       height = 10, width = 12)

# Área de Vida ----

## Por indivíduo ----

### Mínimo polígono convexo para cada espécie ----

area_vida_rhinellas <- function(id){

  kde_95 <- dados_rhinellas_sf |>
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

  kde_50 <- dados_rhinellas_sf |>
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

id <- paste0("RI", 1:4)

id

purrr::walk(id, area_vida_rhinellas)

### Unindo os sahpefiles ----

kde_rhinella <- ls(pattern = "kde_RI") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

kde_rhinella

kde_rhinella_ri1 <- ls(pattern = "kde_RI1") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

kde_rhinella_ri1

kde_rhinella_ri2 <- ls(pattern = "kde_RI2") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

kde_rhinella_ri2

kde_rhinella_ri3 <- ls(pattern = "kde_RI3") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

kde_rhinella_ri3

kde_rhinella_ri4 <- ls(pattern = "kde_RI4") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

kde_rhinella_ri4

### Mapa ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  geom_sf(data = kde_rhinella_ri1, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  geom_sf(data = kde_rhinella_ri2, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  geom_sf(data = kde_rhinella_ri3, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  geom_sf(data = kde_rhinella_ri4, aes(fill = Percent, color = Percent),
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
  geom_sf(data = kde_rhinella, aes(fill = Percent, color = Percent),
          alpha = 0,
          linewidth = 1) +
  scale_fill_manual(values = c("gold", "royalblue")) +
  scale_color_manual(values = c("gold", "royalblue")) +
  facet_wrap(~ID) +
  coord_sf(expand = FALSE,
           ylim = c(-25.101, -25.0925)) +
  scale_x_continuous(breaks = seq(-51.829, -51.823, 0.003)) +
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

ggsave(filename = "kde_individuo_rhinella_icterica_06.01.2026.png",
       height = 10, width = 12)

## Total para a espécie ----

### Mínimo polígono total da espécie ----

kde_total_095 <- dados_rhinellas_sf |>
  sf::st_transform(crs = 32725) |>
  sf::as_Spatial() |>
  adehabitatHR::kernelUD(h = "href") |>
  adehabitatHR::getverticeshr(percent = 95) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(Percent = "100%")

kde_total_095

kde_total_050 <- dados_rhinellas_sf |>
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
           ylim = c(-25.101, -25.0925)) +
  scale_x_continuous(breaks = seq(-51.829, -51.823, 0.003)) +
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

ggsave(filename = "kde_especie_rhinella_icterica_06.01.2026.png",
       height = 10, width = 12)
