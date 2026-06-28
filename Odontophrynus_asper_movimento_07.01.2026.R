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

dados_odontophrynus <- readxl::read_xlsx("Dados Odontophrynus asper.xlsx")

### Visualizando ----

dados_odontophrynus

dados_odontophrynus |> dplyr::glimpse()

### Tratando as coordenadas ----

dados_odontophrynus %<>%
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

dados_odontophrynus |> as.data.frame()

### Completando as coordenadas ----

convertendo_coords <- function(x){

  if(dados_odontophrynus$Longitude[x] |> is.na() | dados_odontophrynus$Latitude[x] |> is.na()){

    dest <- geosphere::destPoint(c(dados_odontophrynus$Longitude[x-1],
                                   dados_odontophrynus$Latitude[x-1]),
                                 dados_odontophrynus$`Angulo de virada`[x-1],
                                 dados_odontophrynus$`Distância (m)`[x-1])

    dados_odontophrynus$Longitude[x] <<- dest[1]

    dados_odontophrynus$Latitude[x] <<- dest[2]

  }

}

purrr::walk(1:nrow(dados_odontophrynus), convertendo_coords)

dados_odontophrynus

### Transformando em shapefile ----

dados_odontophrynus_sf <- dados_odontophrynus |>
  dplyr::group_by(ID) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674) |>
  dplyr:::ungroup()

dados_odontophrynus_sf

dados_odontophrynus_sfline <- dados_odontophrynus_sf |>
  dplyr::group_by(ID) |>
  dplyr::summarise(do_union = FALSE) |>
  sf::st_cast("LINESTRING") |>
  dplyr:::ungroup()

ggplot() +
  geom_sf(data = dados_odontophrynus_sfline, aes(color = ID), linewidth = 1) +
  coord_sf(expand = TRUE)

## Imagem de satélite ----

### Criando um shapefile da área que abrange os pontos ----

area_sf <- dados_odontophrynus_sf |>
  sf::st_bbox() |>
  sf::st_as_sfc()

area_sf

ggplot() +
  geom_sf(data = area_sf) +
  geom_sf(data = dados_odontophrynus_sfline, aes(color = ID), linewidth = 1) +
  coord_sf(expand = TRUE)

### Importando ----

maptiles::get_providers()

img_sat <- maptiles::get_tiles(area_sf,
                               provider = "Esri.WorldImagery",
                               zoom = 18)

img_sat

## Visualizando ----

ggplot() +
  tidyterra::geom_spatraster_rgb(data = img_sat) +
  geom_sf(data = dados_odontophrynus_sfline, aes(color = ID), linewidth = 1) +
  coord_sf(expand = TRUE)

# Mínimo Polígono Convexo ----

## Mínimo polígono total da espécie ----

mcp_total_100 <- dados_odontophrynus_sf |>
  sf::st_transform(crs = 32725) |>
  sf::as_Spatial() |>
  adehabitatHR::mcp(percent = 100) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(Percent = "100%")

mcp_total_100

mcp_total_095 <- dados_odontophrynus_sf |>
  sf::st_transform(crs = 32725) |>
  sf::as_Spatial() |>
  adehabitatHR::mcp(percent = 95) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(Percent = "95%")

mcp_total_095

## Unindo os MCPs ----

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
  scale_x_continuous(breaks = seq(-51.8266, -51.8254, 0.0004)) +
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

ggsave(filename = "mcp_especie_odontophrynus_asper_07.01.2026.png",
       height = 10, width = 12)

# Área de Vida ----

## Mínimo polígono total da espécie ----

kde_total_095 <- dados_odontophrynus_sf |>
  sf::st_transform(crs = 32725) |>
  sf::as_Spatial() |>
  adehabitatHR::kernelUD(h = "href") |>
  adehabitatHR::getverticeshr(percent = 95) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(Percent = "100%")

kde_total_095

kde_total_050 <- dados_odontophrynus_sf |>
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
  coord_sf(expand = FALSE) +
  scale_x_continuous(breaks = seq(-51.8266, -51.8254, 0.0004)) +
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

ggsave(filename = "kde_especie_odontophrynus_asper_07.01.2026.png",
       height = 10, width = 12)
