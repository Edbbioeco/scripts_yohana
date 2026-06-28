# Pacotes ----

library(tidyverse)

library(parzer)

library(geosphere)

library(sf)

library(adehabitatHR)

# Dados ----

## Vetores ----

## Coordenadas ----

lat_ini <- "25°04'26.48''S" |> parzer::parse_lat()

lat_ini

lon_ini <- "51°49'53.63''W" |> parzer::parse_lon()

lon_ini

lat_final <- "25°04'25.93''S" |> parzer::parse_lat()

lat_final

lon_final <- "51°49'53.71''W" |> parzer::parse_lon()

lon_final

### Distâncias em metros ----

distancias <- c(124,100,28,115,160,105,190,360,128,125,230,50,30,90,381,484,150,120,76,59,47,60,83,28,104,132,70,70,100,130,170,158,75,20,115)/100

distancias

### Graus de Azimute (0-360°) ----

angulos <- c(110,60,330,220,270,100,109,270,250,310,310,270,35,190,150,100,140,190,320,250,200,350,280,350,310,310,280,320,300,290,230,300,320,210,160)

angulos

## Dataframe das coordenadas inicais do trajeto ----

trajeto <- tibble::tibble(lon = lon_ini, lat = lat_ini)

trajeto

# MCP ----

## Calculando as novas coordenadas ----

novas_coords <- function(x){

  destino <- geosphere::destPoint(c(trajeto$lon[x],
                                    trajeto$lat[x]),
                                  angulos[x],
                                  distancias[x])

  trajeto <<- dplyr::bind_rows(trajeto,
                              tibble::tibble(lat = destino[2],
                                             lon = destino[1]))

}

purrr::map(1:length(distancias), novas_coords)

trajeto

## Criando o shapefile ----

### Trajeto ----

trajeto_sf <- trajeto |>
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = 4674) |>
  dplyr::summarise(do_union = FALSE) |>
  sf::st_cast("LINESTRING")

trajeto_sf

### Pontos ----

pontos_sf <- trajeto |>
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = 4674)

pontos_sf

### Mapa ----

ggplot() +
  geom_sf(data = trajeto_sf, linewidth = 1) +
  geom_sf(data = pontos_sf, color = "red", size = 2.5) +
  theme_bw() +
  theme(axis.text = element_text(color = "black"))

# MCP ----

## Convertendo para UTM-Zopne 25S ----

pontos_sf_utm <- pontos_sf |>
  sf::st_transform(crs = 32725)

pontos_sf_utm

ggplot() +
  geom_sf(data = pontos_sf_utm, linewidth = 1) +
  theme_bw() +
  theme(axis.text = element_text(color = "black"))

## 100% ----

## 95% ----

mcp_95 <- pontos_sf_utm |>
  sf::as_Spatial() |>
  adehabitatHR::mcp(percent = 95) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(`% de pontos` = "95%")

mcp_95

## 50% ----

mcp_50 <- pontos_sf_utm |>
  sf::as_Spatial() |>
  adehabitatHR::mcp(percent = 50) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(`% de pontos` = "50%")

mcp_50

## Unindo os shapefiles ----

unido_mcp <- ls(pattern = "mcp_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::arrange(`% de pontos` |> dplyr::desc())

unido_mcp

## Mapa ----

ggplot() +
  geom_sf(data = unido_mcp |>
            sf::st_transform(crs = 4674),
          aes(color = `% de pontos`,
              fill = `% de pontos`),
          linewidth = 1) +
  coord_sf(expand = FALSE) +
  scale_color_manual(values = c("royalblue",
                                "orange")) +
  scale_fill_manual(values = c("royalblue",
                               "orange")) +
  guides(fill = guide_legend(title.position = "top"),
         color = guide_legend(title.position = "top")) +
  ggnewscale::new_scale_color() +
  geom_sf(data = pontos_sf, aes(color = "Pontos de registro"), size = 2.5) +
  scale_color_manual(values = c("black")) +
  labs(colour = NULL,
       x = NULL,
       y = NULL) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              height = unit(0.5, "cm")) +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black"),
        legend.position = "bottom")

## Área ----

unido_mcp |>
  sf::st_transform(crs = 32725) |>
  sf::st_area()

# KDE ----

## 95% ----

kde_95 <- pontos_sf_utm |>
  sf::as_Spatial() |>
  adehabitatHR::kernelUD(h = "href") |>
  adehabitatHR::getverticeshr(percent = 95) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(`% de pontos` = "95%")

## 95% ----

kde_50 <- pontos_sf_utm |>
  sf::as_Spatial() |>
  adehabitatHR::kernelUD(h = "href") |>
  adehabitatHR::getverticeshr(percent = 50) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(`% de pontos` = "50%")

## Unindo os shapefiles ----

unido_kde <- ls(pattern = "kde_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::arrange(`% de pontos` |> dplyr::desc())

unido_kde

## Mapa ----

ggplot() +
  geom_sf(data = unido_kde |>
            sf::st_transform(crs = 4674),
          aes(color = `% de pontos`,
              fill = `% de pontos`),
          linewidth = 1) +
  coord_sf(expand = FALSE) +
  scale_color_manual(values = c("royalblue",
                                "orange")) +
  scale_fill_manual(values = c("royalblue",
                                "orange")) +
  guides(fill = guide_legend(title.position = "top"),
         color = guide_legend(title.position = "top")) +
  ggnewscale::new_scale_color() +
  geom_sf(data = pontos_sf, aes(color = "Pontos de registro"), size = 2.5) +
  scale_color_manual(values = c("black")) +
  labs(colour = NULL,
       x = NULL,
       y = NULL) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              height = unit(0.5, "cm")) +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black"),
        legend.position = "bottom")

## Área ----

unido_kde |>
  sf::st_transform(crs = 32725) |>
  sf::st_area()
