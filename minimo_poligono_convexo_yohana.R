# Pacotes ----

library(tidyverse)

library(parzer)

library(geosphere)

library(sf)

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

angulos <- c (110,60,330,220,270,100,109,270,250,310,310,270,35,190,150,100,140,190,320,250,200,350,280,350,310,310,280,320,300,290,230,300,320,210,160)

angulos

## Dataframe das coordenadas inicais do trajeto ----

trajeto <- tibble::tibble(lon = lon_ini, lat = lat_ini)

trajeto

# Análise do mínimo polígono convexo ----

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

trajeto_sf <- trajeto |>
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = 4674) |>
  dplyr::summarise(do_union = FALSE) |>
  sf::st_cast("LINESTRING")

trajeto_sf

ggplot() +
  geom_sf(data = trajeto_sf)

## Polígono convexo ----

trajeto_convexo <- trajeto |>
  sf::st_as_sf(coords = c("lon", "lat"),
               crs = 4674) |>
  sf::st_union() |>
  sf::st_convex_hull()

trajeto_convexo

ggplot() +
  geom_sf(data = trajeto_sf, linewidth = 1) +
  geom_sf(data = trajeto_convexo, fill = "blue", alpha = 0.3)

## Tamanho do percurso ----

tamanho <- trajeto_sf |>
  sf::st_length()

tamanho

## Área ----

area <- trajeto_convexo |>
  sf::st_area()

area

# Mapa ----

ggplot() +
  geom_sf(data = trajeto_convexo, aes(fill = "Home Range"),
          color = "cyan4",
          alpha = 0.3,
          linewidth = 2.5) +
  geom_sf(data = trajeto_sf, linewidth = 0.9, aes(color = "Transect")) +
  coord_sf(label_graticule = "NSWE") +
  scale_color_manual(values = "black") +
  scale_fill_manual(values = "cyan4") +
  labs(fill = NULL,
       color = NULL,
       caption = paste0("Deslocation length: ",
                        tamanho |> round(2),
                        "m | Home Range Area: ",
                        area |> round(2),
                        "m")) +
  #scale_x_continuous(breaks = seq(-51.83157, -51.83155, 0.000001)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 12),
        legend.position = "top",
        plot.caption = element_text(color = "black", size = 12, hjust = 0.5))

ggsave(filename = "home_range_yohana.png", height = 10, width = 12)
