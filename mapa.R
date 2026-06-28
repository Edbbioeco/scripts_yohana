# Pacotes -----

library(geobr)

library(tidyverse)

library(sf)

library(ggrepel)

library(cowplot)

# Dados -----

## Brasil -----

### Importando ----

br <- geobr::read_country(year = 2019)

### Visualizando ----

br

ggplot() +
  geom_sf(data = br, color = "black") 

## Paraná ----

### Importando ----

parana <- geobr:::read_state(year = 2019) |> 
  dplyr::filter(name_state == "Paraná")

### Visualizando ----

parana

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = parana, color = "black", fill = "green4")

## Municípios ----

### Municípios ----

muni <- geobr::read_municipality(year = 2019) |> 
  dplyr::filter(name_muni %in% c("Tibagi", "Londrina", "Curitiba", "Ponta Grossa",
                                 "Goioxim", "Guarapuava", "Pinhão", "Cantagalo", "Candói", "Turvo", "Campina Do Simão") & name_state == "Paraná") |> 
  dplyr::mutate(type = dplyr::case_when(name_muni %in% c("Tibagi", "Londrina", "Curitiba", "Ponta Grossa") ~ "Comparative municipality",
                                        .default = "Non comparative municipality"))

### Visualizando ----

muni 

ggplot() +
  geom_sf(data = br, color = "black") +
  geom_sf(data = parana, color = "black", fill = "green4") +
  geom_sf(data = muni, aes(fill = type), color = "black") +
  coord_sf(xlim = c(-52.5, -49.5),
           ylim = c(-26, -23))

## Nome dos municípioss ----

nome_muni <- muni |> 
  sf::st_centroid() |> 
  sf::st_coordinates() |> 
  as.data.frame() |> 
  dplyr::mutate(name_muni = muni$name_muni) |> 
  dplyr::left_join(muni |> 
                     as.data.frame() |> 
                     dplyr::select(name_muni, type),
                   by = "name_muni")

nome_muni

# Mapa ----

## Setnado tema ----

theme_set(theme_bw() +
            theme(axis.text = element_text(size = 15, color = "black"),
                  axis.title = element_text(size = 15, color = "black"),
                  strip.text = element_text(size = 15, color = "black"),
                  strip.background = element_rect(linewidth = 1, color = "black"),
                  legend.text = element_text(size = 15, color = "black"),
                  legend.title = element_text(size = 15, color = "black"),
                  panel.border = element_rect(linewidth = 1, color = "black"),
                  legend.position = "bottom"))

## Inset map ----

inset_map <- ggplot() +
  geom_sf(data = br, color = "black", fill = "white", linewidth = 0.5) +
  geom_sf(data = parana, color = "black", fill = "gray40", linewidth = 0.5) +
  geom_sf(data = muni, aes(fill = type), color = "black") +
  geom_rect(aes(xmin = -52.5, 
                xmax = -49.25,
                ymin = -26,
                ymax = -23),
            color = "darkred",
            fill = "red",
            linewidth = 0.1,
            alpha = 0.3) +
  labs(fill = NULL) +
  theme_void() +
  theme(legend.position = "none")
  

inset_map

## Mapa principal ----

mapa_principal <- ggplot() +
  geom_sf(data = br, aes(fill = "Brazil"), color = "black", linewidth = 1) +
  geom_sf(data = parana, aes(fill = "Paraná"), color = "black", linewidth = 1) +
  geom_sf(data = muni, aes(fill = type), color = "black", linewidth = 1) +
  ggrepel::geom_text_repel(data = nome_muni, aes(X, Y, label = name_muni, color = type),
                           bg.color = "black",
                           size = 5,
                           fontface = "bold",
                           box.padding = 1,
                           segment.size = 1.2,
                           segment.color = "black",
                           show.legend = FALSE) +
  scale_fill_manual(values = c("Brazil" = "white",
                               "Paraná" = "gray40",
                               "Comparative municipality" = "royalblue",
                               "Non comparative municipality" = "orange"),
                    breaks = c("Brazil", "Paraná", "Non comparative municipality", "Comparative municipality")) +
  scale_color_manual(values = c("Comparative municipality" = "cyan",
                               "Non comparative municipality" = "gold")) +
  labs(fill = NULL,
       color = NULL,
       x = NULL,
       y = NULL) +
  coord_sf(xlim = c(-52.5, -49.25),
           ylim = c(-26, -23),
           label_graticule = "NSWE")

mapa_principal

## Unindo ----

cowplot::ggdraw(mapa_principal) +
  cowplot::draw_plot(inset_map,
                     x = 0.15, 
                     y = 0.65,
                     width = 0.3, 
                     height = 0.3)

## Salvando ----

ggsave(filename = "map.png", height = 10, width = 12)

