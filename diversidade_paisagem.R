# Pacotes ----

library(sf)

library(tidyverse)

library(terra)

library(tidyterra)

library(landscapemetrics)

library(ggview)

# Shapefile da unidade de conservação ----

## Importar ----

uc <- sf::st_read("./uc_municipal_ceuc_outros_construcao.shp") |>
  dplyr::filter(nome |> stringr::str_detect("Capivara"))

## Visualizar ----

uc

ggplot() +
  geom_sf(data = uc, color = "black")

## Área bbox da uc ----

uc_bbox <- uc |>
  sf::st_bbox() |>
  sf::st_as_sfc() |>
  sf::st_as_sf()

uc_bbox

ggplot() +
  geom_sf(data = uc_bbox, color = "red") +
  geom_sf(data = uc, color = "black")

# Raster de uso e cobertura do solo ----

## Importar ----

uso_solo <- terra::rast("https://storage.googleapis.com/mapbiomas-public/initiatives/brasil/collection_10/lulc/coverage/brazil_coverage_2024.tif") |>
  terra::crop(uc_bbox) |>
  terra::mask(uc_bbox) |>
  terra::as.factor()

## Visualiza ----

### Cores das classes ----

source("https://raw.github.com/Edbbioeco/mapbiomas_classes/main/cor_classes_funcao.R")

cores <- vetorizar_cores(classes = uso_solo |>
                           terra::values() |>
                           as.character() |>
                           unique())

cores

### Nomes das classes ----

source("https://raw.github.com/Edbbioeco/mapbiomas_classes/main/nome_classe_funcao.R")

nomes <- vetorizar_classes(classes = uso_solo |>
                             terra::values() |>
                             as.character() |>
                             unique())

nomes

### Mapa ----

ggplot() +
  tidyterra::geom_spatraster(data = uso_solo) +
  scale_fill_manual(values = cores,
                    labels = nomes,
                    na.translate = FALSE,
                    guide = guide_legend(title.position = "top",
                                         title.hjust = 0.5)) +
  geom_sf(data = uc,
          color = "black",
          fill = "transparent",
          linewidth = 1) +
  coord_sf(expand = FALSE) +
  labs(fill = "Classes de uso e cobertura do solo") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.position = "bottom") +
  ggview::canvas(height = 10, width = 20)
