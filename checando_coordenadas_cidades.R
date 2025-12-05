# Pacotes -----

library(geobr)

library(sf)

# Dados ----

## Cidades do Brasil ----

### Importando ----

cidades <- geobr::read_municipality(year = 2019)

### Visualizando ----

cidades

ggplot() +
  geom_sf(data = cidades)

## Coordenadas ----

### Criando ----

coords <- data.frame(x = c(374914, 382121, 381168),
                 y = c(7275539, 7270403, 7275139))

### Visualizando ----

coords

coords |> dplyr::glimpse()

### Criando op shapefile ----

sf <- coords |>
  sf::st_as_sf(coords = c("x", "y"),
               crs = 32722) |>
  sf::st_transform(crs = 4674)

sf

ggplot() +
  geom_sf(data = cidades) +
  geom_sf(data = sf, color = "red")

# Comparando ----

sf |>
  sf::st_join(cidades)
