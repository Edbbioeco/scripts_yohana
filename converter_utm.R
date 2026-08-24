# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(writexl)

# Dados ----

## Importar ----

dados <- readxl::read_xlsx("./caminho_dos_dados/nome_do_arquivo.xlsx")

## Visualizar ----

dados

dados |> dplyr::glimpse()

# Converter coordenadas ----

## Converter ----

dados_sf <- dados |>
  sf::st_as_sf(coords = c("Latitude", "Longitude"),
               crs = 32725) |>
  sf::st_transform(crs = 4674) |>
  sf::st_coordinates() |>
  as.data.frame() |>
  dplyr::rename("Latitude" = X,
                "Longitude" = Y)

dados_sf

## Exportar os dados ----

dados_sf |>
  writexl::write_xlsx("./caminho_dos_dados/coordenadas_convertidas.xlsx")
