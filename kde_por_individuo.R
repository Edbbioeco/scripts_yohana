# Pacotes ----

library(tidyverse)

library(readxl)

library(magrittr)

library(parzer)

library(geosphere)

library(sf)

library(adehabitatHR)

library(writexl)

library(flextable)

# Dados ----

## Importar ----

especies <- c("Boana faber",
              "Phyllomedusa tetraploidea",
              "Rhinella crucifer",
              "Rhinella icterica",
              "Odontophrynus asper",
              "Leptodactylus mystacinus")

sps_mov_dados <- purrr::map_dfr(
  especies,
  ~readxl::read_xlsx(paste0("Dados ", .x, ".xlsx")) |>
    dplyr::mutate(Espécie = .x),
  .progress = TRUE)

## Visualizar ----

sps_mov_dados

sps_mov_dados |> dplyr::glimpse()

# Tratar coordenadas ----

## Converter as coordenadas para decimal ----

sps_mov_dados %<>%
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

sps_mov_dados

## Completar coordenadas ----

purrr::map(
  1:nrow(sps_mov_dados),
  \(x){

    if(sps_mov_dados$Longitude[x] |> is.na() | sps_mov_dados$Latitude[x] |> is.na()){

      dest <- geosphere::destPoint(
        c(sps_mov_dados$Longitude[x-1],
          sps_mov_dados$Latitude[x-1]),
        sps_mov_dados$`Angulo de virada`[x-1],
        sps_mov_dados$`Distância (m)`[x-1])

      sps_mov_dados$Longitude[x] <<- dest[1]

      sps_mov_dados$Latitude[x] <<- dest[2]

    }

  },
  .progress = TRUE)

sps_mov_dados |> as.data.frame()

# KDE ----

## Transformar os dados em shapefile ----

sf_dados <- sps_mov_dados |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674)

sf_dados

ggplot() +
  geom_sf(data = sf_dados, aes(color = Espécie))

## Calcular área de cada espécie por indivíduo ----

kde_sps_ind <- purrr::map_dfr(
  especies,
  \(sps){

    ind <- sps_mov_dados |>
      dplyr::filter(Espécie == sps) |>
      dplyr::pull(ID) |>
      unique()

    purrr::map(
      ind,
      \(ind){

        tibble::tibble(area_kernel95 = sf_dados |>
                         dplyr::filter(ID == ind) |>
                         sf::st_transform(crs = 32725) |>
                         sf::as_Spatial() |>
                         adehabitatHR::kernelUD(h = "href",
                                                extent = 3) |>
                         adehabitatHR::getverticeshr(percent = 95) |>
                         sf::st_as_sf() |>
                         sf::st_area() |>
                         as.numeric(),
                       area_kernel50 = sf_dados |>
                         dplyr::filter(ID == ind) |>
                         sf::st_transform(crs = 32725) |>
                         sf::as_Spatial() |>
                         adehabitatHR::kernelUD(h = "href",
                                                extent = 3) |>
                         adehabitatHR::getverticeshr(percent = 50) |>
                         sf::st_as_sf() |>
                         sf::st_area() |>
                         as.numeric()) |>
          dplyr::mutate(Espécie = sps,
                        ID = ind)

      }

    )

  },
  .progress = TRUE)

kde_sps_ind
