# Pacotes ----

library(tidyverse)

library(readxl)

library(magrittr)

library(parzer)

library(geosphere)

library(sf)

library(adehabitatHR)

library(ggspatial)

library(patchwork)

# Dados ----

## Ativar Google Drive ----

googledrive::drive_auth("edson.nilton@ufpe.br")

## Importar dados ----

mov <- purrr::pmap_dfr(
  "Artigo pererecas" |>
    googledrive::drive_get() |>
    googledrive::drive_ls() |>
    dplyr::filter(name |> stringr::str_detect("Dados ")) |>
    dplyr::select(id, name) |>
    dplyr::rename("arquivo" = 1,
                  "nome" = 2),
  \(arquivo, nome){

    arquivo |>
      googledrive::drive_download(file = _,
                                  path = tempfile(fileext = ".xlsx")) |>
      dplyr::pull(local_path) |>
      readxl::read_xlsx() |>
      dplyr::mutate(Especie = nome |>
                      stringr::str_remove_all("Dados |.xlsx"))

  },
  .progress = TRUE)

## Visualizar ----

mov

mov |> dplyr::glimpse()

## Converter Coordenadas ----

mov %<>%
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

mov

## Completar coordenadas ----

mov <- purrr::reduce(
  .init = mov,
  .x = 1:nrow(mov),
  .f = \(dados, linha){

    if(is.na(dados$Longitude[linha]) || is.na(dados$Latitude[linha])){

      coord <- geosphere::destPoint(
        p = c(dados$Longitude[linha - 1], dados$Latitude[linha - 1]),
        b = dados$`Angulo de virada`[linha - 1],
        d = dados$`Distância (m)`[linha - 1]
        )

      dados$Longitude[linha] <- coord[1]

      dados$Latitude[linha] <- coord[2]

    }

    dados

    })

mov

## Área de vida ----

## Transformar os pontos em shapefile ----

mov_sf <- mov |>
  sf::st_as_sf(coords = c(2:3),
               crs = 4674)

mov_sf

ggplot() +
  geom_sf(data = mov_sf, aes(color = Especie))

## Calcular áreas de vida ----

area_perereca <- purrr::map_dfr(
  mov_sf$Especie |> unique(),
  \(perereca){

    id <- mov_sf |>
      dplyr::filter(Especie == perereca) |>
      dplyr::pull(ID) |>
      unique()

    purrr::map(
      id,
      \(ind){

        KDE95 <- mov_sf |>
          dplyr::filter(Especie == perereca & ID == ind) |>
          sf::as_Spatial() |>
          adehabitatHR::kernelUD(h = "href",
                                 extent = 3) |>
          adehabitatHR::getverticeshr(percent = 90) |>
          sf::st_as_sf() |>
          dplyr::mutate(Tipo = "KDE 95%",
                        Espécie = perereca,
                        Indivíduo = ind)

        kde50 <- mov_sf |>
          dplyr::filter(Especie == perereca & ID == ind) |>
          sf::as_Spatial() |>
          adehabitatHR::kernelUD(h = "href",
                                 extent = 3) |>
          adehabitatHR::getverticeshr(percent = 50) |>
          sf::st_as_sf() |>
          dplyr::mutate(Tipo = "KDE 50%",
                        Especie = perereca,
                        Indivíduo = ind)

        dplyr::bind_rows(kde50, KDE95)

      }
    )

  },
  .progress = TRUE) |>
  dplyr::mutate(Tipo = Tipo |> forcats::fct_relevel(c("KDE 95%",
                                                      "KDE 50%")))

area_perereca
