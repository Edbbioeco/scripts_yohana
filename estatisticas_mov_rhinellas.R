# Pacotes ----

library(tidyverse)

library(readxl)

library(magrittr)

library(parzer)

library(geosphere)

library(sf)

library(adehabitatHR)

# Dados ----

## Baixar do Google Drive ----

googledrive::drive_auth("edson.nilton@ufpe.br")

Sys.getenv("pasta_artigo_rhinella") |>
  googledrive::drive_get() |>
  googledrive::drive_ls() |>
  dplyr::filter(name |> stringr::str_detect("Dados Rhinella")) |>
  dplyr::pull(name) |>
  purrr::map(
    \(arquivo){

      googledrive::drive_download(file = arquivo,
                                  path = paste0("./", arquivo),
                                  overwrite = TRUE)

    },
    .progress = TRUE)

### Importar ----

dados_rhinellas <- purrr::map_dfr(
  c("Rhinella icterica", "Rhinella crucifer"),
  \(rhinella){

    readxl::read_xlsx(paste0("Dados ", rhinella, ".xlsx")) |>
      dplyr::mutate(Espécie = rhinella)

  },
  .progress = TRUE)

### Visualizar ----

dados_rhinellas

dados_rhinellas |> dplyr::glimpse()

### Converter coordenadas ----

dados_rhinellas %<>%
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

dados_rhinellas

### Completar coordenadas ----

dados_rhinellas <- purrr::reduce(
  .init = dados_rhinellas,
  .x = 1:nrow(dados_rhinellas),
  .f = \(dados, x){

    if(is.na(dados$Longitude[x]) || is.na(dados$Latitude[x])){

      dest <- geosphere::destPoint(
        p = c(dados$Longitude[x - 1], dados$Latitude[x - 1]),
        b = dados$`Angulo de virada`[x - 1],
        d = dados$`Distância (m)`[x - 1])

      dados$Longitude[x] <- dest[1]

      dados$Latitude[x]  <- dest[2]

    }

    dados

  })

dados_rhinellas

### Transformar em shapefile ----

dados_rhinellas_sf <- dados_rhinellas |>
  sf::st_as_sf(coords = c(2:3),
               crs = 4674)

dados_rhinellas_sf

ggplot() +
  geom_sf(data = dados_rhinellas_sf, aes(color = Espécie))

# Estatísticas das áreas de vida ----

## Calcular área de vida ----

area_rhinella <- purrr::pmap_dfr(
  dados_rhinellas |>
    dplyr::select(Espécie, ID) |>
    dplyr::distinct(sps = Espécie, ind = ID),
  \(sps, ind){

    KDE95 <- dados_rhinellas_sf |>
      dplyr::filter(ID == ind) |>
      sf::as_Spatial() |>
      adehabitatHR::kernelUD(h = "href",
                             extent = 3) |>
      adehabitatHR::getverticeshr(percent = 90) |>
      sf::st_as_sf() |>
      dplyr::mutate(Tipo = "KDE 95%",
                    Espécie = sps,
                    idivíduo = ind)

    kde50 <- dados_rhinellas_sf |>
      dplyr::filter(ID == ind) |>
      sf::as_Spatial() |>
      adehabitatHR::kernelUD(h = "href",
                             extent = 3) |>
      adehabitatHR::getverticeshr(percent = 50) |>
      sf::st_as_sf() |>
      dplyr::mutate(Tipo = "KDE 50%",
                    Espécie = sps,
                    idivíduo = ind)

    dplyr::bind_rows(kde50, KDE95)

  },
  .progress = TRUE) |>
  dplyr::mutate(Tipo = Tipo |> forcats::fct_relevel(c("KDE 95%",
                                                      "KDE 50%"))) |>
  dplyr::mutate(area = geometry |>
                  sf::st_transform(crs = 31982) |>
                  sf::st_area() |>
                  as.numeric())

area_rhinella

## Estatísticas descritivas ----

### Medias por espécies ----

area_rhinella |>
  sf::st_drop_geometry() |>
  dplyr::summarise(media = area |> mean() |> round(2),
                   sd = area |> sd() |> round(2),
                   .by = Espécie)

