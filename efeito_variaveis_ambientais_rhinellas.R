# Pacotes ----

library(googledrive)

library(readxl)

library(tidyverse)

library(parzer)

library(geosphere)

library(sf)

library(adehabitatHR)

library(janitor)

library(reshape2)

library(ggview)

library(lmerTest)

library(performance)

library(lme4)

library(ggeffects)

library(DHARMa)

# Dados ----

## Dados de movimentação ----

### Baixar do Google Drive ----

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

mov <- purrr::map_dfr(
  c("Rhinella crucifer",
    "Rhinella icterica"),
  \(sps){

    readxl::read_xlsx(paste0("Dados ",
                             sps,
                             ".xlsx")) |>
      dplyr::mutate(Espécie = sps)

    },
  .progress = TRUE)

mov

### Visualizar ----

mov

mov |> dplyr::glimpse()

### Completar coordenadas ----

mov <- mov |>
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

mov

mov |> dplyr::glimpse()

### Completar as coordenadas ----

mov_trat <- purrr::reduce(
  .x = 1:nrow(mov),
  .f = \(dados, linha){

    if(is.na(dados$Longitude[linha]) || is.na(dados$Longitude[linha])){

      coord <- geosphere::destPoint(
        p = c(dados$Longitude[linha - 1], dados$Latitude[linha - 1]),
        b = dados$`Angulo de virada`[linha - 1],
        d = dados$`Distância (m)`[linha - 1]
      )

      dados$Longitude[linha] <- coord[1]

      dados$Latitude[linha] <- coord[2]

    }

    dados

    },
  .init = mov)

mov_trat

### Transformar em shapefile ----

dados_rhinellas_sf <- mov_trat |>
  sf::st_as_sf(coords = c(2:3),
               crs = 4674)

dados_rhinellas_sf

## Dados ambientais ----

### Baixar do Google Drive ----

Sys.getenv("pasta_artigo_rhinella") |>
  googledrive::drive_get() |>
  googledrive::drive_ls() |>
  dplyr::filter(name |> stringr::str_detect("AMB")) |>
  dplyr::pull(name) |>
  purrr::map(
    \(arquivo){

      googledrive::drive_download(file = arquivo,
                                  path = paste0("./", arquivo),
                                  overwrite = TRUE)

    },
    .progress = TRUE)

### Importar ----

amb <- readxl::read_xlsx("./DADOS AMBIENTAIS COM AS 6 ESPÉCIES INDIVIDUAIS.xlsx")


### Visualizar ----

amb

amb |> dplyr::glimpse()

# Modelos lineares ----

## Calcular valores médios das variáveis ambientais para as espécies de Rhinella ----

amb_trat <- amb |>
  dplyr::summarise(dplyr::across(.cols = dplyr::where(is.numeric),
                                 .fns = ~.x |> mean()),
                   .by = INDIVIDUO) |>
  dplyr::rename("ID" = 1) |>
  dplyr::filter(ID |> stringr::str_detect("^R"))

amb_trat

## Calculara área de vida das espécies ----

area_rhinella <- purrr::map_dfr(
  c("Rhinella icterica", "Rhinella crucifer"),
  \(rhinella){

    id <- dados_rhinellas_sf |>
      dplyr::filter(Espécie == rhinella) |>
      dplyr::pull(ID) |>
      unique()

    purrr::map(
      id,
      \(ind){

        KDE95 <- dados_rhinellas_sf |>
          dplyr::filter(Espécie == rhinella & ID == ind) |>
          sf::as_Spatial() |>
          adehabitatHR::kernelUD(h = "href",
                                 extent = 3) |>
          adehabitatHR::getverticeshr(percent = 90) |>
          sf::st_as_sf() |>
          dplyr::mutate(Tipo = "KDE 95%",
                        Espécie = rhinella,
                        Indivíduo = ind)

        kde50 <- dados_rhinellas_sf |>
          dplyr::filter(Espécie == rhinella & ID == ind) |>
          sf::as_Spatial() |>
          adehabitatHR::kernelUD(h = "href",
                                 extent = 3) |>
          adehabitatHR::getverticeshr(percent = 50) |>
          sf::st_as_sf() |>
          dplyr::mutate(Tipo = "KDE 50%",
                        Espécie = rhinella,
                        Indivíduo = ind)

        dplyr::bind_rows(kde50, KDE95)

      }
    )

  },
  .progress = TRUE) |>
  dplyr::mutate(Tipo = Tipo |> forcats::fct_relevel(c("KDE 95%",
                                                      "KDE 50%")),
                area = geometry |>
                  sf::st_transform(crs = 31982) |>
                  sf::st_area() |>
                  as.numeric()) |>
  dplyr::rename("ID" = Indivíduo)

area_rhinella
