# Pacotes ----

library(googledrive)

library(readxl)

library(tidyverse)

library(parzer)

library(geosphere)

library(sf)

library(adehabitatHR)

library(ggbeeswarm)

library(janitor)

library(reshape2)

library(performance)

library(DHARMa)

library(marginaleffects)

library(flextable)

library(ggh4x)

library(glmmTMB)

# Dados ----

## Dados de movimentação ----

### Ativar Google Drive ----

googledrive::drive_auth("edson.nilton@ufpe.br")

### Importar ----

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

### Visualizar ----

mov

mov |> dplyr::glimpse()

### Converter coordenadas ----

mov <- mov |>
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

mov

### Complear as coordenadas ----

mov <- purrr::reduce(
  .x = 1:nrow(mov),
  .init = mov,
  .f = \(dados, linha){

    if(is.na(dados$Longitude[linha]) || is.na(dados$Latitude[linha])){

      destino <- geosphere::destPoint(
        p = c(dados$Longitude[linha - 1], dados$Latitude[linha - 1]),
        b = dados$`Angulo de virada`[linha - 1],
        d = dados$`Distância (m)`
        )

      dados$Longitude[linha] <- destino[1]

      dados$Latitude[linha] <- destino[2]

      }

    dados

    }
  )

mov

### Transformar em shapefile ----

mov_sf <- mov |>
  sf::st_as_sf(coords = c(2:3),
               crs = 4674)

mov_sf

ggplot() +
  geom_sf(data = mov_sf, aes(color = Especie))

## Dados morfológicos ----

## Importar ----

morf <- purrr::map_dfr(
  c("Boana faber", "Phyllomedusa tetraploidea"),
  \(sps){

    "Artigo pererecas" |>
      googledrive::drive_get() |>
      googledrive::drive_ls() |>
      dplyr::filter(name |> stringr::str_detect("médias")) |>
      dplyr::pull(id) |>
      googledrive::drive_download(file = _,
                                  path = tempfile(fileext = ".xlsx")) |>
      dplyr::pull(local_path) |>
      readxl::read_xlsx(sheet = sps) |>
      dplyr::mutate(Especie = sps)

    },
  .progress = TRUE)

### Visualizar ----

morf

morf |> dplyr::glimpse()

## Dados ambientais ----

### Importar ----

amb <- "Artigo pererecas" |>
  googledrive::drive_get() |>
  googledrive::drive_ls() |>
  dplyr::filter(name |> stringr::str_detect("AMB")) |>
  dplyr::pull(id) |>
  googledrive::drive_download(file = _,
                              path = tempfile(fileext = ".xlsx")) |>
  dplyr::pull(local_path) |>
  readxl::read_xlsx() |>
  dplyr::filter(INDIVIDUO |> stringr::str_detect("BF|PT")) |>
  dplyr::rename("ID" = 1) |>
  dplyr::mutate(Especie = dplyr::case_when(

    ID |> stringr::str_detect("BF") ~ "Boana faber",
    .default = "Phyllomedusa tetraploidea"
    ))
