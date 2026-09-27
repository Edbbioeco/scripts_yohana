# Pacotes ----

library(tidyverse)

library(readxl)

library(parzer)

library(sf)

library(geosphere)

library(flextable)

library(adehabitatHR)

library(ggview)

# Dados ----

## Registros de espécies ----

### Importar ----

reg <- purrr::map_dfr(
  c("Boana faber",
    "Phyllomedusa tetraploidea",
    "Rhinella crucifer",
    "Rhinella icterica",
    "Odontophrynus americanus",
    "Leptodactylus mystacinus"),
  \(sps){

    readxl::read_xlsx("dados de campo.xlsx",
                      sheet = sps) |>
      dplyr::select(ID,
                    Data,
                    `Coordenada inicial`,
                    Ponto:`Ângulo de virada (°)`) |>
      tidyr::separate(col = `Coordenada inicial`,
                      into = c("Latitude", "Longitude"),
                      sep = " ") |>
      dplyr::mutate(Espécie = sps,
                    .before = 1)

    }

  )

### Visualizar ----

reg

reg |> dplyr::glimpse()

### Converter dados de longitude e latitude ----

reg <- reg |>
  dplyr::mutate(Longitude = parzer::parse_lon(Longitude),
                Latitude  = parzer::parse_lat(Latitude))

reg

### Completar as coordenadas ----

reg_trat <- purrr::reduce(
  .x = 1:nrow(reg),
  .f = \(dados, x){

    if(is.na(dados$Longitude[x]) || is.na(dados$Latitude[x])){

      dest <- geosphere::destPoint(
        c(dados$Longitude[x - 1], dados$Latitude[x - 1]),
        dados$`Ângulo de virada (°)`[x - 1],
        dados$`Distancia percorrida (m)`[x - 1])

      dados$Longitude[x] <- dest[1]

      dados$Latitude[x]  <- dest[2]

      }

    dados

    },

  .init = reg)

reg_trat

### Transformar em shapefile ----

reg_sf <- reg_trat |>
  sf::st_as_sf(coords = c(2:3),
               crs = 4674)

reg_sf

ggplot() +
  geom_sf(data = reg_sf)

## Variáveis ambientais ----

### Importar -----

amb <- readxl::read_xlsx("DADOS AMBIENTAIS COM AS 6 ESPÉCIES INDIVIDUAIS.xlsx")

### Visualizar ----

amb

amb |> dplyr::glimpse()

# Distância percorrida ----

## Por indivíduo ----

### Calcular trajeto ----

trajeto <- purrr::map_dfr(
  reg_sf$ID |> unique(),
  \(ind){

    percurso <- reg_sf |>
          dplyr::filter(ID == ind) |>
          dplyr::summarise(geometry = geometry |>
                             sf::st_combine()) |>
          sf::st_cast(to = "LINESTRING") |>
          sf::st_length()

    sps <- reg_sf |>
      dplyr::filter(ID == ind) |>
      dplyr::pull(Espécie) |>
      unique()

    tibble::tibble(Espécie = sps,
                   ID = ind,
                   `Distância percorrida (m)` = percurso |>
                     as.numeric() |>
                     round(2))

        },
  .progress = TRUE)

trajeto

### Tabela flextable ----

trajeto_flex <- trajeto |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(j = c(1, 3),
                   width = 2) |>
  flextable::italic(j = 1,
                    part = "body")

trajeto_flex

trajeto_flex |> flextable::save_as_docx(path = "tabela_ind_trajeto.docx")

## Por indivíduo e data ----

### Calcular trajeto ----

trajeto_data <- purrr::map_dfr(
  reg_sf$ID |> unique(),
  \(ind){

    percurso <- reg_sf |>
      dplyr::filter(ID == ind) |>
      dplyr::summarise(geometry = geometry |>
                         sf::st_combine()) |>
      sf::st_cast(to = "LINESTRING") |>
      sf::st_length()

    sps <- reg_sf |>
      dplyr::filter(ID == ind) |>
      dplyr::pull(Espécie) |>
      unique()

    tibble::tibble(Espécie = sps,
                   ID = ind,
                   `Distância percorrida (m)` = percurso |>
                     as.numeric() |>
                     round(2))

  },
  .progress = TRUE)

trajeto_data

# Índice de tortuosidade ----

## Por indivíduo ----

### Calcular índice de tortuosidade ----

tortuosidade <- purrr::map_dfr(
  reg_sf$ID |> unique(),
  \(ind){

    percurso <- reg_sf |>
      dplyr::filter(ID == ind) |>
      dplyr::summarise(geometry = geometry |>
                         sf::st_combine()) |>
      sf::st_cast(to = "LINESTRING") |>
      sf::st_length()

    percurso_min <- reg_sf |>
      dplyr::filter(ID == ind) |>
      dplyr::slice(c(1, dplyr::n())) |>
      dplyr::summarise(geometry = geometry |>
                         sf::st_combine()) |>
      sf::st_cast(to = "LINESTRING") |>
      sf::st_length()

    sps <- reg_sf |>
      dplyr::filter(ID == ind) |>
      dplyr::pull(Espécie) |>
      unique()

    tibble::tibble(Espécie = sps,
                   ID = ind,
                   `Índice de tortuosidade` = (percurso_min / percurso) |>
                     round(2))

    },
  .progress = TRUE)

tortuosidade
