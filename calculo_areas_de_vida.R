# Pacotes ----

library(readxl)

library(tidyverse)

library(parzer)

library(geosphere)

library(sf)

library(adehabitatHR)

library(flextable)

# Dados ----

## Importar ----

importar_dados <- function(especies){

  dados <- readxl::read_xlsx(paste0("Dados ", especies, ".xlsx")) |>
    dplyr::mutate(Espécie = especies)

  assign(paste0("dados_", especies |> stringr::word(2)),
         dados,
         envir = globalenv())

}

especies <- c("Boana faber",
              "Phyllomedusa tetraploidea",
              "Rhinella crucifer",
              "Rhinella icterica",
              "Odontophrynus asper",
              "Leptodactylus mystacinus")

purrr::map(especies, importar_dados)

## Visualizar ----

ls(pattern = "dados_") |>
  mget(envir = globalenv())

### Unir os dados ----

sps_dados <- ls(pattern = "dados_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

sps_dados

# Tratar coordenadas ----

## Converter as coordenadas para decimal ----

sps_dados <- sps_dados |>
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

sps_dados

## Completar as coordenadas faltantes ----

convertendo_coords <- function(x){

  if(sps_dados$Longitude[x] |> is.na() | sps_dados$Latitude[x] |> is.na()){

    dest <- geosphere::destPoint(c(sps_dados$Longitude[x-1],
                                   sps_dados$Latitude[x-1]),
                                 sps_dados$`Angulo de virada`[x-1],
                                 sps_dados$`Distância (m)`[x-1])

    sps_dados$Longitude[x] <<- dest[1]

    sps_dados$Latitude[x] <<- dest[2]

  }

}

purrr::map(1:nrow(sps_dados), convertendo_coords)

sps_dados |> as.data.frame()

## Transformar em shapefile ----

sf_dados <- sps_dados |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674)

sf_dados

# Áreas de vida ----

## MCP ----

### Calcular ----

mcp_sps <- function(sps){

  mcp_100 <- sf_dados |>
    dplyr::filter(Espécie == sps) |>
    sf::as_Spatial() |>
    adehabitatHR::mcp(percent = 100) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = 32725) |>
    dplyr::mutate(Species = sps,
                  Method = "MCP 100%",
                  Area = geometry |>
                    sf::st_area() |>
                    as.numeric() / 1e6,
                  Area = dplyr::case_when(Area > 0.1 ~ Area |> round(3) |>
                                            as.character(),
                                          Area > 0.01 ~ Area |> round(4) |>
                                            as.character(),
                                          Area > 0.001 ~ Area |> round(5) |>
                                            as.character(),
                                          Area > 0.0001 ~ Area |> round(6) |>
                                            as.character())) |>
    as.data.frame() |>
    dplyr::select(Species:Area)

  mcp_095 <- sf_dados |>
    dplyr::filter(Espécie == sps) |>
    sf::as_Spatial() |>
    adehabitatHR::mcp(percent = 95) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = 32725) |>
    dplyr::mutate(Species = sps,
                  Method = "MCP 095%",
                  Area = geometry |>
                    sf::st_area() |>
                    as.numeric() / 1e6,
                  Area = dplyr::case_when(Area > 0.1 ~ Area |> round(3) |>
                                            as.character(),
                                          Area > 0.01 ~ Area |> round(4) |>
                                            as.character(),
                                          Area > 0.001 ~ Area |> round(5) |>
                                            as.character(),
                                          Area > 0.0001 ~ Area |> round(6) |>
                                            as.character())) |>
    as.data.frame() |>
    dplyr::select(Species:Area)

  mcps <- dplyr::bind_rows(mcp_100,
                           mcp_095)

  assign(paste0("mcp_sps_", sps),
         mcps,
         envir = globalenv())

}

sps <- sf_dados |>
  dplyr::pull(Espécie) |>
  unique()

sps

purrr::map(sps, mcp_sps)

### Visualizar ----

ls(pattern = "mcp_sps_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

### Unir os MCPs ----

sps_mcp <- ls(pattern = "mcp_sps_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

sps_mcp

## KDE ----

### Calcular ----

kde_sps <- function(sps){

  kde_90 <- sf_dados |>
    dplyr::filter(Espécie == sps) |>
    sf::as_Spatial() |>
    adehabitatHR::kernelUD(h = "href",
                           extent = 3.75) |>
    adehabitatHR::getverticeshr(percent = 95) |>
    sf::st_as_sf() |>
    sf::st_make_valid() |>
    dplyr::mutate(Species = sps,
                  Method = "KDE 095%",
                  Area = geometry |>
                    sf::st_area() |>
                    as.numeric() / 1e6,
                  Area = dplyr::case_when(Area > 0.1 ~ Area |> round(3) |>
                                            as.character(),
                                          Area > 0.01 ~ Area |> round(4) |>
                                            as.character(),
                                          Area > 0.001 ~ Area |> round(5) |>
                                            as.character(),
                                          Area > 0.0001 ~ Area |> round(6) |>
                                            as.character())) |>
    as.data.frame() |>
    dplyr::select(Species:Area)

  kde_50 <- sf_dados |>
    dplyr::filter(Espécie == sps) |>
    sf::as_Spatial() |>
    adehabitatHR::kernelUD(h = "href",
                           extent = 3.75) |>
    adehabitatHR::getverticeshr(percent = 50) |>
    sf::st_as_sf() |>
    sf::st_make_valid() |>
    dplyr::mutate(Species = sps,
                  Method = "KDE 050%",
                  Area = geometry |>
                    sf::st_area() |>
                    as.numeric() / 1e6,
                  Area = dplyr::case_when(Area > 0.1 ~ Area |> round(3) |>
                                            as.character(),
                                          Area > 0.01 ~ Area |> round(4) |>
                                            as.character(),
                                          Area > 0.001 ~ Area |> round(5) |>
                                            as.character(),
                                          Area > 0.0001 ~ Area |> round(6) |>
                                            as.character())) |>
    as.data.frame() |>
    dplyr::select(Species:Area)

 kdes <- dplyr::bind_rows(kde_90,
                          kde_50)

  assign(paste0("kde_sps_", sps),
         kdes,
         envir = globalenv())

}

purrr::map(sps, kde_sps)

### Visualizar ----

ls(pattern = "kde_sps_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

### Unir os KDEs ----

sps_kde <- ls(pattern = "kde_sps_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

sps_kde

# Tabela ----

## Gerar o data frame unindo os MCPs e KDEs ----

sps_areas <- dplyr::bind_rows(sps_mcp,
                              sps_kde)

sps_areas

## Criar a tabela flextable ----

tabela_areas <- sps_areas |>
  dplyr::rename("Area (km²)" = 3) |>
  flextable::flextable() |>
  flextable::width(width = 1, j = 2:3) |>
  flextable::width(width = 2.5, j = 1) |>
  flextable::align(align = "center", part = "all") |>
  flextable::fontsize(size = 12) |>
  flextable::bold(part = "header") |>
  flextable::italic(j = 1, part = "body")

tabela_areas

## Exportar a tabela ----

tabela_areas |>
  flextable::save_as_docx(path = "tabela_areas_de_vida.docx")
