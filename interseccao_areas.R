# Pacotes ----

library(readxl)

library(tidyverse)

library(parzer)

library(geosphere)

library(sf)

library(adehabitatHR)

library(flextable)

# Dados ----

## Dados de movimentação ----

### Importando ----

importar_mov <- function(especies){

  dados <- readxl::read_xlsx(paste0("Dados ", especies, ".xlsx")) |>
    dplyr::mutate(Espécie = especies)

  assign(paste0("dados_mov_", especies |> stringr::word(2)),
         dados,
         envir = globalenv())

}

especies <- c("Boana faber",
              "Phyllomedusa tetraploidea",
              "Rhinella crucifer",
              "Rhinella icterica",
              "Odontophrynus asper",
              "Leptodactylus mystacinus")

purrr::map(especies, importar_mov)

### Visualizando ----

ls(pattern = "dados_mov_") |>
  mget(envir = globalenv())

### Unindo os dados ----

sps_mov_dados <- ls(pattern = "dados_mov_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

sps_mov_dados

sps_mov_dados |> dplyr::glimpse()

# Tratando as coordenadas ----

## Convertendo as coordenadas ----

sps_mov_dados <- sps_mov_dados |>
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

sps_mov_dados

## Completando as coordenadas ----

convertendo_coords <- function(x){

  if(sps_mov_dados$Longitude[x] |> is.na() | sps_mov_dados$Latitude[x] |> is.na()){

    dest <- geosphere::destPoint(c(sps_mov_dados$Longitude[x-1],
                                   sps_mov_dados$Latitude[x-1]),
                                 sps_mov_dados$`Angulo de virada`[x-1],
                                 sps_mov_dados$`Distância (m)`[x-1])

    sps_mov_dados$Longitude[x] <<- dest[1]

    sps_mov_dados$Latitude[x] <<- dest[2]

  }

}

purrr::map(1:nrow(sps_mov_dados), convertendo_coords)

sps_mov_dados |> as.data.frame()

## Transformando em shapefile ----

sf_dados <- sps_mov_dados |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674)

sf_dados

ggplot() +
  geom_sf(data = sf_dados, aes(color = Espécie))

# Movimento ----

## Calculando o movimento ----

areas_vida <- function(sps){

  sf_area_mcp_100 <- sf_dados |>
    dplyr::filter(Espécie == sps) |>
    sf::as_Spatial() |>
    adehabitatHR::mcp(percent = 100) |>
    sf::st_as_sf() |>
    dplyr::mutate(Espécie = sps,
                  Tipo = paste0("MCP_", sps |> stringr::word(2)),
                  Percent = 100)

  sf_area_mcp_095 <- sf_dados |>
    dplyr::filter(Espécie == sps) |>
    sf::as_Spatial() |>
    adehabitatHR::mcp(percent = 95) |>
    sf::st_as_sf() |>
    dplyr::mutate(Espécie = sps,
                  Tipo = paste0("MCP_", sps |> stringr::word(2)),
                  Percent = 95)

  sf_area_kernel_95 <- sf_dados |>
    dplyr::filter(Espécie == sps) |>
    sf::as_Spatial() |>
    adehabitatHR::kernelUD(h = "href",
                           extent = 4) |>
    adehabitatHR::getverticeshr(percent = 95) |>
    sf::st_as_sf() |>
    dplyr::mutate(Espécie = sps,
                  Tipo = paste0("KDE_", sps |> stringr::word(2)),
                  Percent = 95)

  sf_area_kernel_50 <- sf_dados |>
    dplyr::filter(Espécie == sps) |>
    sf::as_Spatial() |>
    adehabitatHR::kernelUD(h = "href",
                           extent = 4) |>
    adehabitatHR::getverticeshr(percent = 50) |>
    sf::st_as_sf() |>
    dplyr::mutate(Espécie = sps,
                  Tipo = paste0("KDE_", sps |> stringr::word(2)),
                  Percent = 50)

  lista_sf <- list(sf_area_mcp_100 = sf_area_mcp_100,
                   sf_area_mcp_095 = sf_area_mcp_095,
                   sf_area_kernel_95 = sf_area_kernel_95,
                   sf_area_kernel_50 = sf_area_kernel_50)

  nome_lista <- lista_sf |> names()

  sps_id <- sps |> stringr::word(2)

  criar_sf <- function(nome_lista, lista_sf){

    nome_sf <- paste0(nome_lista,
                      "_",
                      sps_id)

    assign(nome_sf,
           lista_sf,
           envir = globalenv())

  }

  purrr::map2(nome_lista, lista_sf, criar_sf)

}

sps <- sf_dados |>
  dplyr::pull(Espécie) |>
  unique()

sps

purrr::map(sps, areas_vida)

ls(pattern = "sf_area_") |>
  mget(envir = globalenv())

## Unindo os dados ----

df_area <- ls(pattern = "^sf_area_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

df_area

ggplot() +
  geom_sf(data = df_area, aes(color = Tipo)) +
  coord_sf(expand = TRUE)

# Interseção das áreas ----

## Criando função para calcular porcetagem ----

porcetagem <- function(area1, area2){

  area <- (area1 / area2) * 100

  return(area)

}

## Combinações ----

### MCP ----

comb_mcp <- combn(df_area |>
                    dplyr::filter(Tipo|> stringr::str_detect("MCP")) |>
                    dplyr::pull(Tipo) |>
                    unique(),
                  2,
                  simplify = FALSE)

comb_mcp

### KDE ----

comb_kde <- combn(df_area |>
                    dplyr::filter(Tipo|> stringr::str_detect("KDE")) |>
                    dplyr::pull(Tipo) |>
                    unique(),
                  2,
                  simplify = FALSE)

comb_kde


## MCP 100% ----

### Calculando ----

int_mcp_100 <- function(id){

  tipo1 <- comb_mcp[[id]][1]

  tipo2 <- comb_mcp[[id]][2]

  area1 <- df_area |>
    dplyr::filter(Tipo == tipo1,
                  Percent == 100) |>
    sf::st_transform(crs = 32725)

  area2 <- df_area |>
    dplyr::filter(Tipo == tipo2,
                  Percent == 100) |>
    sf::st_transform(crs = 32725)

  sps1 <- df_area |>
    dplyr::filter(Tipo == tipo1,
                  Percent == 100) |>
    dplyr::pull(Espécie)

  sps2 <- df_area |>
    dplyr::filter(Tipo == tipo2,
                  Percent == 100) |>
    dplyr::pull(Espécie)

  intersecao <- sf::st_intersection(area1, area2) |>
    sf::st_area() |>
    as.numeric() |>
    round(2)

  if(length(intersecao) == 0){

    intersecao <- 0

  }

  areaps1 <- df_area |>
    dplyr::filter(Tipo == tipo1,
                  Percent == 100) |>
    sf::st_transform(crs = 32725) |>
    sf::st_area() |>
    as.numeric()

  areaps2 <- df_area |>
    dplyr::filter(Tipo == tipo2,
                  Percent == 100) |>
    sf::st_transform(crs = 32725) |>
    sf::st_area() |>
    as.numeric()

  sps_1 <<- c(sps_1, sps1)

  sps_2 <<- c(sps_2, sps2)

  area_int <<- c(area_int, intersecao)

  area_ps1 <<- c(area_ps1, areaps1)

  area_ps2 <<- c(area_ps2, areaps2)

}

sps_1 <- c()

sps_2 <- c()

area_int <- c()

area_ps1 <- c()

area_ps2 <- c()

purrr::map(1:length(comb_mcp), int_mcp_100)

sps_1

sps_2

area_int

area_ps1

area_ps2

### Dataframe ----

df_mcp_100 <- tibble::tibble(`Species 1` = sps_1,
                             `Species 2` = sps_2,
                             `Intersection area (m²)` = area_int |> round(2),
                             `Species 1 area (m²)` = area_ps1 |> round(2),
                             `Species 2 area (m²)` = area_ps2 |> round(2)) |>
  dplyr::mutate(`Porcentage Species 1` = porcetagem(`Intersection area (m²)`,
                                                    `Species 1 area (m²)`) |> round(2),
                `Porcentage Species 2` = porcetagem(`Intersection area (m²)`,
                                                    `Species 2 area (m²)`) |> round(2))

df_mcp_100

df_mcp_100 |> dplyr::glimpse()

### Flextable ----

df_mcp_100_flex <- df_mcp_100 |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(width = 1.5) |>
  flextable::bg(bg = "white", part = "all") |>
  flextable::italic(j = 1:2)

df_mcp_100_flex

flextable::save_as_docx(df_mcp_100_flex,
                        path = "tabela_interseccao_mcp_100.docx")

## MCP 95% ----

### Calculando ----

int_mcp_095 <- function(id){

  tipo1 <- comb_mcp[[id]][1]

  tipo2 <- comb_mcp[[id]][2]

  area1 <- df_area |>
    dplyr::filter(Tipo == tipo1,
                  Percent == 95) |>
    sf::st_transform(crs = 32725)

  area2 <- df_area |>
    dplyr::filter(Tipo == tipo2,
                  Percent == 95) |>
    sf::st_transform(crs = 32725)

  sps1 <- df_area |>
    dplyr::filter(Tipo == tipo1,
                  Percent == 95) |>
    dplyr::pull(Espécie)

  sps2 <- df_area |>
    dplyr::filter(Tipo == tipo2,
                  Percent == 95) |>
    dplyr::pull(Espécie)

  intersecao <- sf::st_intersection(area1, area2) |>
    sf::st_area() |>
    as.numeric() |>
    round(2)

  if(length(intersecao) == 0){

    intersecao <- 0

  }

  areaps1 <- df_area |>
    dplyr::filter(Tipo == tipo1,
                  Percent == 95) |>
    sf::st_transform(crs = 32725) |>
    sf::st_area() |>
    as.numeric()

  areaps2 <- df_area |>
    dplyr::filter(Tipo == tipo2,
                  Percent == 95) |>
    sf::st_transform(crs = 32725) |>
    sf::st_area() |>
    as.numeric()

  sps_1 <<- c(sps_1, sps1)

  sps_2 <<- c(sps_2, sps2)

  area_int <<- c(area_int, intersecao)

  area_ps1 <<- c(area_ps1, areaps1)

  area_ps2 <<- c(area_ps2, areaps2)

}

sps_1 <- c()

sps_2 <- c()

area_int <- c()

area_ps1 <- c()

area_ps2 <- c()

purrr::map(1:length(comb_mcp), int_mcp_095)

sps_1

sps_2

area_int

area_ps1

area_ps2

### Dataframe ----

df_mcp_095 <- tibble::tibble(`Species 1` = sps_1,
                             `Species 2` = sps_2,
                             `Intersection area (m²)` = area_int |> round(2),
                             `Species 1 area (m²)` = area_ps1 |> round(2),
                             `Species 2 area (m²)` = area_ps2 |> round(2)) |>
  dplyr::mutate(`Porcentage Species 1` = porcetagem(`Intersection area (m²)`,
                                                    `Species 1 area (m²)`) |> round(2),
                `Porcentage Species 2` = porcetagem(`Intersection area (m²)`,
                                                    `Species 2 area (m²)`) |> round(2))

df_mcp_095

### Flextable ----

df_mcp_095_flex <- df_mcp_095 |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(width = 1.5) |>
  flextable::bg(bg = "white", part = "all") |>
  flextable::italic(j = 1:2)

df_mcp_095_flex

flextable::save_as_docx(df_mcp_095_flex,
                        path = "tabela_interseccao_mcp_095.docx")

## KDE 95% ----

### Calculando ----

int_kde_95 <- function(id){

  tipo1 <- comb_kde[[id]][1]

  tipo2 <- comb_kde[[id]][2]

  area1 <- df_area |>
    dplyr::filter(Tipo == tipo1,
                  Percent == 95) |>
    sf::st_transform(crs = 32725)

  area2 <- df_area |>
    dplyr::filter(Tipo == tipo2,
                  Percent == 95) |>
    sf::st_transform(crs = 32725)

  sps1 <- df_area |>
    dplyr::filter(Tipo == tipo1,
                  Percent == 95) |>
    dplyr::pull(Espécie)

  sps2 <- df_area |>
    dplyr::filter(Tipo == tipo2,
                  Percent == 95) |>
    dplyr::pull(Espécie)

  intersecao <- sf::st_intersection(area1, area2) |>
    sf::st_area() |>
    as.numeric() |>
    round(2)

  if(length(intersecao) == 0){

    intersecao <- 0

  }

  areaps1 <- df_area |>
    dplyr::filter(Tipo == tipo1,
                  Percent == 95) |>
    sf::st_transform(crs = 32725) |>
    sf::st_area() |>
    as.numeric()

  areaps2 <- df_area |>
    dplyr::filter(Tipo == tipo2,
                  Percent == 95) |>
    sf::st_transform(crs = 32725) |>
    sf::st_area() |>
    as.numeric()

  sps_1 <<- c(sps_1, sps1)

  sps_2 <<- c(sps_2, sps2)

  area_int <<- c(area_int, intersecao)

  area_ps1 <<- c(area_ps1, areaps1)

    area_ps2 <<- c(area_ps2, areaps2)


  }

sps_1 <- c()

sps_2 <- c()

area_int <- c()

area_ps1 <- c()

area_ps2 <- c()

purrr::map(1:length(comb_mcp), int_kde_95)

sps_1

sps_2

area_int

area_ps1

area_ps2

### Dataframe ----

df_kde_95 <- tibble::tibble(`Species 1` = sps_1,
                             `Species 2` = sps_2,
                             `Intersection area (m²)` = area_int |> round(2),
                             `Species 1 area (m²)` = area_ps1 |> round(2),
                             `Species 2 area (m²)` = area_ps2 |> round(2)) |>
  dplyr::mutate(`Porcentage Species 1` = porcetagem(`Intersection area (m²)`,
                                                    `Species 1 area (m²)`) |> round(2),
                `Porcentage Species 2` = porcetagem(`Intersection area (m²)`,
                                                    `Species 2 area (m²)`) |> round(2))

df_kde_95

### Flextable ----

df_kde_95_flex <- df_kde_95 |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(width = 1.5) |>
  flextable::bg(bg = "white", part = "all") |>
  flextable::italic(j = 1:2)

df_kde_95_flex

flextable::save_as_docx(df_kde_95_flex,
                        path = "tabela_interseccao_kde_95.docx")

## KDE 50% ----

### Calculando ----

int_kde_50 <- function(id){

  tipo1 <- comb_kde[[id]][1]

  tipo2 <- comb_kde[[id]][2]

  area1 <- df_area |>
    dplyr::filter(Tipo == tipo1,
                  Percent == 50) |>
    sf::st_transform(crs = 32725)

  area2 <- df_area |>
    dplyr::filter(Tipo == tipo2,
                  Percent == 50) |>
    sf::st_transform(crs = 32725)

  sps1 <- df_area |>
    dplyr::filter(Tipo == tipo1,
                  Percent == 50) |>
    dplyr::pull(Espécie)

  sps2 <- df_area |>
    dplyr::filter(Tipo == tipo2,
                  Percent == 50) |>
    dplyr::pull(Espécie)

  intersecao <- sf::st_intersection(area1, area2) |>
    sf::st_area() |>
    as.numeric() |>
    round(2)

  if(length(intersecao) == 0){

    intersecao <- 0

  }

  areaps1 <- df_area |>
    dplyr::filter(Tipo == tipo1,
                  Percent == 50) |>
    sf::st_transform(crs = 32725) |>
    sf::st_area() |>
    as.numeric()

  areaps2 <- df_area |>
    dplyr::filter(Tipo == tipo2,
                  Percent == 50) |>
    sf::st_transform(crs = 32725) |>
    sf::st_area() |>
    as.numeric()

  sps_1 <<- c(sps_1, sps1)

  sps_2 <<- c(sps_2, sps2)

  area_int <<- c(area_int, intersecao)

  area_ps1 <<- c(area_ps1, areaps1)

  area_ps2 <<- c(area_ps2, areaps2)

}

sps_1 <- c()

sps_2 <- c()

area_int <- c()

area_ps1 <- c()

area_ps2 <- c()

purrr::map(1:length(comb_mcp), int_kde_50)

sps_1

sps_2

area_int

area_ps1

area_ps2

### Dataframe ----

df_kde_50 <- tibble::tibble(`Species 1` = sps_1,
                             `Species 2` = sps_2,
                             `Intersection area (m²)` = area_int |> round(2),
                             `Species 1 area (m²)` = area_ps1 |> round(2),
                             `Species 2 area (m²)` = area_ps2 |> round(2)) |>
  dplyr::mutate(`Porcentage Species 1` = porcetagem(`Intersection area (m²)`,
                                                    `Species 1 area (m²)`) |> round(2),
                `Porcentage Species 2` = porcetagem(`Intersection area (m²)`,
                                                    `Species 2 area (m²)`) |> round(2))
df_kde_50

### Flextable ----

df_kde_50_flex <- df_kde_50 |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(width = 1.5) |>
  flextable::bg(bg = "white", part = "all") |>
  flextable::italic(j = 1:2)

df_kde_50_flex

flextable::save_as_docx(df_kde_50_flex,
                        path = "tabela_interseccao_kde_50.docx")
