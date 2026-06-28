# Pacotes ----

library(readxl)

library(tidyverse)

library(parzer)

library(geosphere)

library(sf)

library(flextable)

library(ggbeeswarm)

library(ggview)

# Dados ----

## Importando ----## Importando ----

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

## Visualizando ----

ls(pattern = "dados_") |>
  mget(envir = globalenv())

## Unindo os dados ----

sps_dados <- ls(pattern = "dados_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

sps_dados

# Tratando as coordenadas ----

## Convertendo as coordenadas ----

sps_dados <- sps_dados |>
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

sps_dados

## Completando as coordenadas ----

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

## Shapefile do trajeto total ----

trajeto <- sps_dados |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674) |>
  dplyr::group_by(ID) |>
  dplyr::summarise(do_union = FALSE) |>
  sf::st_cast("LINESTRING") |>
  dplyr::left_join(sps_dados |>
                     dplyr::select(ID, Espécie) |>
                     dplyr::distinct(ID, .keep_all = TRUE),
                   by = "ID")

trajeto |> sf::st_coordinates()

ggplot() +
  geom_sf(data = trajeto, aes(color = Espécie))

## Trajeto mínimo ----

trajeto_minimo <- sps_dados |>
  dplyr::group_by(ID) |>
  dplyr::slice(c(1, dplyr::n())) |>
  dplyr::ungroup() |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674) |>
  dplyr::group_by(ID) |>
  dplyr::summarise(do_union = FALSE) |>
  sf::st_cast("LINESTRING") |>
  dplyr::left_join(sps_dados |>
                     dplyr::select(ID, Espécie) |>
                     dplyr::distinct(ID, .keep_all = TRUE),
                   by = "ID")

trajeto_minimo

ggplot() +
  geom_sf(data = trajeto_minimo, aes(color = Espécie))

# Tortuosidade ----

## Índice de tortuosidade ----

comprimento_trajeto <- trajeto |>
  sf::st_transform(crs = 32725) |>
  sf::st_length()

comprimento_trajeto

comprimento_trajeto_minimo <- trajeto_minimo |>
  sf::st_transform(crs = 32725) |>
  sf::st_length()

comprimento_trajeto_minimo

tortuosidade <- 1 - as.numeric(comprimento_trajeto_minimo / comprimento_trajeto)

tortuosidade

## Tabela ----

### Dataframe ----

df_tor <- tibble::tibble(ID = sps_dados$ID |> unique()) |>
  dplyr::left_join(sps_dados |>
                     dplyr::select(ID, Espécie),
                   by = "ID") |>
  dplyr::rename("Species" = Espécie) |>
  dplyr::distinct(ID, .keep_all = TRUE) |>
  dplyr::mutate(`Tortuosity Index` = tortuosidade |> round(2))

df_tor

### Criando a tabela ----

df_tor_flex <- df_tor |>
  flextable::flextable() |>
  flextable::width(width = 2.25, j = 2) |>
  flextable::align(align = "center", part = "all")

df_tor_flex

### Exportando a tabela ----

df_tor_flex |>
  flextable::save_as_docx(path = "tabela_tortuosidade.docx")

# Gráfico ----

df_tor |>
  ggplot(aes(Species, `Tortuosity Index`)) +
  ggbeeswarm::geom_quasirandom(color = "black", size = 3.5, width = 0.1) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 17.5),
        axis.text.x = element_text(angle = 45,hjus = 1, face = "italic"),
        axis.title = element_text(color = "black", size = 17.5)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "grafico_tortuosidade_individuos.png",
       height = 10, width = 12)
