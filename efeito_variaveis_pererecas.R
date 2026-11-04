# Pacotes ----

library(googledrive)

library(readxl)

library(tidyverse)

library(parzer)

library(geosphere)

library(sf)

library(viridis)

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
  dplyr::filter(INDIVIDUO |> stringr::str_detect("B|PT")) |>
  dplyr::rename("ID" = 1) |>
  dplyr::mutate(ID = dplyr::case_when(

    ID |> stringr::str_detect("B") ~ ID |> stringr::str_replace("B", "BF"),
    .default = ID
  ),
                Especie = dplyr::case_when(

    ID |> stringr::str_detect("BF") ~ "Boana faber",
    .default = "Phyllomedusa tetraploidea"
    ))

### Visualizar ----

amb

amb |> dplyr::glimpse()

# Multicolinearidade ----

## Dados mordológicos ----

cor_morf <- purrr::map_dfr(
  c("Boana faber", "Phyllomedusa tetraploidea"),
  \(sps){

    matriz <- morf |>
      dplyr::filter(Especie == sps) |>
      dplyr::select_if(is.numeric) |>
      dplyr::rename("Body mass (g)" = "peso",
                    "Hindlimb proportion" = "Proporção CMA",
                    "Forelimb proportion" = "Proporção CMP",
                    "Hindlimb length" = "CMA",
                    "Forelimb length" = "CMP",
                    "SVL" = "crc") |>
      cor(method = "spearman") |>
      as.matrix()

    matriz[upper.tri(matriz)] <- NA

    matriz |>
      reshape2::melt() |>
      tidyr::drop_na() |>
      dplyr::filter(Var1 != Var2) |>
      dplyr::mutate(value = value |> round(2),
                    Especie = sps)

    },
  .progress = TRUE)

cor_morf

cor_morf |>
  ggplot(aes(Var1, Var2, fill = value, label = value)) +
  geom_tile(color = "black", linewidth = 1) +
  geom_text(size = 8) +
  labs(x = NULL,
       y = NULL,
       fill = "Spearman correlation") +
  coord_equal() +
  facet_wrap(~Especie) +
  scale_fill_gradientn(colours = c(viridis::viridis(n = 10) |> rev(),
                                   viridis::viridis(n = 10)),
                       limits = c(-1, 1),
                       guide = guide_colourbar(

                         title.position = "top",
                         title.hjust = 0.5,
                         frame.colour = "black",
                         ticks.colour = "black",
                         ticks.linewidth = 0.75,
                         barwidth = 30,
                         barheight = 1.5

                       )) +
  theme_bw() +
  theme(aspect.ratio = 1,
        axis.text = element_text(color = "black", size = 20),
        axis.text.x = element_text(angle = 90,
                                   hjust = 1, vjust = 0.5),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.position = "bottom",
        strip.text = element_text(color = "black", size = 30,
                                  face = "italic"),
        strip.background = element_rect(color = "black",
                                        linewidth = 1.5),
        panel.border = element_rect(color = "black", linewidth = 1.5))

ggsave(filename = "multicolinearidade_morf_pererecas.png",
       height = 12,
       width = 16)

"multicolinearidade_morf_pererecas.png" |>
  googledrive::drive_upload(path = "Artigo pererecas",
                            name = "multicolinearidade_morf_pererecas.png",
                            overwrite = TRUE)

## Dados ambientais ----

cor_amb <- purrr::map_dfr(
  c("Boana faber", "Phyllomedusa tetraploidea"),
  \(sps){

    matriz <- amb |>
      dplyr::filter(Especie == sps) |>
      dplyr::select_if(is.numeric) |>
      dplyr::rename("Max temperature (°C)" = "Temperatura máxima",
                    "Min temperature (°C)" = "Temperatura mínima",
                    "Mean relative humidity" = "Umidade Relativa -média",
                    "Max relative humidity" = "Umidade Relativa -máxima",
                    "Min relative humidity" = "Umidade Relativa - mínima",
                    "Precipitation (mm)" = "Precipitação",
                    "Mean temperature (°C)" = "Temperatura média") |>
      cor(method = "spearman") |>
      as.matrix()

    matriz[upper.tri(matriz)] <- NA

    matriz |>
      reshape2::melt() |>
      tidyr::drop_na() |>
      dplyr::filter(Var1 != Var2) |>
      dplyr::mutate(value = value |> round(2),
                    Especie = sps)

  },
  .progress = TRUE)

cor_amb

cor_amb |>
  ggplot(aes(Var1, Var2, fill = value, label = value)) +
  geom_tile(color = "black", linewidth = 1) +
  geom_text(size = 8) +
  labs(x = NULL,
       y = NULL,
       fill = "Spearman correlation") +
  coord_equal() +
  facet_wrap(~Especie) +
  scale_fill_gradientn(colours = c(viridis::viridis(n = 10) |> rev(),
                                   viridis::viridis(n = 10)),
                       limits = c(-1, 1),
                       guide = guide_colourbar(

                         title.position = "top",
                         title.hjust = 0.5,
                         frame.colour = "black",
                         ticks.colour = "black",
                         ticks.linewidth = 0.75,
                         barwidth = 30,
                         barheight = 1.5

                       )) +
  theme_bw() +
  theme(aspect.ratio = 1,
        axis.text = element_text(color = "black", size = 20),
        axis.text.x = element_text(angle = 90,
                                   hjust = 1, vjust = 0.5),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        legend.position = "bottom",
        strip.text = element_text(color = "black", size = 30,
                                  face = "italic"),
        strip.background = element_rect(color = "black",
                                        linewidth = 1.5),
        panel.border = element_rect(color = "black", linewidth = 1.5))

ggsave(filename = "multicolinearidade_amb_pererecas.png",
       height = 12,
       width = 16)

"multicolinearidade_amb_pererecas.png" |>
  googledrive::drive_upload(path = "Artigo pererecas",
                            name = "multicolinearidade_amb_pererecas.png",
                            overwrite = TRUE)
