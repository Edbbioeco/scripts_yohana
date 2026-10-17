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

### Converter coordenadas ----

mov <- mov |>
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

mov

mov |> dplyr::glimpse()

### Completar as coordenadas ----

mov_trat <- purrr::reduce(
  .init = mov,
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

  })

mov_trat

## Dados morfológicos ----

### Baixar do Google Drive ----

Sys.getenv("pasta_artigo_rhinella") |>
  googledrive::drive_get() |>
  googledrive::drive_ls() |>
  dplyr::filter(name |> stringr::str_detect("médias")) |>
  dplyr::pull(name) |>
  purrr::map(
    \(arquivo){

      googledrive::drive_download(file = arquivo,
                                  path = paste0("./", arquivo),
                                  overwrite = TRUE)

    },
    .progress = TRUE)

### Importar ----

morf <- purrr::map_dfr(
  c("Rhinella crucifer",
    "Rhinella icterica"),
  \(sps){

    readxl::read_xlsx("./médias das espécies.xlsx",
                      sheet = sps) |>
      dplyr::mutate(Espécie = sps)

    },
  .progress = TRUE)

### Visualizar ----

morf

morf |> dplyr::glimpse()

# Modelos lineares ----

## Calcular área de vida ----

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
                                                      "KDE 50%"))) |>
  dplyr::mutate(area = geometry |>
                  sf::st_transform(crs = 31982) |>
                  sf::st_area() |>
                  as.numeric())

area_rhinella

## Multicolinaridade ----

cor_rhinellas <- purrr::map_dfr(
  c("Rhinella crucifer",
    "Rhinella icterica"),
  \(sps){

    cor_mat <- morf |>
      dplyr::filter(Espécie == sps) |>
      dplyr::select_if(is.numeric) |>
      cor(method = "spearman") |>
      as.matrix()

    cor_mat[upper.tri(cor_mat)] <- NA

    cor_mat |>
      reshape2::melt() |>
      tidyr::drop_na() |>
      dplyr::filter(Var1 != Var2) |>
      dplyr::mutate(value = value |> round(2),
                    Espécie = sps)

    },
  .progress = TRUE)

cor_rhinellas

cor_rhinellas |>
  ggplot(aes(Var1, Var2, fill = value, label = value)) +
  geom_tile(color = "black", linewidth = 1) +
  geom_text(size = 8) +
  labs(x = NULL,
       y = NULL,
       fill = "Spearman correlation") +
  coord_equal() +
  facet_wrap(~Espécie) +
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

ggsave(filename = "multicolinearidade_rhinella.png",
       height = 12,
       width = 16)

## unir os dados ----

area_mof_rhinella <- area_rhinella |>
  sf::st_drop_geometry() |>
  dplyr::rename("ID" = Indivíduo) |>
  dplyr::select(ID, area) |>
  dplyr::left_join(morf,
                   by = "ID")

area_mof_rhinella
