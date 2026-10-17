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

## Importar ----

dados_rhinellas <- purrr::map_dfr(
  c("Rhinella icterica", "Rhinella crucifer"),
  \(rhinella){

    readxl::read_xlsx(paste0("Dados ", rhinella, ".xlsx")) |>
      dplyr::mutate(Espécie = rhinella)

    },
  .progress = TRUE)

## Visualizar ----

dados_rhinellas

dados_rhinellas |> dplyr::glimpse()

## Converter coordenadas ----

dados_rhinellas %<>%
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

dados_rhinellas

## Completar coordenadas ----

dados_rhinellas <- purrr::reduce(
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

  },

  .init = dados_rhinellas)

dados_rhinellas

# Análise de área de vida ----

## Transformas as coordenadas em shapefiles ----

dados_rhinellas_sf <- dados_rhinellas |>
  sf::st_as_sf(coords = c(2:3),
               crs = 4674)

dados_rhinellas_sf

ggplot() +
  geom_sf(data = dados_rhinellas_sf, aes(color = Espécie))

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
  .progress = TRUE)

area_rhinella

## Mapas ----

### Rhinella icterica ----

mapa_icterica <- purrr::map(
  dados_rhinellas_sf |>
    dplyr::filter(Espécie == "Rhinella icterica") |>
    dplyr::pull(ID) |>
    unique(),
  \(ind){

    ggplot() +
      geom_sf(data = dados_rhinellas_sf |>
                dplyr::rename("Indivíduo" = ID) |>
                dplyr::filter(Indivíduo == ind),
              aes(color = "Individuals")) +
      geom_sf(data = area_rhinella |>
                dplyr::filter(Indivíduo == ind),
              aes(color = Tipo),
              fill = "transparent",
              linewidth = 1) +
      scale_color_manual(values = c("black", "royalblue", "orange")) +
      scale_fill_viridis_d(guide = guide_legend(
        title = "Individuals",
        title.position = "top",
        title.hjust = 0.5
        )) +
      labs(x = NULL,
           y = NULL,
           color = NULL) +
      ggspatial::annotation_scale(location = "br",
                                  text_col = "black",
                                  text_cex = 1.5,
                                  bar_cols = c("black", "gold")) +
      facet_wrap(~Indivíduo) +
      theme_bw() +
      theme(aspect.ratio = 1,
            axis.text = element_text(color = "black", size = 10),
            axis.text.x = element_text(angle = 90,
                                       hjust = 1, vjust = 0.5),
            legend.text = element_text(color = "black", size = 20),
            legend.title = element_text(color = "black", size = 20),
            strip.text = element_text(color = "black", size = 20),
            strip.background = element_rect(color = "black",
                                            linewidth = 1.5),
            panel.border = element_rect(color = "black", linewidth = 1),
            plot.title = element_text(color = "black", size = 20,
                                      face = "italic"))

    },
  .progress = TRUE) |>
  patchwork::wrap_plots(guides = "collect") &
  theme(legend.position = "bottom")

mapa_icterica

ggsave(plot = mapa_icterica,
       filename = "mapa_area_vida_rhinella_icterica.png",
       height = 10, width = 12)

### Rhinella crucifer ----

mapa_crucifer <- purrr::map(
  dados_rhinellas_sf |>
    dplyr::filter(Espécie == "Rhinella crucifer") |>
    dplyr::pull(ID) |>
    unique(),
  \(ind){

    ggplot() +
      geom_sf(data = dados_rhinellas_sf |>
                dplyr::rename("Indivíduo" = ID) |>
                dplyr::filter(Indivíduo == ind),
              aes(color = "Individuals")) +
      geom_sf(data = area_rhinella |>
                dplyr::filter(Indivíduo == ind),
              aes(color = Tipo),
              fill = "transparent",
              linewidth = 1) +
      scale_color_manual(values = c("black", "royalblue", "orange")) +
      scale_fill_viridis_d(guide = guide_legend(
        title = "Individuals",
        title.position = "top",
        title.hjust = 0.5
      )) +
      labs(x = NULL,
           y = NULL,
           color = NULL) +
      ggspatial::annotation_scale(location = "br",
                                  text_col = "black",
                                  text_cex = 1.5,
                                  bar_cols = c("black", "gold")) +
      facet_wrap(~Indivíduo) +
      theme_bw() +
      theme(aspect.ratio = 1,
            axis.text = element_text(color = "black", size = 10),
            axis.text.x = element_text(angle = 90,
                                       hjust = 1, vjust = 0.5),
            legend.text = element_text(color = "black", size = 20),
            legend.title = element_text(color = "black", size = 20),
            strip.text = element_text(color = "black", size = 20),
            strip.background = element_rect(color = "black",
                                            linewidth = 1.5),
            panel.border = element_rect(color = "black", linewidth = 1.5),
            plot.title = element_text(color = "black", size = 20,
                                      face = "italic"))

  },
  .progress = TRUE) |>
  patchwork::wrap_plots(guides = "collect") &
  theme(legend.position = "bottom")

mapa_crucifer

ggsave(plot = mapa_crucifer,
       filename = "mapa_area_vida_rhinella_crucifer.png",
       height = 10, width = 12)
