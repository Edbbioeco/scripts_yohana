# Pacotes ----

library(tidyverse)

library(readxl)

library(sf)

library(parzer)

library(geosphere)

library(adehabitatHR)

library(ggview)

library(flextable)

# Dados ----

## Registros de ocorrência ----

### Importar ----

sps <- c("Odontophrynus asper",
         "Rhinella crucifer",
         "Phyllomedusa tetraploidea")

sps

dados_3sps <- purrr::map(sps, \(sps){

  readxl::read_xlsx(paste0("Dados ",
                           sps,
                           ".xlsx")) |>
    dplyr::mutate(Especie = sps)
  }) |>
  dplyr::bind_rows()

### Visualizar ----

dados_3sps

dados_3sps |> dplyr::glimpse()

## Shapefile da área ----

### Importar ----

uc <- sf::st_read("uc_municipal_ceuc_outros_construcao.shp") |>
  dplyr::filter(nome |> stringr::str_detect("Capivara"))

### Visualizar ----

uc

ggplot() +
  geom_sf(data = uc)

# Completar coordenadas ----

## Converter coordenadas ----

dados_3sps <- dados_3sps |>
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

dados_3sps

## Completar coordenadas ----

purrr::map(1:nrow(dados_3sps), \(x){

  if(dados_3sps$Longitude[x] |> is.na() | dados_3sps$Latitude[x] |> is.na()){

    dest <- geosphere::destPoint(c(dados_3sps$Longitude[x-1],
                                   dados_3sps$Latitude[x-1]),
                                 dados_3sps$`Angulo de virada`[x-1],
                                 dados_3sps$`Distância (m)`[x-1])

    dados_3sps$Longitude[x] <<- dest[1]

    dados_3sps$Latitude[x] <<- dest[2]

  }

  })

dados_3sps

0
# Área de vida ----

## Odontophrynus asper ----

### Grids ----

ext <- seq(2, 10, by = 1)

ext

### Gerar áreas de vida ----

areas_vida_asper <- purrr::map(
  ext,
  purrr::in_parallel(
    \(ext){

      purrr::map(c(50, 95),
                 \(percent){

                   dados_3sps |>
                     dplyr::filter(Especie == "Odontophrynus asper") |>
                     dplyr::mutate(ID = "OA") |>
                     sf::st_as_sf(coords = c(2:3),
                                  crs = 4674) |>
                     dplyr::select(1) |>
                     sf::st_transform(crs = 32725) |>
                     sf::as_Spatial() |>
                     adehabitatHR::kernelUD(h = "href",
                                            extent = ext) |>
                     adehabitatHR::getverticeshr(percent = percent) |>
                     sf::st_as_sf() |>
                     sf::st_transform(crs = 4674) |>
                     dplyr::mutate( Percent = percent |>
                                      as.character(),
                                    Extent = ext,
                                    Espécie = "Odontophrynus asper")

                   }
      )

      }
    ),
  .progress = TRUE) |>
  dplyr::bind_rows() |>
  dplyr::arrange(Percent |> dplyr::desc()) |>
  dplyr::arrange(Extent)

### Visualizar ----

areas_vida_asper

ggplot() +
  geom_sf(data = uc, color = "black", linewidth = 0.75) +
  geom_sf(data = areas_vida_asper,
          aes(color = Percent,
              fill = Percent),) +
  facet_wrap(~Extent) +
  scale_color_manual(values = c("royalblue", "orange")) +
  scale_fill_manual(values = c("royalblue", "orange")) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 20),
        legend.position = "bottom") +
  ggview::canvas(height = 10, width = 12)

### Valores de área de vida ----

areas_asper_flex <- areas_vida_asper |>
  as.data.frame() |>
  dplyr::arrange(Percent) |>
  dplyr::arrange(Extent) |>
  dplyr::select(area, Percent, Extent) |>
  dplyr::rename("Área (m²)" = area,
                "Percentual (%)" = Percent,
                "Extensão do grid" = Extent) |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(width = c(1.5, 1.5, 1.5), unit = "in")

areas_asper_flex

flextable::save_as_docx(path = "areas_odontophrynus_asper_flex.docx")

## Rhinella crucifer ----

### Gerar áreas de vida ----

areas_vida_crucifer <- purrr::map(
  ext,
  purrr::in_parallel(
    \(ext){

      purrr::map(c(50, 95),
                 \(percent){

                   dados_3sps |>
                     dplyr::filter(Especie == "Rhinella crucifer") |>
                     dplyr::mutate(ID = "OA") |>
                     sf::st_as_sf(coords = c(2:3),
                                  crs = 4674) |>
                     dplyr::select(1) |>
                     sf::st_transform(crs = 32725) |>
                     sf::as_Spatial() |>
                     adehabitatHR::kernelUD(h = "href",
                                            extent = ext) |>
                     adehabitatHR::getverticeshr(percent = percent) |>
                     sf::st_as_sf() |>
                     sf::st_transform(crs = 4674) |>
                     dplyr::mutate( Percent = percent |>
                                      as.character(),
                                    Extent = ext,
                                    Espécie = "Rhinella crucifer")

                 }
      )

    }
  ),
  .progress = TRUE) |>
  dplyr::bind_rows() |>
  dplyr::arrange(Percent |> dplyr::desc()) |>
  dplyr::arrange(Extent)

### Visualizar ----

areas_vida_crucifer

ggplot() +
  geom_sf(data = uc, color = "black", linewidth = 0.75) +
  geom_sf(data = areas_vida_crucifer,
          aes(color = Percent,
              fill = Percent),) +
  facet_wrap(~Extent) +
  scale_color_manual(values = c("royalblue", "orange")) +
  scale_fill_manual(values = c("royalblue", "orange")) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 20),
        legend.position = "bottom") +
  ggview::canvas(height = 10, width = 12)

### Valores de área de vida ----

areas_crucifer_flex <- areas_vida_crucifer |>
  as.data.frame() |>
  dplyr::arrange(Percent) |>
  dplyr::arrange(Extent) |>
  dplyr::select(area, Percent, Extent) |>
  dplyr::rename("Área (m²)" = area,
                "Percentual (%)" = Percent,
                "Extensão do grid" = Extent) |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(width = c(1.5, 1.5, 1.5), unit = "in")

areas_crucifer_flex

flextable::save_as_docx(path = "areas_rhinella_crucifer_flex.docx")

## Phyllomedusa tetraploidea  ----

### Grids ----

ext <- seq(1, 5, length.out = 10) |> round(2)

ext

### Gerar áreas de vida ----

areas_vida_tetraploidea <- purrr::map(
  ext,
  purrr::in_parallel(
    \(ext){

      purrr::map(c(50, 95),
                 \(percent){

                   dados_3sps |>
                     dplyr::filter(Especie == "Phyllomedusa tetraploidea") |>
                     dplyr::mutate(ID = "OA") |>
                     sf::st_as_sf(coords = c(2:3),
                                  crs = 4674) |>
                     dplyr::select(1) |>
                     sf::st_transform(crs = 32725) |>
                     sf::as_Spatial() |>
                     adehabitatHR::kernelUD(h = "href",
                                            extent = ext) |>
                     adehabitatHR::getverticeshr(percent = percent) |>
                     sf::st_as_sf() |>
                     sf::st_transform(crs = 4674) |>
                     dplyr::mutate( Percent = percent |>
                                      as.character(),
                                    Extent = ext,
                                    Espécie = "Phyllomedusa tetraploidea")

                 }
      )

    }
  ),
  .progress = TRUE) |>
  dplyr::bind_rows() |>
  dplyr::arrange(Percent |> dplyr::desc()) |>
  dplyr::arrange(Extent)

### Visualizar ----

areas_vida_tetraploidea

ggplot() +
  geom_sf(data = uc, color = "black", linewidth = 0.75) +
  geom_sf(data = areas_vida_tetraploidea,
          aes(color = Percent,
              fill = Percent),) +
  facet_wrap(~Extent) +
  scale_color_manual(values = c("royalblue", "orange")) +
  scale_fill_manual(values = c("royalblue", "orange")) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.title = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 20),
        legend.position = "bottom") +
  ggview::canvas(height = 10, width = 12)

### Valores de área de vida ----

areas_tetraploidea_flex <- areas_vida_tetraploidea |>
  as.data.frame() |>
  dplyr::arrange(Percent) |>
  dplyr::arrange(Extent) |>
  dplyr::select(area, Percent, Extent) |>
  dplyr::rename("Área (m²)" = area,
                "Percentual (%)" = Percent,
                "Extensão do grid" = Extent) |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(width = c(1.5, 1.5, 1.5), unit = "in")

areas_tetraploidea_flex

flextable::save_as_docx(path = "areas_phyllomedusa_tetraploidea_flex.docx")
