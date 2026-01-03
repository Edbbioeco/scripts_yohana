# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(parzer)

library(geosphere)

library(sf)

library(ggspatial)

library(adehabitatHR)

# Dados ----

## Boana faber ----

### Importando ----

faber <- readxl::read_xlsx("Dados movimentação PEVV 2025 Análise .xlsx")

### Visualizando ----

faber

faber |> dplyr::glimpse()

### Tratando ----

faber %<>%
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat(),
                `Distância (cm)` = `Distância (cm)`/100)

faber

faber |> dplyr::glimpse()

## Rhinella crucifer ----

### Importando ----

crucifer <- readxl::read_xlsx("Dados movimentação PEVV 2025 Análise .xlsx",
                              sheet = 2)

### Visualizando ----

crucifer

crucifer |> dplyr::glimpse()

### Tratando ----

crucifer %<>%
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat(),
                `Distância (cm)` = `Distância (cm)`/100)

crucifer

crucifer |> dplyr::glimpse()

## Phyllomedusa tetraploidea ----

### Importando ----

tetraploidea <- readxl::read_xlsx("Dados movimentação PEVV 2025 Análise .xlsx",
                              sheet = 3)

### Visualizando ----

tetraploidea

tetraploidea |> dplyr::glimpse()

### Tratando ----

tetraploidea %<>%
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat(),
                `Distância (cm)` = `Distância (cm)`/100)

tetraploidea |> as.data.frame()

tetraploidea |> dplyr::glimpse()

# Criando shapefile de trajeto ----

## Boana faber ----

### Dataframe de trajeto ----

traj_faber <- faber

traj_faber

### Calculando as novas coordenadas ----

convertendo_coords_faber <- function(x){

  if(traj_faber$Longitude[x] |> is.na() | traj_faber$Latitude[x] |> is.na()){

    dest <- geosphere::destPoint(c(traj_faber$Longitude[x-1],
                                   traj_faber$Latitude[x-1]),
                                 traj_faber$`Ângulos de virada`[x-1],
                                 traj_faber$`Distância (cm)`[x-1])

    traj_faber$Longitude[x] <<- dest[1]

    traj_faber$Latitude[x] <<- dest[2]

  }

}

purrr::walk(1:nrow(traj_faber), convertendo_coords_faber)

traj_faber |> as.data.frame()

### Criando o shapefile ----

#### Trajeto ----

traj_faber_sf <- traj_faber |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674) |>
  dplyr::summarise(do_union = FALSE) |>
  sf::st_cast("LINESTRING")

traj_faber_sf

#### Pontos ----

pontos_faber_sf <- traj_faber |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674)

pontos_faber_sf

#### Mapa ----

ggplot() +
  geom_sf(data = traj_faber_sf, linewidth = 1) +
  geom_sf_text(data = pontos_faber_sf,
               aes(label = Pontos),
               color = "red") +
  ggspatial::annotation_scale(location = "tl",
                              text_cex = 1.5,
                              text_face = "bold",
                              height = unit(0.5, "cm")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black"))

## Rhinella crucifer ----

### Dataframe de trajeto ----

traj_crucifer <- crucifer

traj_crucifer |> as.data.frame()

### Calculando as novas coordenadas ----

convertendo_coords_crucifer <- function(x){

  if(traj_crucifer$Longitude[x] |> is.na() | traj_crucifer$Latitude[x] |> is.na()){

    dest <- geosphere::destPoint(c(traj_crucifer$Longitude[x-1],
                                   traj_crucifer$Latitude[x-1]),
                                 traj_crucifer$`Ângulos de virada`[x-1],
                                 traj_crucifer$`Distância (cm)`[x-1])

    traj_crucifer$Longitude[x] <<- dest[1]

    traj_crucifer$Latitude[x] <<- dest[2]

  }

}

purrr::walk(1:nrow(traj_crucifer), convertendo_coords_crucifer)

traj_crucifer |> as.data.frame()

### Criando o shapefile ----

#### Trajeto ----

traj_crucifer_sf <- traj_crucifer |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674) |>
  dplyr::mutate(ID = crucifer$ID) |>
  dplyr::group_by(ID) |>
  dplyr::summarise(do_union = FALSE) |>
  sf::st_cast("LINESTRING") |>
  dplyr::ungroup()

traj_crucifer_sf

#### Pontos ----

pontos_crucifer_sf <- traj_crucifer |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674)

pontos_crucifer_sf

#### Mapa ----

ggplot() +
  geom_sf(data = traj_crucifer_sf,
          aes(color = ID),
          linewidth = 1) +
  geom_sf_text(data = pontos_crucifer_sf,
          aes(color = ID, label = Pontos)) +
  ggspatial::annotation_scale(location = "tl",
                              text_cex = 1.5,
                              text_face = "bold",
                              height = unit(0.5, "cm")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black"))

## Phyllomedusa tetraploidea ----

### Dataframe de trajeto ----

traj_tetraploidea <- tetraploidea

traj_tetraploidea |> as.data.frame()

### Calculando as novas coordenadas ----

convertendo_coords_tetraploidea <- function(x){

  if(traj_tetraploidea$Longitude[x] |> is.na() | traj_tetraploidea$Latitude[x] |> is.na()){

    dest <- geosphere::destPoint(c(traj_tetraploidea$Longitude[x-1],
                                   traj_tetraploidea$Latitude[x-1]),
                                 traj_tetraploidea$`Ângulos de virada`[x-1],
                                 traj_tetraploidea$`Distância (cm)`[x-1])

    traj_tetraploidea$Longitude[x] <<- dest[1]

    traj_tetraploidea$Latitude[x] <<- dest[2]

  }

}

purrr::walk(1:nrow(traj_tetraploidea), convertendo_coords_tetraploidea)

traj_tetraploidea |> as.data.frame()

### Criando o shapefile ----

#### Trajeto ----

traj_tetraploidea_sf <- traj_tetraploidea |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674) |>
  dplyr::summarise(do_union = FALSE) |>
  sf::st_cast("LINESTRING")

traj_tetraploidea_sf

#### Pontos ----

pontos_tetraploidea_sf <- traj_tetraploidea |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674)

pontos_tetraploidea_sf

#### Mapa ----

ggplot() +
  geom_sf(data = traj_tetraploidea_sf, linewidth = 1) +
  geom_sf(data = pontos_tetraploidea_sf, color = "red") +
  ggspatial::annotation_scale(location = "tl",
                              text_cex = 1.5,
                              text_face = "bold",
                              height = unit(0.5, "cm")) +
  theme_bw() +
  theme(axis.text = element_text(color = "black"))

# MCP ----

## Boana faber ----

### Convertendo para UTM-Zopne 25S ----

pontos_sf_utm_faber <- pontos_faber_sf |>
  sf::st_transform(crs = 32725)

pontos_sf_utm_faber

ggplot() +
  geom_sf(data = pontos_sf_utm_faber) +
  theme_bw() +
  theme(axis.text = element_text(color = "black"))

### 95% ----

mcp_faber_95 <- pontos_sf_utm_faber |>
  sf::as_Spatial() |>
  adehabitatHR::mcp(percent = 95) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(`% de pontos` = "95%")

mcp_faber_95

### 50% ----

mcp_faber_50 <- pontos_sf_utm_faber |>
  sf::as_Spatial() |>
  adehabitatHR::mcp(percent = 50) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(`% de pontos` = "50%")

mcp_faber_50

### Unindo os shapefiles ----

unido_faber_mcp <- ls(pattern = "mcp_faber") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::arrange(`% de pontos` |> dplyr::desc())

unido_faber_mcp

### Mapa ----

ggplot() +
  geom_sf(data = unido_faber_mcp |>
            sf::st_transform(crs = 4674),
          aes(color = `% de pontos`,
              fill = `% de pontos`),
          linewidth = 1) +
  coord_sf(expand = FALSE) +
  scale_color_manual(values = c("royalblue",
                                "orange")) +
  scale_fill_manual(values = c("royalblue",
                               "orange")) +
  guides(fill = guide_legend(title.position = "top"),
         color = guide_legend(title.position = "top")) +
  ggnewscale::new_scale_color() +
  geom_sf(data = pontos_faber_sf, aes(color = "Pontos de registro"), size = 2.5) +
  scale_color_manual(values = c("black")) +
  labs(colour = NULL,
       x = NULL,
       y = NULL) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              height = unit(0.5, "cm")) +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black"),
        legend.position = "bottom")

### Área ----

unido_faber_mcp |>
  sf::st_transform(crs = 32725) |>
  sf::st_area()

## Rhinella crucifer ----

### Convertendo para UTM-Zopne 25S ----

pontos_sf_utm_crucifer <- pontos_crucifer_sf |>
  sf::st_transform(crs = 32725)

pontos_sf_utm_crucifer

ggplot() +
  geom_sf(data = pontos_sf_utm_crucifer) +
  theme_bw() +
  theme(axis.text = element_text(color = "black"))

### 100% ----

### 95% ----

mcp_crucifer_95 <- pontos_sf_utm_crucifer |>
  sf::as_Spatial() |>
  adehabitatHR::mcp(percent = 95) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(`% de pontos` = "95%")

mcp_crucifer_95

### 50% ----

mcp_crucifer_50 <- pontos_sf_utm_crucifer |>
  sf::as_Spatial() |>
  adehabitatHR::mcp(percent = 50) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(`% de pontos` = "50%")

mcp_crucifer_50

### Unindo os shapefiles ----

unido_crucifer_mcp <- ls(pattern = "mcp_crucifer") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::arrange(`% de pontos` |> dplyr::desc())

unido_crucifer_mcp

### Mapa ----

ggplot() +
  geom_sf(data = unido_crucifer_mcp |>
            sf::st_transform(crs = 4674),
          aes(color = `% de pontos`,
              fill = `% de pontos`),
          linewidth = 1) +
  coord_sf(expand = FALSE) +
  scale_color_manual(values = c("royalblue",
                                "orange")) +
  scale_fill_manual(values = c("royalblue",
                               "orange")) +
  guides(fill = guide_legend(title.position = "top"),
         color = guide_legend(title.position = "top")) +
  ggnewscale::new_scale_color() +
  geom_sf(data = pontos_crucifer_sf, aes(color = "Pontos de registro"), size = 2.5) +
  scale_color_manual(values = c("black")) +
  labs(colour = NULL,
       x = NULL,
       y = NULL) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              height = unit(0.5, "cm")) +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black"),
        legend.position = "bottom")

### Área ----

unido_crucifer_mcp |>
  sf::st_transform(crs = 32725) |>
  sf::st_area()

## Phyllomedusa tetraploidea ----

### Convertendo para UTM-Zopne 25S ----

pontos_sf_utm_tetraploidea <- pontos_tetraploidea_sf |>
  sf::st_transform(crs = 32725)

pontos_sf_utm_tetraploidea

ggplot() +
  geom_sf(data = pontos_sf_utm_tetraploidea) +
  theme_bw() +
  theme(axis.text = element_text(color = "black"))

### 100% ----

### 95% ----

mcp_tetraploidea_95 <- pontos_sf_utm_tetraploidea |>
  sf::as_Spatial() |>
  adehabitatHR::mcp(percent = 95) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(`% de pontos` = "95%")

mcp_tetraploidea_95

### 50% ----

mcp_tetraploidea_50 <- pontos_sf_utm_tetraploidea |>
  sf::as_Spatial() |>
  adehabitatHR::mcp(percent = 50) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(`% de pontos` = "50%")

mcp_tetraploidea_50

### Unindo os shapefiles ----

unido_tetraploidea_mcp <- ls(pattern = "mcp_tetraploidea") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::arrange(`% de pontos` |> dplyr::desc())

unido_tetraploidea_mcp

### Mapa ----

ggplot() +
  geom_sf(data = unido_tetraploidea_mcp |>
            sf::st_transform(crs = 4674),
          aes(color = `% de pontos`,
              fill = `% de pontos`),
          linewidth = 1) +
  coord_sf(expand = FALSE) +
  scale_color_manual(values = c("royalblue",
                                "orange")) +
  scale_fill_manual(values = c("royalblue",
                               "orange")) +
  guides(fill = guide_legend(title.position = "top"),
         color = guide_legend(title.position = "top")) +
  ggnewscale::new_scale_color() +
  geom_sf(data = pontos_tetraploidea_sf, aes(color = "Pontos de registro"), size = 2.5) +
  scale_color_manual(values = c("black")) +
  labs(colour = NULL,
       x = NULL,
       y = NULL) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              height = unit(0.5, "cm")) +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black"),
        legend.position = "bottom")

### Área ----

unido_tetraploidea_mcp |>
  sf::st_transform(crs = 32725) |>
  sf::st_area()

# KDE ----

## Boana Faber ----

### 95% ----

kde_faber_95 <- pontos_sf_utm_faber |>
  sf::as_Spatial() |>
  adehabitatHR::kernelUD(h = "href") |>
  adehabitatHR::getverticeshr(percent = 95) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(`% de pontos` = "95%")

### 95% ----

kde_faber_50 <- pontos_sf_utm_faber |>
  sf::as_Spatial() |>
  adehabitatHR::kernelUD(h = "href") |>
  adehabitatHR::getverticeshr(percent = 50) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(`% de pontos` = "50%")

### Unindo os shapefiles ----

unido_faber_kde <- ls(pattern = "kde_faber") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::arrange(`% de pontos` |> dplyr::desc())

unido_faber_kde

### Mapa ----

ggplot() +
  geom_sf(data = unido_faber_kde |>
            sf::st_transform(crs = 4674),
          aes(color = `% de pontos`,
              fill = `% de pontos`),
          linewidth = 1) +
  coord_sf(expand = FALSE) +
  scale_color_manual(values = c("royalblue",
                                "orange")) +
  scale_fill_manual(values = c("royalblue",
                               "orange")) +
  guides(fill = guide_legend(title.position = "top"),
         color = guide_legend(title.position = "top")) +
  ggnewscale::new_scale_color() +
  geom_sf(data = pontos_faber_sf, aes(color = "Pontos de registro"), size = 2.5) +
  scale_color_manual(values = c("black")) +
  labs(colour = NULL,
       x = NULL,
       y = NULL) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              height = unit(0.5, "cm")) +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black"),
        legend.position = "bottom")

### Área ----

unido_faber_kde |>
  sf::st_transform(crs = 32725) |>
  sf::st_area()

## Rhinella crucifer ----

### 95% ----

kde_crucifer_95 <- pontos_sf_utm_crucifer |>
  sf::as_Spatial() |>
  adehabitatHR::kernelUD(h = "href") |>
  adehabitatHR::getverticeshr(percent = 95) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(`% de pontos` = "95%")

### 95% ----

kde_crucifer_50 <- pontos_sf_utm_crucifer |>
  sf::as_Spatial() |>
  adehabitatHR::kernelUD(h = "href") |>
  adehabitatHR::getverticeshr(percent = 50) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(`% de pontos` = "50%")

### Unindo os shapefiles ----

unido_crucifer_kde <- ls(pattern = "kde_crucifer") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::arrange(`% de pontos` |> dplyr::desc())

unido_crucifer_kde

### Mapa ----

ggplot() +
  geom_sf(data = unido_crucifer_kde |>
            sf::st_transform(crs = 4674),
          aes(color = `% de pontos`,
              fill = `% de pontos`),
          linewidth = 1) +
  coord_sf(expand = FALSE) +
  scale_color_manual(values = c("royalblue",
                                "orange")) +
  scale_fill_manual(values = c("royalblue",
                               "orange")) +
  guides(fill = guide_legend(title.position = "top"),
         color = guide_legend(title.position = "top")) +
  ggnewscale::new_scale_color() +
  geom_sf(data = pontos_crucifer_sf, aes(color = "Pontos de registro"), size = 2.5) +
  scale_color_manual(values = c("black")) +
  labs(colour = NULL,
       x = NULL,
       y = NULL) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              height = unit(0.5, "cm")) +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black"),
        legend.position = "bottom")

### Área ----

unido_crucifer_kde |>
  sf::st_transform(crs = 32725) |>
  sf::st_area()

## Phyllomedusa tetraploidea ----

### 95% ----

kde_tetraploidea_95 <- pontos_sf_utm_tetraploidea |>
  sf::as_Spatial() |>
  adehabitatHR::kernelUD(h = "href") |>
  adehabitatHR::getverticeshr(percent = 95) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(`% de pontos` = "95%")

### 95% ----

kde_tetraploidea_50 <- pontos_sf_utm_tetraploidea |>
  sf::as_Spatial() |>
  adehabitatHR::kernelUD(h = "href") |>
  adehabitatHR::getverticeshr(percent = 50) |>
  sf::st_as_sf() |>
  sf::st_transform(crs = 4674) |>
  dplyr::mutate(`% de pontos` = "50%")

### Unindo os shapefiles ----

unido_tetraploidea_kde <- ls(pattern = "kde_tetraploidea") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::arrange(`% de pontos` |> dplyr::desc())

unido_tetraploidea_kde

### Mapa ----

ggplot() +
  geom_sf(data = unido_tetraploidea_kde |>
            sf::st_transform(crs = 4674),
          aes(color = `% de pontos`,
              fill = `% de pontos`),
          linewidth = 1) +
  coord_sf(expand = FALSE) +
  scale_color_manual(values = c("royalblue",
                                "orange")) +
  scale_fill_manual(values = c("royalblue",
                               "orange")) +
  guides(fill = guide_legend(title.position = "top"),
         color = guide_legend(title.position = "top")) +
  ggnewscale::new_scale_color() +
  geom_sf(data = pontos_tetraploidea_sf, aes(color = "Pontos de registro"), size = 2.5) +
  scale_color_manual(values = c("black")) +
  labs(colour = NULL,
       x = NULL,
       y = NULL) +
  ggspatial::annotation_scale(text_cex = 1.5,
                              text_face = "bold",
                              height = unit(0.5, "cm")) +
  theme_bw() +
  theme(axis.text = element_text(size = 15, color = "black"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 15, color = "black"),
        legend.position = "bottom")

### Área ----

unido_tetraploidea_kde |>
  sf::st_transform(crs = 32725) |>
  sf::st_area()
