# Pacotes ----

library(ape)

library(ggtree)

library(ggview)

library(readxl)

library(tidyverse)

library(parzer)

library(geosphere)

library(sf)

library(adehabitatHR)

library(phytools)

library(patchwork)

# Dados ----

## Filogenia ----

### Importando ----

arvore <- ape::read.tree("consenso.phy")

### Visualizando ----

arvore

ggtree::ggtree(arvore, layout = "circular") +
  ggtree::geom_tiplab(size = 4,
                      color = "black",
                      fontface = "bold.italic",
                      offset = 14.5) +
  xlim(-100, 300) +
  ggtree::theme_tree() +
  ggview::canvas(height = 10, width = 12)

## Dados de movimentação ----

### Importando ----

sps_mov_dados <- purrr::map_dfr(
  especies,
  ~readxl::read_xlsx(paste0("Dados ", .x, ".xlsx")) |>
    dplyr::mutate(Espécie = .x),
  .progress = TRUE)

### Visualizando ----

sps_mov_dados

sps_mov_dados |> dplyr::glimpse()

# Tratando as coordenadas ----

## Convertendo as coordenadas ----

sps_mov_dados <- sps_mov_dados |>
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

sps_mov_dados

## Completando as coordenadas ----

purrr::map(
  1:nrow(sps_mov_dados),
  \(x){

    if(sps_mov_dados$Longitude[x] |> is.na() | sps_mov_dados$Latitude[x] |> is.na()){

      dest <- geosphere::destPoint(
        c(sps_mov_dados$Longitude[x-1],
          sps_mov_dados$Latitude[x-1]),
        sps_mov_dados$`Angulo de virada`[x-1],
        sps_mov_dados$`Distância (m)`[x-1])

      sps_mov_dados$Longitude[x] <<- dest[1]

      sps_mov_dados$Latitude[x] <<- dest[2]

      }

    },
  .progress = TRUE)

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

df_area <- purrr::map_dfr(
  ind,
  purrr::in_parallel(

    ~tibble::tibble(area_kernel95 = sf_dados |>
                      dplyr::filter(ID == .x) |>
                      sf::st_transform(crs = 32725) |>
                      sf::as_Spatial() |>
                      adehabitatHR::kernelUD(h = "href",
                                             extent = 3) |>
                      adehabitatHR::getverticeshr(percent = 95) |>
                      sf::st_as_sf() |>
                      sf::st_area() |>
                      as.numeric(),
                    area_kernel50 = sf_dados |>
                      dplyr::filter(ID == .x) |>
                      sf::st_transform(crs = 32725) |>
                      sf::as_Spatial() |>
                      adehabitatHR::kernelUD(h = "href",
                                             extent = 3) |>
                      adehabitatHR::getverticeshr(percent = 50) |>
                      sf::st_as_sf() |>
                      sf::st_area() |>
                      as.numeric()) |>
      dplyr::mutate(ID = .x)

    ),
  .progress = TRUE) |>
  dplyr::left_join(sps_mov_dados |>
                     dplyr::select(ID, Espécie) |>
                     dplyr::distinct(ID, .keep_all = TRUE),
                   by = "ID")

df_area

## Médias ----

df_area_medias <- df_area |>
  dplyr::summarise(dplyr::across(.cols = dplyr::where(is.numeric),
                                 .fns = ~mean(.)),
                   .by = Espécie) |>
  dplyr::mutate(Espécie = Espécie |>
                  forcats::fct_relevel(arvore$tip.label |>
                                         rev() |>
                                         stringr::str_replace("_",
                                                              " "))) |>
  dplyr::arrange(Espécie = Espécie |>
                   forcats::fct_relevel(arvore$tip.label |>
                                          rev() |>
                                          stringr::str_replace("_",
                                                               " ")))

df_area_medias

# Sinal filogenético ----

## Calculando ----

var <- df_area_medias |>
  dplyr::select(2:3) |>
  names()

var

var_df_phy <- purrr::map_dfr(
  var,
  purrr::in_parallel(

    ~tibble::tibble(k = paste0(
      "k = ",
      phytools::phylosig(tree = arvore,
                         x = df_area_medias[[.x]],
                         method = "K") |>
        as.numeric() |>
        round(2)),
      Variable = .x)

    ),
  .progress = TRUE)

var_df_phy

## Gráfico ----

### Cladograma ----

plot_arvore <- ggtree::ggtree(arvore, size = 1) +
  ggtree::theme_tree() +
  ggview::canvas(height = 10, width = 12)

plot_arvore

### Variação da tortuosidade ----

plot_tortuosidade <- df_area_medias |>
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      names_to = "Variable",
                      values_to = "Area value") |>
  dplyr::left_join(var_df_phy,
                   by = "Variable") |>
  dplyr::mutate(Variable = paste0(Variable |>
                                    stringr::str_remove("area_"),
                                  " = ",
                                  k)) |>
  ggplot(aes(`Area value`, Espécie)) +
  geom_col(color = "black",
           width = 0.1) +
  scale_y_discrete(position = "right") +
  #scale_x_continuous(limits = c(0, 1)) +
  labs(y = NULL,
       x = "Area (m)") +
  facet_wrap(~Variable, nrow = 1, scales = "free_x") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 17, color = "black", angle = 90,
                                   hjust = 1),
        axis.text.y = element_text(size = 17, color = "black",
                                   hjust = 0,
                                   face = "bold.italic"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 17, color = "black"),
        legend.position = "none",
        axis.title = element_text(size = 17, color = "black"),
        axis.line.x = element_line(color = "black", linewidth = 1),
        plot.title = element_text(size = 20, color = "black",
                                  hjust = 0.5),
        strip.text = element_text(size = 10, color = "black"),
        strip.background = element_rect(color = "black", fill = "gray")) +
  ggview::canvas(height = 10, width = 12)

plot_tortuosidade

## Sinalização finogenética ----

(plot_arvore + plot_tortuosidade) +
  ggview::canvas(height = 10, width = 14)

ggsave(filename = "sinalizacao_filogenetica_movimento.png",
       height = 10, width = 14)
