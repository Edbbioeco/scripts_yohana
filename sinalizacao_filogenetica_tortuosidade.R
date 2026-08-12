# Pacotes ----

library(ape)

library(ggtree)

library(ggview)

library(readxl)

library(tidyverse)

library(parzer)

library(geosphere)

library(sf)

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

especies <- c("Boana faber",
              "Phyllomedusa tetraploidea",
              "Rhinella crucifer",
              "Rhinella icterica",
              "Odontophrynus asper",
              "Leptodactylus mystacinus")

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

# Tortuosidade ----

## Shapefile do trajeto total ----

trajeto <- sps_mov_dados |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674) |>
  dplyr::group_by(ID) |>
  dplyr::summarise(do_union = FALSE) |>
  sf::st_cast("LINESTRING") |>
  dplyr::left_join(sps_mov_dados |>
                     dplyr::select(ID, Espécie) |>
                     dplyr::distinct(ID, .keep_all = TRUE),
                   by = "ID")

trajeto

ggplot() +
  geom_sf(data = trajeto, aes(color = Espécie))

## Shapefile do trajeto mínimo ----

trajeto_minimo <- sps_mov_dados |>
  dplyr::group_by(ID) |>
  dplyr::slice(c(1, dplyr::n())) |>
  dplyr::ungroup() |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674) |>
  dplyr::group_by(ID) |>
  dplyr::summarise(do_union = FALSE) |>
  sf::st_cast("LINESTRING") |>
  dplyr::left_join(sps_mov_dados |>
                     dplyr::select(ID, Espécie) |>
                     dplyr::distinct(ID, .keep_all = TRUE),
                   by = "ID")

trajeto_minimo

ggplot() +
  geom_sf(data = trajeto_minimo, aes(color = Espécie))

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

## Calculando a média por espécies ----

tortuosidade_phy <- tibble::tibble(Tortuosidade = tortuosidade,
               ID = sps_mov_dados |>
                 dplyr::pull(ID) |>
                 unique()) |>
  dplyr::left_join(sps_mov_dados |>
                     dplyr::select(ID, Espécie),
                   by = "ID") |>
  dplyr::summarise(Tortuosidade = Tortuosidade |> mean(),
                   .by = Espécie) |>
  dplyr::arrange(Espécie = Espécie |>
                   forcats::fct_relevel(arvore$tip.label |>
                                          rev() |>
                                          stringr::str_replace("_",
                                                               " "))) |>
  dplyr::mutate(Espécie = Espécie |>
                  forcats::fct_relevel(arvore$tip.label |>
                                         rev() |>
                                         stringr::str_replace("_",
                                                              " ")))

tortuosidade_phy

# Sinalização filogenética ----

## Calculando a sinalização ----

sinal <- phytools::phylosig(tree = arvore,
                            x = tortuosidade_phy$Tortuosidade,
                            method = "K")

sinal

## Visualizando ----

### Cladograma ----

plot_arvore <- ggtree::ggtree(arvore, size = 1) +
  ggtree::theme_tree() +
  ggview::canvas(height = 10, width = 12)

plot_arvore

### Variação da tortuosidade ----

plot_tortuosidade <- tortuosidade_phy |>
  dplyr::mutate(k_tortuosidade = paste0("k = ",
                                        sinal |> as.numeric() |> round(2))) |>
  ggplot(aes(Tortuosidade, Espécie, fill = Tortuosidade)) +
  geom_col(color = "black",
           width = 0.1) +
  scale_y_discrete(position = "right") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_fill_viridis_c() +
  labs(y = NULL,
       x = "Tortuosity") +
  facet_wrap(~k_tortuosidade) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17, color = "black"),
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
        strip.text = element_text(size = 17, color = "black"),
        strip.background = element_rect(color = "black", fill = "gray")) +
  ggview::canvas(height = 10, width = 12)

plot_tortuosidade

## Sinalização finogenética ----

(plot_arvore + plot_tortuosidade) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "sinalizacao_filogenetica_tortuosidade.png",
       height = 10, width = 12)
