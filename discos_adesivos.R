# Pacotes ----

library(readxl)

library(tidyverse)

library(ggview)

library(vegan)

library(FactoMineR)

# Dados ----

## Boana faber ----

### Importando ----

bf <- readxl::read_xlsx("Dados discos adesivos.xlsx")

### Visualizando ----

bf

bf |> dplyr::glimpse()

## Phyllomedusa tetraploidea ----

### Importando ----

pt <- readxl::read_xlsx("Dados discos adesivos.xlsx",
                        sheet = 2)

### Visualizando ----

pt

pt |> dplyr::glimpse()

# Boana faber ----

## Largura ----

bf |>
  tidyr::pivot_longer(cols = dplyr::contains("Largura"),
                      values_to = "Largura",
                      names_to = "Tipo") |>
  ggplot(aes(CRC, Largura,fill = Sexo)) +
  geom_point(size = 5, shape = 21, stroke = 1) +
  facet_wrap(~Tipo) +
  ggview::canvas(height = 10, width = 12) +
  theme_bw()

ggsave(filename = "boana_faber_largura_discos.png",
       height = 10, width = 12)

## Área ----

bf |>
  tidyr::pivot_longer(cols = dplyr::contains("AREA"),
                      values_to = "Área",
                      names_to = "Tipo") |>
  ggplot(aes(CRC, Área,fill = Sexo)) +
  geom_point(size = 5, shape = 21, stroke = 1) +
  facet_wrap(~Tipo) +
  ggview::canvas(height = 10, width = 12) +
  theme_bw()

ggsave(filename = "boana_faber_area_discos.png",
       height = 10, width = 12)

## PCA ----

### Calculando a PCA ----

pca_bf <- bf |>
  dplyr::select(dplyr::contains("AREA")) |>
  vegan::decostand(method = "standardize") |>
  FactoMineR::PCA()

pca_bf

### Informações da PCA ----

pca_bf$eig

pca_bf$var

### Scores ----

paste0("PC1 (", pca_bf$eig[1, 2] |> round(2), "%)")

paste0("PC2 (", pca_bf$eig[2, 2] |> round(2), "%)")

scores_bf <- pca_bf$ind$coord |>
  as.data.frame() |>
  dplyr::select("PC1 (74.59%)" = Dim.1,
                "PC2 (14.455%)" = Dim.2)

scores_bf

### Gráfico ----

bf |>
  dplyr::bind_cols(scores_bf) |>
  ggplot(aes(`PC1 (74.59%)`, `PC2 (14.455%)`, fill = Sexo)) +
  geom_point(size = 5, shape = 21, stroke = 1) +
  ggview::canvas(height = 10, width = 12) +
  theme_bw()

ggsave(filename = "boana_faber_pca_discos.png",
       height = 10, width = 12)

bf |>
  dplyr::bind_cols(scores_bf) |>
  ggplot(aes(CRC, `PC1 (74.59%)`,fill = Sexo)) +
  geom_point(size = 5, shape = 21, stroke = 1) +
  ggview::canvas(height = 10, width = 12) +
  theme_bw()

ggsave(filename = "boana_faber_crc_pca_discos.png",
       height = 10, width = 12)

# Phyllomedusa tetraploidea ----

## Largura ----

pt |>
  tidyr::pivot_longer(cols = dplyr::contains("Largura"),
                      values_to = "Largura",
                      names_to = "Tipo") |>
  ggplot(aes(CRC, Largura, fill = Sexo)) +
  geom_point(size = 5, shape = 21, stroke = 1) +
  facet_wrap(~Tipo) +
  ggview::canvas(height = 10, width = 12) +
  theme_bw()

ggsave(filename = "phyllomedusa_tetraploidea_largura_discos.png",
       height = 10, width = 12)

## Área ----

pt |>
  tidyr::pivot_longer(cols = dplyr::contains("AREA"),
                      values_to = "Área",
                      names_to = "Tipo") |>
  ggplot(aes(CRC, Área, fill = Sexo)) +
  geom_point(size = 5, shape = 21, stroke = 1) +
  facet_wrap(~Tipo) +
  ggview::canvas(height = 10, width = 12) +
  theme_bw()

ggsave(filename = "phyllomedusa_tetraploidea_area_discos.png",
       height = 10, width = 12)

## PCA ----

### Calculando a PCA ----

pca_pt <- pt |>
  dplyr::select(dplyr::contains("AREA")) |>
  vegan::decostand(method = "standardize") |>
  FactoMineR::PCA()

pca_pt

### Informações da PCA ----

pca_pt$eig

pca_pt$var

### Scores ----

paste0("PC1 (", pca_pt$eig[1, 2] |> round(2), "%)")

paste0("PC2 (", pca_pt$eig[2, 2] |> round(2), "%)")

scores_pt <- pca_pt$ind$coord |>
  as.data.frame() |>
  dplyr::select("PC1 (49.32%)" = Dim.1,
                "PC2 (25.48%)" = Dim.2)

scores_pt

### Gráfico ----

pt |>
  dplyr::bind_cols(scores_pt) |>
  ggplot(aes(`PC1 (49.32%)`, `PC2 (25.48%)`, fill = Sexo)) +
  geom_point(size = 5, shape = 21, stroke = 1) +
  ggview::canvas(height = 10, width = 12) +
  theme_bw()

ggsave(filename = "phyllomedusa_tetraploidea_pca_discos.png",
       height = 10, width = 12)

pt |>
  dplyr::bind_cols(scores_pt) |>
  ggplot(aes(CRC, `PC1 (49.32%)`,fill = Sexo)) +
  geom_point(size = 5, shape = 21, stroke = 1) +
  ggview::canvas(height = 10, width = 12) +
  theme_bw()

ggsave(filename = "phyllomedusa_tetraploidea_crc_pca_discos.png",
       height = 10, width = 12)
