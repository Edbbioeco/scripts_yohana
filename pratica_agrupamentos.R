# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(ggdendro)

library(factoextra)

library(ggview)

library(magrittr)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/mestrado/matriz_ambientais.xlsx")

## Visualizando ----

dados

dados |> dplyr::glimpse()

# Agrupamento hierárquico ----

## Calculando a matriz de distância ----

dist_euclid <- dados |>
  tibble::column_to_rownames(var = "Unidade Amostral") |> 
  vegan::decostand(method = "standardize") |>
  vegan::vegdist(method = "euclidean")

dist_euclid

## Calculando o UPGMA ----

upgma <- dist_euclid |>
  hclust(method = "average")

upgma

upgma |> plot()

## Visualizando ----

### Criando um dendro data ----

upgma_data <- upgma |>
  as.dendrogram() |>
  ggdendro::dendro_data()

upgma_data

### Gráfico ----

ggplot() +
  geom_segment(data = upgma_data$segments, aes(x = x, y = y,
                                               xend = xend, yend = yend),
               linewidth = 1) +
  labs(x = NULL,
       y = "Euclidean Distance") +
  scale_x_continuous(breaks = 1:11,
                     labels = upgma_data$labels$label) +
  scale_y_continuous(expand = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                   color = "black", size = 15),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        panel.grid.major.y = element_line(linewidth = 1, color = "gray80"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "upgma.png", height = 10, width = 12)

# Agrupamento não-hierárquico ----

## Número ideal de grupos ----

dados |>
  dplyr::select_if(is.numeric) |>
  vegan::cascadeKM(inf.gr = 2,
                   sup.gr = 10) |>
  plot()

## Calculando k-means ----

set.seed(123); dados |>
  dplyr::select_if(is.numeric) |>
  kmeans(centers = 2, nstart = 100) -> kmeans

kmeans

## Visualizando ----

kmeans |>
  factoextra::fviz_cluster(geom = "point",
                           data = dados |> dplyr::select_if(is.numeric)) +
  scale_fill_manual(values = c("orange", "royalblue", "seagreen")) +
  scale_color_manual(values = c("orange", "royalblue", "seagreen")) +
  theme_minimal() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        legend.position = "bottom") +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "kmeans.png", height = 10, width = 12)

# Agrupamento hierárquico com as informações do agrupamento não-hierárquico ----

## Tratando os dados de dendograma ----

chave_cluster <- tibble::tibble(cluster = kmeans$cluster |> as.character(),
                                label = dados$`Unidade Amostral`)

chave_cluster

upgma_data$labels %<>%
  dplyr::left_join(chave_cluster,
                   by = "label")

upgma_data$labels

## Visualizando ----

ggplot() +
  geom_segment(data = upgma_data$segments, aes(x = x, y = y,
                                               xend = xend, yend = yend),
               linewidth = 1) +
  geom_text(data = upgma_data$labels,
            aes(x = x, y = y, label = label, color = cluster),
            angle = 270,
            hjust = -0.05,
            fontface = "bold",
            size =  7.5) +
  labs(x = NULL,
       y = "Euclidean Distance") +
  scale_y_continuous(limits = c(-1, 5.5)) +
  scale_color_manual(values = c("orange", "royalblue", "seagreen")) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        panel.grid.major.y = element_line(linewidth = 1, color = "gray80"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.text = element_text(color = "black", size = 15),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "top") +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "upgma_cluster.png", height = 10, width = 12)