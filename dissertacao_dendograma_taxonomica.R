# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(vegan)

library(ggdendro)

library(ggview)

# Dados ----

## Importando ----

comp <- readxl::read_xlsx("Diversidade de Shannon ABUNDANCIA.xlsx")

## Visualizando ----

comp

comp |> dplyr::glimpse()

## Tratando ----

comp  %<>%
  dplyr::select(1:5) %<>%
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      values_to = "Abundância",
                      names_to = "Local") %<>%
  tidyr::pivot_wider(names_from = Espécies,
                     values_from = Abundância) %<>%
  tibble::column_to_rownames(var = "Local")

comp

# Dendograma ----

## Matriz de distância de Bray-Curtis ----

matriz_bray <- comp |>
  vegan::vegdist()

matriz_bray

## Calculando UPGMA ----

upgma_tax <- matriz_bray |>
  hclust(method = "average")

upgma_tax

## Criando os dados de dendograma ----

dendro_data_tax <- upgma_tax |>
  as.dendrogram() |>
  ggdendro::dendro_data()

dendro_data_tax

## Separando os ramos ----

ramos_tax <- dendro_data_tax$segments

ramos_tax

## Nomes dos ramos ----

nomes_ramos_tax <- dendro_data_tax$labels

nomes_ramos_tax

## Gráfico ----

ggplot() +
  geom_segment(data = ramos_tax, aes(x = x, y = y,
                                 xend = xend, yend = yend),
               linewidth = 2) +
  labs(x = NULL,
       y = "Similaridade de Bray-Curtis") +
  scale_x_continuous(breaks = 1:4,
                     labels = nomes_ramos_tax$label) +
  #scale_y_continuous(limits = c(0, 0.38), expand = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 5,
                                   color = "black", size = 25),
        axis.text = element_text(color = "black", size = 25),
        axis.title = element_text(color = "black", size = 25),
        panel.grid.major.y = element_line(linewidth = 1, color = "gray80"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "dissertacao_upgma_tax.png", height = 10, width = 12)
