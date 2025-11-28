# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(ggdendro)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("Dados.xlsx")

## Visualizando ----

dados

dados |> dplyr::glimpse()

## Matriz de composição ----

matriz <- dados |>
  dplyr::mutate(Especie = Especie |> stringr::str_replace("_", " ")) |>
  dplyr::summarise(Abundancia = Abundancia |> max(),
                   .by = c(Especie, Area)) |>
  tidyr::pivot_wider(names_from = Especie,
                     values_from = Abundância,
                     values_fill = 0) |>
  tibble::column_to_rownames(var = "Area")

matriz

## Dados temporais ----

temp <- dados |>
  dplyr::mutate(`Horário e data` = paste0(Data, " : ", Hora) |>
                  lubridate::as_datetime())

temp

# UPGMA ----

## Calculando a similaridade de Bray-Curtis

bray <- 1 - matriz |> vegan::vegdist()

bray

## Calculando o UPGMA ----

upgma <- bray |>
  hclust(method = "average")

upgma

## Gráfico ----

### Extraindo os dados de Dendograma ----

upgma_data <- upgma |>
  as.dendrogram() |>
  ggdendro::dendro_data()

upgma_data

ramos <- upgma_data$segments

ramos

area <- upgma_data$label

area

### Dendograma ----

ggplot() +
  geom_segment(data = ramos, aes(x = x, y = y,
                                 xend = xend, yend = yend),
               linewidth = 1) +
  labs(x = NULL,
       y = "Similçaridade de Bray-Curtis") +
  scale_x_continuous(breaks = 1:4,
                     labels = agrup_hie$labels$label) +
  scale_y_continuous(expand = FALSE) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,
                                   color = "black", size = 17.5),
        axis.text = element_text(color = "black", size = 17.5),
        axis.title = element_text(color = "black", size = 17.5),
        title = element_text(color = "black", size = 17.5),
        panel.grid.major.y = element_line(linewidth = 1, color = "gray80"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank()) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "upgma.png", height = 10, width = 12)

# Curva de rarefação ----

# Diversidade taxonômica ----

## Riqueza e diversidade ----

## Equitabilidade de Hill ----

# Análises temporais ----
