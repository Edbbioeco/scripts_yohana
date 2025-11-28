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

## Matriz de composição por tempo ----

matriz_temp <- dados |>
  tidyr::pivot_wider(names_from = Especie,
                     values_from = Abundância,
                     values_fill = 0)

matriz_temp

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

## Calculando a curva pra Chao1 ----

chao1 <- matriz_temp |>
  vegan::estaccumR() |>
  summary(display = c("S", "chao"))

chao1

dados_curva <- chao1$S |>
  tibble::as_tibble() |>
  dplyr::rename("Riqueza" = S,
                "Número de Parcelas" = N) |>
  dplyr::mutate(Tipo = "Observada") |>
  dplyr::bind_rows(chao1$chao |>
                     tibble::as_tibble() |>
                     dplyr::rename("Riqueza" = Chao,
                                   "Número de Dias" = N) |>
                     dplyr::mutate(Tipo = "Estimada"))

dados_curva

## Gráfico ----

dados_curva |>
  ggplot(aes(`Número de dias`, Riqueza, color = Tipo, fill = Tipo)) +
  geom_ribbon(aes(x = `Número de Parcelas`,
                  ymin = Riqueza - Std.Dev,
                  ymax = Riqueza + Std.Dev),
              alpha = 0.3,
              color = "transparent") +
  geom_line(linewidth = 1) +
  geom_point(color = "black", size = 5, shape = 21) +
  scale_x_continuous(breaks = seq(1, 11, 1),
                     limits = c(1, 11)) +
  scale_y_continuous(breaks = seq(3, 14, 1),
                     limits = c(3, 14)) +
  scale_color_manual(values = c("royalblue", "orange")) +
  scale_fill_manual(values = c("royalblue", "orange")) +
  labs(fill = NULL,
       color = NULL) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 15),
        axis.title = element_text(color = "black", size = 15),
        legend.text = element_text(color = "black", size = 15),
        panel.border = element_rect(color = "black"),
        legend.position = "bottom",
        panel.background = element_rect(color = "black", linewidth = 1))

ggsave(filename = "curva_de_rarefacao.png", height = 10, width = 12)

# Diversidade taxonômica ----

## Riqueza e diversidade ----

## Equitabilidade de Hill ----

# Análises temporais ----
