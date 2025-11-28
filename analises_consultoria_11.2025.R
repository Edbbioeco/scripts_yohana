# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(ggdendro)

library(ggview)

library(flextable)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("Dados.xlsx")

## Visualizando ----

dados

dados |> dplyr::glimpse()

## Matriz de composição ----

matriz <- dados |>
  dplyr::mutate(Especies = Especies |> stringr::str_replace("_", " ")) |>
  dplyr::summarise(Abundancia = Abundancia |> max(),
                   .by = c(Especies, Área)) |>
  tidyr::pivot_wider(names_from = Especies,
                     values_from = Abundancia,
                     values_fill = 0) |>
  tibble::column_to_rownames(var = "Área")

matriz

## Matriz de composição por tempo ----

matriz_temp <- dados |>
  dplyr::summarise(Abundancia = Abundancia |> max(),
                  .by = c(Especies, Data)) |>
  tidyr::pivot_wider(names_from = Especies,
                     values_from = Abundancia,
                     values_fill = 0)

matriz_temp

## Dados temporais ----

temp <- dados |>
  dplyr::mutate(`Horário e data` = paste0(Data, " ", Hora) |>
                  lubridate::as_datetime())

temp |> dplyr::glimpse()

## Matriz para o diagram de Whitaker ----

matriz_whit <- dados |>
  dplyr::mutate(Especies = Especies |> stringr::str_replace("_", " ")) |>
  dplyr::summarise(Abundancia = Abundancia |> max(),
                   .by = c(Especies, Área)) |>
  dplyr::arrange(Área, Abundancia |> desc()) |>
  dplyr::mutate(Rank = dplyr::row_number(),
                .by = Área)

matriz_whit

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

Área <- upgma_data$label

Área

### Dendograma ----

ggplot() +
  geom_segment(data = ramos, aes(x = x, y = y,
                                 xend = xend, yend = yend),
               linewidth = 2) +
  labs(x = NULL,
       y = "Similçaridade de Bray-Curtis") +
  scale_x_continuous(breaks = 1:3,
                     labels = Área$label) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1,
                                   color = "black", size = 25),
        axis.text = element_text(color = "black", size = 25),
        axis.title = element_text(color = "black", size = 25),
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
                "Número de dias" = N) |>
  dplyr::mutate(Tipo = "Observada") |>
  dplyr::bind_rows(chao1$chao |>
                     tibble::as_tibble() |>
                     dplyr::rename("Riqueza" = Chao,
                                   "Número de dias" = N) |>
                     dplyr::mutate(Tipo = "Estimada"))

dados_curva

## Gráfico ----

dados_curva |>
  ggplot(aes(`Número de dias`, Riqueza, color = Tipo, fill = Tipo)) +
  geom_ribbon(aes(x = `Número de dias`,
                  ymin = Riqueza - Std.Dev,
                  ymax = Riqueza + Std.Dev),
              alpha = 0.3,
              color = "transparent") +
  geom_line(linewidth = 1) +
  geom_point(color = "black", size = 5, shape = 21) +
  scale_color_manual(values = c("royalblue", "orange")) +
  scale_fill_manual(values = c("royalblue", "orange")) +
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  labs(fill = NULL,
       color = NULL) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 25),
        axis.title = element_text(color = "black", size = 25),
        legend.text = element_text(color = "black", size = 25),
        legend.position = "bottom") +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "curva_de_rarefacao.png", height = 10, width = 12)

# Diversidade taxonômica ----

## Riqueza e diversidade ----

div_tax <- matriz |>
  vegan::renyi(scales = c(0, 1), hill = TRUE) |>
  tibble::rownames_to_column() |>
  dplyr::rename("Área" = 1,
                "q = 0" = 2,
                "q = 1" = 3)

div_tax

## Equitabilidade de Hill ----

eq_hill <- matriz |>
  vegan::renyi(scales = c(1, 2), hill = TRUE) |>
  tibble::rownames_to_column() |>
  dplyr::rename("Área" = 1) |>
  dplyr::mutate(`Equitabilidade de Hill` = `2` / `1`) |>
  dplyr::select(1, 4)

eq_hill

## Tabela ----

### Unindo os dados ----

tabela_div <- div_tax |>
  dplyr::left_join(eq_hill,
                   by = "Área")

tabela_div

### Criando a tabela flextable ----

flex_dic <- tabela_div |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(width = 2, j = 4) |>
  flextable::bg(bg = "white", part = "all")

flex_dic

### Salvanso a tabela -----

flextable::save_as_docx(path = "tabela_valores_diversidade.docx")

# Diagrama de Whitaker ----

matriz_whit |>
  ggplot(aes(Rank, Abundancia)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(1, 16, 2)) +
  scale_y_continuous(breaks = seq(1, 10, 1)) +
  facet_wrap(~ Área, scales = "free", ncol = 2) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 25),
        axis.title = element_text(color = "black", size = 25),
        legend.text = element_text(color = "black", size = 25),
        legend.position = "bottom",
        strip.text = element_text(color = "black", size = 25),
        strip.background = element_rect(color = "black", fill = "gray")) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "diagrama_whitaker.png", height = 10, width = 12)
