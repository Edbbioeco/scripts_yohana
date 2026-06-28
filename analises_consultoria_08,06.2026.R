# PAcotes ----

library(readxl)

library(tidyverse)

library(iNEXT)

library(vegan)

library(ggview)

# Dados ----

## Amostragem inicial ----

### Importar ----

dados_ini <- readxl::read_xlsx("Lista spp CGH São Francisco de Sales.xlsx")

### Visualizar ----

dados_ini

dados_ini |> dplyr::glimpse()

### Tratar dados ----

dados_ini_trat <- dados_ini |>
  dplyr::select(1, 2, 5, 6) |>
  tidyr::drop_na() |>
  dplyr::rename("Espécie" = 1,
                "Abundância" = 2) |>
  dplyr::mutate(Área = "A",
                .before = Espécie) |>
  dplyr::bind_rows(dados_ini |>
                     dplyr::select(3:6) |>
                     tidyr::drop_na() |>
                     dplyr::rename("Espécie" = 1,
                                   "Abundância" = 2) |>
                     dplyr::mutate(Área = "B",
                                   .before = Espécie))

dados_ini_trat

dados_ini_trat |> dplyr::glimpse()

## Dados brutos ----

### Importar ----

dados_bru <- readxl::read_xlsx("Dados Brutos Índices - CGH São F. de Sales - Mastofauna.xlsx",
                               sheet = 2)

### Visualizar ----

dados_bru

dados_bru |> dplyr::glimpse()

### Tratar os dados ----

dados_bru_trat <- dados_bru |>
  dplyr::summarise(Abundância = dplyr::n(),
                   .by = c(`Nome Científico`, Área)) |>
  dplyr::left_join(dados_bru |>
                     dplyr::select(`Nome Científico`,
                                   `Status br`,
                                   `Status IUCN`),
                   by = "Nome Científico") |>
  dplyr::rename("Espécie" = 1) |>
  dplyr::relocate(Área,
                  .before = Espécie)

dados_bru_trat

dados_bru_trat |> dplyr::glimpse()

# Curvas ----

## Rarefação ----

### Calcular rarefação ----

rarefacao <- dados_trat |>
  dplyr::select(1:3) |>
  tidyr::pivot_wider(names_from = 2,
                     values_from = 3,
                     values_fill = 0) |>
  tibble::column_to_rownames(var = "Área") |>
  colSums() |>
  iNEXT::iNEXT(q = 0,
               datatype = "abundance",
               endpoint = 20)

rarefacao

### Visualizar ----

rarefacao |>
  iNEXT::ggiNEXT(type = 1) +
  scale_linetype_discrete(labels = c("Interpolado", "Extrapolado")) +
  scale_colour_manual(values ="darkorange") +
  scale_fill_manual(values ="darkorange") +
  labs(x = "Número de indivíduos",
       y = " Riqueza de espécies",
       fill = NULL,
       color = NULL) +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid = element_line(color = "gray50", linetype = "dashed")) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "consultoria_08.06.2026/rarefacao.png",
       height = 10, width = 12)

## Estimador de Riqueza por Jackknife 1 ----

### Calcular ----

riqueza <- dados_trat |>
  dplyr::select(1:3) |>
  tidyr::pivot_wider(names_from = 2,
                     values_from = 3,
                     values_fill = 0) |>
  tibble::column_to_rownames(var = "Área") |>
  vegan::specaccum(method = "rarefaction") %>%
  .$richness

riqueza

sd <- dados_trat |>
  dplyr::select(1:3) |>
  tidyr::pivot_wider(names_from = 2,
                     values_from = 3,
                     values_fill = 0) |>
  tibble::column_to_rownames(var = "Área") |>
  vegan::specaccum(method = "rarefaction") %>%
  .$sd

sd

jack1 <- dados_trat |>
  dplyr::select(1:3) |>
  tidyr::pivot_wider(names_from = 2,
                     values_from = 3,
                     values_fill = 0) |>
  tibble::column_to_rownames(var = "Área") |>
  vegan::specpool() %>%
  .$jack1

jack1

df_rare <- tibble::tibble(`Número de locais` = 0:2,
                          `Riqueza` = c(0, riqueza),
                          sd = c(sd[1], sd))

df_rare

### Gráfico ----

df_rare |>
  ggplot(aes(`Número de locais`, Riqueza)) +
  geom_ribbon(aes(ymin = Riqueza - sd, ymax = Riqueza + sd),
              alpha = 0.5) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = jack1,
             linewidth = 1,
             linetype = "dashed") +
  geom_label(aes(x = 0,
                 y = jack1,
                 label = "Rqueza estimada por Jackknife 1"),
             color = "black",
             size = 7.5,
             linewidth = 1,
             fill = "gray",
             hjust = 0) +
  scale_x_continuous(breaks = seq(0, 2, 1)) +
  scale_y_continuous(breaks = seq(0, 6, 1)) +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
        axis.title = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid = element_line(color = "gray50", linetype = "dashed"),
        panel.border = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "consultoria_08.06.2026/estimador_Jackknife1.png",
       height = 10, width = 12)

# ìndicesd de diversidade ----

## Calcular índices ----

indices <- purrr::map(c("shannon", "simpson"),
                      \(indice){

                        div <- dados_trat |>
                          dplyr::select(1:3) |>
                          tidyr::pivot_wider(names_from = 2,
                                             values_from = 3,
                                             values_fill = 0) |>
                          tibble::column_to_rownames(var = "Área") |>
                          vegan::diversity(index = indice)

                        tibble::tibble(div = div) |>
                          dplyr::rename(!!indice := 1)

                      },
                      .progress = TRUE) |>
  dplyr::bind_cols() |>
  dplyr::mutate(Área = LETTERS[1:2],
                .before = shannon) |>
  dplyr::mutate(pielou = shannon / log(dados_trat |>
                                         dplyr::select(1:3) |>
                                         tidyr::pivot_wider(names_from = 2,
                                                            values_from = 3,
                                                            values_fill = 0) |>
                                         tibble::column_to_rownames(var = "Área") |>
                                          vegan::specnumber())) |>
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      values_to = "Diversidade",
                      names_to = "Índice")

indices

## Gráficos ----

### Gerar ----

purrr::map2(c("shannon", "simpson", "pielou"),
            c("Diversidade de Shannon-Wiener",
              "Diversidade de Gini-Simpson",
              "Equitabilidade de Pielou"),
            \(indice, indicex){

              indices |>
                dplyr::filter(Índice == indice) |>
                ggplot(aes(Área, Diversidade)) +
                geom_col(color = "black", width = 0.5) +
                labs(y = indicex) +
                theme_bw() +
                theme(axis.text = element_text(size = 20,
                                               color = "black"),
                      axis.title = element_text(size = 20,
                                                color = "black"),
                      legend.text = element_text(size = 20,
                                                 color = "black"),
                      legend.title = element_blank(),
                      legend.position = "bottom",
                      panel.grid = element_line(color = "gray50",
                                                linetype = "dashed"),
                      panel.border = element_rect(color = "black",
                                                  linewidth = 1)) +
                ggview::canvas(height = 10, width = 12)

            })

graficos <- purrr::map2(c("shannon", "simpson", "pielou"),
                        c("Diversidade de Shannon-Wiener",
                          "Diversidade de Gini-Simpson",
                          "Equitabilidade de Pielou"),
                        \(indice, indicex){

                         indices |>
                           dplyr::filter(Índice == indice) |>
                           ggplot(aes(Área, Diversidade)) +
                           geom_col(color = "black", width = 0.5) +
                           labs(y = indicex) +
                           theme_bw() +
                           theme(axis.text = element_text(size = 20,
                                                          color = "black"),
                                 axis.title = element_text(size = 20,
                                                           color = "black"),
                                 legend.text = element_text(size = 20,
                                                            color = "black"),
                                 legend.title = element_blank(),
                                 legend.position = "bottom",
                                 panel.grid = element_line(color = "gray50",
                                                           linetype = "dashed"),
                                 panel.border = element_rect(color = "black",
                                                             linewidth = 1))

                         })

### Salvar ----

purrr::map2(graficos,
            c("shannon", "simpson", "pielou"),
           ~ggsave(plot = .x,
                   filename = paste0("consultoria_08.06.2026/",
                                     .y,
                                     ".png"),
                   height = 10,
                   width = 12))
