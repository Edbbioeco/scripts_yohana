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
                                   `Status BR`,
                                   `Status IUCN`) |>
                     dplyr::distinct(`Nome Científico`, .keep_all = TRUE),
                   by = "Nome Científico") |>
  dplyr::rename("Espécie" = 1) |>
  dplyr::relocate(Área,
                  .before = Espécie)

dados_bru_trat

dados_bru_trat |> dplyr::glimpse()

## Unir os dados ----

dados_unidos <- ls(pattern = "_trat$") |>
  mget(env = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = c(Área, Espécie)) |>
  dplyr::left_join(ls(pattern = "_trat$") |>
                     mget(env = globalenv()) |>
                     dplyr::bind_rows() |>
                     dplyr::select(Espécie,
                                   `Status BR`,
                                   `Status IUCN`) |>
                     dplyr::distinct(Espécie, .keep_all = TRUE),
                   by = "Espécie") |>
  dplyr::arrange(Área)

dados_unidos

dados_unidos |> dplyr::glimpse()

# Curvas ----

## Rarefação ----

### Calcular rarefação ----

rarefacao <- dados_unidos |>
  dplyr::select(1:3) |>
  tidyr::pivot_wider(names_from = 2,
                     values_from = 3,
                     values_fill = 0) |>
  tibble::column_to_rownames(var = "Área") |>
  colSums() |>
  iNEXT::iNEXT(q = 0,
               datatype = "abundance",
               endpoint = 40)

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

riqueza <- dados_unidos |>
  dplyr::select(1:3) |>
  tidyr::pivot_wider(names_from = 2,
                     values_from = 3,
                     values_fill = 0) |>
  tibble::column_to_rownames(var = "Área") |>
  vegan::specaccum(method = "rarefaction") %>%
  .$richness

riqueza

sd <- dados_unidos |>
  dplyr::select(1:3) |>
  tidyr::pivot_wider(names_from = 2,
                     values_from = 3,
                     values_fill = 0) |>
  tibble::column_to_rownames(var = "Área") |>
  vegan::specaccum(method = "rarefaction") %>%
  .$sd

sd

jack1 <- dados_unidos |>
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
                          sd = c(0, rep(sd[1], 2)))

df_rare

### Gráfico ----

ggplot() +
  geom_line(data = df_rare,
            aes(x = `Número de locais`, y = Riqueza + sd,
                linetype = "Desvio Padrão"),
            color = "black",
            linewidth = 2) +
  geom_line(data = df_rare,
            aes(x = `Número de locais`, y = Riqueza,
                linetype = "Curva de Acumulação de Espécies"),
            color = "black",
            linewidth = 2) +
  geom_line(data = df_rare,
            aes(x = `Número de locais`, y = Riqueza - sd,
                linetype = "Desvio Padrão"),
            color = "black",
            linewidth = 2) +
  labs(x = "Campanhas",
       y = "Espécies",
       title = "Curva do coletor",
       linetype = NULL) +
  scale_x_continuous(breaks = seq(0, 2, 1),
                     limits = c(0, 2),
                     expand = FALSE) +
  scale_y_continuous(breaks = seq(0, 12, 2),
                     limits = c(0, 12),
                     expand = FALSE) +
  guides(linetype = guide_legend(keywidth = unit(90, "pt"))) +
  theme_classic() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey88", linewidth = 0.4),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(color = "black", size = 25),
        axis.text.x = element_text(margin = margin(t = 20)),
        axis.text.y = element_text(margin = margin(r = 20)),
        axis.title = element_text(color = "black", size = 25),
        axis.title.y = element_text(margin = margin(r = 6)),
        axis.title.x = element_text(margin = margin(t = 6)),
        plot.title = element_text(hjust = 0.5, size = 45,
                                  face = "plain", margin = margin(b = 8)),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA),
        legend.key.width = unit(28, "pt"),
        legend.text = element_text(size = 25),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.direction = "horizontal",
        legend.spacing.x = unit(12, "pt"),
        plot.margin = margin(10, 14, 8, 8)) +
  ggview::canvas(height = 10, width = 20)

ggsave(filename = "consultoria_08.06.2026/estimador_Jackknife1.png",
       height = 10, width = 20)

# ìndicesd de diversidade ----

## Calcular índices ----

indices <- purrr::map(c("shannon", "simpson"),
                      \(indice){

                        purrr::imap(list(dados_ini_trat,
                                         dados_bru_trat |>
                                           dplyr::mutate(
                                             Abundância = Abundância |>
                                               as.numeric())) |>
                                      setNames(c("Outono 2026",
                                                 "Verão 2026")),
                                   \(dados, estacao){

                          div <- dados |>
                            dplyr::select(1:3) |>
                            tidyr::pivot_wider(names_from = 2,
                                               values_from = 3,
                                               values_fill = 0) |>
                            tibble::column_to_rownames(var = "Área") |>
                            vegan::diversity(index = indice)

                          tibble::tibble(Área = LETTERS[1:2],
                                         div = div) |>
                            dplyr::mutate(Estação = estacao,
                                          tipo = indice)

                        })

                      },
                      .progress = TRUE) |>
  purrr::list_flatten() |>
  dplyr::bind_rows() |>
  tidyr::pivot_wider(names_from = tipo,
                     values_from = div) |>
  dplyr::mutate(pielou = shannon / log(dados_unidos |>
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
