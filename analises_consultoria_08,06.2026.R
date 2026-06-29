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

                          riqueza <- dados |>
                            dplyr::select(1:3) |>
                            tidyr::pivot_wider(names_from = 2,
                                               values_from = 3,
                                               values_fill = 0) |>
                            tibble::column_to_rownames(var = "Área") |>
                            vegan::specnumber() |>
                            (\(x) x[sort(names(x))])()

                          div <- dados |>
                            dplyr::select(1:3) |>
                            tidyr::pivot_wider(names_from = 2,
                                               values_from = 3,
                                               values_fill = 0) |>
                            tibble::column_to_rownames(var = "Área") |>
                            vegan::diversity(index = indice) |>
                            (\(x) x[sort(names(x))])()

                          tibble::tibble(Área = LETTERS[1:2],
                                         div = div,
                                         Riqueza = riqueza) |>
                            dplyr::mutate(Estação = estacao,
                                          tipo = indice)

                        })

                      },
                      .progress = TRUE) |>
  purrr::list_flatten() |>
  dplyr::bind_rows() |>
  tidyr::pivot_wider(names_from = tipo,
                     values_from = div) |>
  dplyr::mutate(
    pielou = shannon / log(Riqueza),
    pielou = dplyr::if_else(pielou |> is.nan(),
                            0,
                            pielou)) |>
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      values_to = "Diversidade",
                      names_to = "Índice") |>
  dplyr::mutate(Estação = Estação |>
                  forcats::fct_relevel("Verão 2026", "Outono 2026"))

indices

## Gráficos ----

### Gerar ----

purrr::map2(
  c("shannon", "simpson", "pielou"),
  c("Índice de Diversidade de Shannon-Wiener",
    "Índice de Diversidade de Gini-Simpson",
    "Índice de Equitabilidade de Pielou"),
  \(indice, indicex){

    indice_a_verao <- indices |>
                dplyr::filter(Índice == indice &
                                Estação == "Verão 2026" &
                                Área == "A") |>
      dplyr::pull(Diversidade) |>
      round(2)

    indice_b_verao <- indices |>
      dplyr::filter(Índice == indice &
                      Estação == "Verão 2026" &
                      Área == "B") |>
      dplyr::pull(Diversidade) |>
      round(2)

    indice_a_outono <- indices |>
      dplyr::filter(Índice == indice &
                      Estação == "Outono 2026" &
                      Área == "A") |>
      dplyr::pull(Diversidade) |>
      round(2)

    indice_b_outono <- indices |>
      dplyr::filter(Índice == indice &
                      Estação == "Outono 2026" &
                      Área == "B") |>
      dplyr::pull(Diversidade) |>
      round(2)

    indice_range <- if(indice == "shannon"){

      c(-0.67, 1.7)

      } else {

        c(-0.67, 1.05)

      }

    indice_seq <- if(indice == "shannon"){

      seq(0, 1.7, 0.1)

      } else {

        seq(0, 1, 0.1)

        }

    ggplot(indices |>
             dplyr::filter(Índice == indice),
           aes(x = Estação, y = Diversidade, fill = Área)) +
      annotate("segment",
               x = 0.6, xend = 2.6, y = indice_seq, yend = indice_seq,
               linewidth = 0.4, color = "gray") +
      geom_col(position = "dodge", width = 0.5) +
      scale_fill_manual(values=c("A" = "grey30",
                                 "B" = "grey75")) +
      scale_x_discrete(expand = c(0, 0.4)) +
      scale_y_continuous(
        breaks = indice_seq,
        labels = indice_seq |>
          as.character() |>
          stringr::str_replace("\\.", ","),
        limits = indice_range,
        expand = c(0, 0)
      ) +
      # Labels do eixo y redesenhados junto ao geom_vline
      annotate("text",
               x = 0.575,
               y = indice_seq,
               label = indice_seq |>
                 as.character() |>
                 stringr::str_replace("\\.", ","),
               size=7.5,
               hjust = 1, size = 3, color = "black"
      ) +
      # Linhas horizontais da tabela
      annotate("segment",
               x = 0.6, xend = 2.6, y = 0.00, yend = 0.00,
               linewidth = 0.4, color="grey75") +
      annotate("segment",
               x = 0.20, xend = 2.6, y = -0.22, yend = -0.22,
               linewidth=0.4, color="grey75") +
      annotate("segment",
               x = 0.20, xend = 2.6, y = -0.44, yend = -0.44,
               linewidth = 0.4, color = "grey75") +
      annotate("segment",
               x = 0.20, xend = 2.6, y = -0.66, yend = -0.66,
               linewidth = 0.4, color = "grey75") +
      # Linhas verticais da tabela
      annotate("segment",
               x = 0.2, xend = 0.2, y = -0.67, yend = -0.22,
               linewidth = 0.4, color = "grey75") +
      annotate("segment",
               x = 0.6, xend = 0.6, y = -0.67, yend = -0,
               linewidth = 0.4, color = "grey75") +
      annotate("segment",
               x = 1.55, xend = 1.55, y = -0.66, yend = 0,
               inewidth = 0.4, color = "grey75") +
      annotate("segment",
               x = 2.60, xend = 2.60, y = -0.66, yend = 0,
               linewidth = 0.4, color = "grey75") +
      # Cabeçalho
      annotate("text",
               x = 1, y = -0.11, label = "Verão 2026",
               hjust = 0.5, size = 9.5, color = "black", hjust = 0.5) +
      annotate("text",
               x = 2, y = -0.11, label = "Outono 2026",
               hjust = 0.5, size = 9.5, color = "black") +
      # Quadradinhos
      annotate("rect",
               xmin = 0.25, xmax = 0.4, ymin = -0.415, ymax = -0.245,
               fill = "grey30", color = NA) +
      annotate("rect",
               xmin = 0.25, xmax = 0.4, ymin = -0.635, ymax = -0.465,
               fill = "grey75", color = NA) +
      # Rótulos das áreas
      annotate("text",
               x = 0.425, y = -0.33, label = "Área A",
               hjust = 0, size = 7.5, color = "black") +
      annotate("text",
               x = 0.425, y = -0.55, label = "Área B",
               hjust = 0, size = 7.5, color = "black") +
      # Valores
      annotate("text",
               x = 1, y = -0.33, label = indice_a_verao,
               hjust = 0.5, size = 9.5, color = "black") +
      annotate("text",
               x = 1, y = -0.55, label = indice_b_verao,
               hjust = 0.5, size = 9.5, color = "black") +
      annotate("text",
               x = 2, y = -0.33, label = indice_a_outono,
               hjust = 0.5, size = 9.5, color = "black") +
      annotate("text",
               x = 2, y = -0.55, label = indice_b_outono,
               hjust = 0.5, size = 9.5, color = "black") +
      labs(title = indicex,
           x = NULL, y = NULL) +
      coord_cartesian(clip="off") +
      theme_classic() +
      theme(plot.background = element_rect(fill = "white", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text = element_text(color = "black", size = 25),
            axis.title = element_text(color = "black", size = 25),
            axis.title.y = element_text(margin = margin(r = 6)),
            axis.title.x = element_text(margin = margin(t = 6)),
            legend.background = element_rect(fill = "white", color = NA),
            legend.key = element_rect(fill = "white", color = NA),
            legend.key.width = unit(28, "pt"),
            legend.text = element_text(size = 25),
            legend.title = element_blank(),
            legend.position = "none",
            legend.direction = "horizontal",
            legend.spacing.x = unit(12, "pt"),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),   # suprime labels originais
            axis.ticks.y = element_blank(),   # suprime ticks originais
            axis.line.y = element_blank(),
            axis.line.x = element_blank(),
            plot.margin = margin(t=10, r=14, b=20, l=50),
            plot.title = element_text(hjust = 0.5, size = 35)) +
      ggview::canvas(height = 10, width = 16)

    },
  .progress = TRUE)

graficos <- purrr::map2(
  c("shannon", "simpson", "pielou"),
  c("Índice de Diversidade de Shannon-Wiener",
    "Índice de Diversidade de Gini-Simpson",
    "Índice de Equitabilidade de Pielou"),
  \(indice, indicex){

    indice_a_verao <- indices |>
      dplyr::filter(Índice == indice &
                      Estação == "Verão 2026" &
                      Área == "A") |>
      dplyr::pull(Diversidade) |>
      round(2)

    indice_b_verao <- indices |>
      dplyr::filter(Índice == indice &
                      Estação == "Verão 2026" &
                      Área == "B") |>
      dplyr::pull(Diversidade) |>
      round(2)

    indice_a_outono <- indices |>
      dplyr::filter(Índice == indice &
                      Estação == "Outono 2026" &
                      Área == "A") |>
      dplyr::pull(Diversidade) |>
      round(2)

    indice_b_outono <- indices |>
      dplyr::filter(Índice == indice &
                      Estação == "Outono 2026" &
                      Área == "B") |>
      dplyr::pull(Diversidade) |>
      round(2)

    indice_range <- if(indice == "shannon"){

      c(-0.67, 1.7)

    } else {

      c(-0.67, 1.05)

    }

    indice_seq <- if(indice == "shannon"){

      seq(0, 1.7, 0.1)

    } else {

      seq(0, 1, 0.1)

    }

    ggplot(indices |>
             dplyr::filter(Índice == indice),
           aes(x = Estação, y = Diversidade, fill = Área)) +
      annotate("segment",
               x = 0.6, xend = 2.6, y = indice_seq, yend = indice_seq,
               linewidth = 0.4, color = "gray") +
      geom_col(position = "dodge", width = 0.5) +
      scale_fill_manual(values=c("A" = "grey30",
                                 "B" = "grey75")) +
      scale_x_discrete(expand = c(0, 0.4)) +
      scale_y_continuous(
        breaks = indice_seq,
        labels = indice_seq |>
          as.character() |>
          stringr::str_replace("\\.", ","),
        limits = indice_range,
        expand = c(0, 0)
      ) +
      # Labels do eixo y redesenhados junto ao geom_vline
      annotate("text",
               x = 0.575,
               y = indice_seq,
               label = indice_seq |>
                 as.character() |>
                 stringr::str_replace("\\.", ","),
               size=7.5,
               hjust = 1, size = 3, color = "black"
      ) +
      # Linhas horizontais da tabela
      annotate("segment",
               x = 0.6, xend = 2.6, y = 0.00, yend = 0.00,
               linewidth = 0.4, color="grey75") +
      annotate("segment",
               x = 0.20, xend = 2.6, y = -0.22, yend = -0.22,
               linewidth=0.4, color="grey75") +
      annotate("segment",
               x = 0.20, xend = 2.6, y = -0.44, yend = -0.44,
               linewidth = 0.4, color = "grey75") +
      annotate("segment",
               x = 0.20, xend = 2.6, y = -0.66, yend = -0.66,
               linewidth = 0.4, color = "grey75") +
      # Linhas verticais da tabela
      annotate("segment",
               x = 0.2, xend = 0.2, y = -0.67, yend = -0.22,
               linewidth = 0.4, color = "grey75") +
      annotate("segment",
               x = 0.6, xend = 0.6, y = -0.67, yend = -0,
               linewidth = 0.4, color = "grey75") +
      annotate("segment",
               x = 1.55, xend = 1.55, y = -0.66, yend = 0,
               inewidth = 0.4, color = "grey75") +
      annotate("segment",
               x = 2.60, xend = 2.60, y = -0.66, yend = 0,
               linewidth = 0.4, color = "grey75") +
      # Cabeçalho
      annotate("text",
               x = 1, y = -0.11, label = "Verão 2026",
               hjust = 0.5, size = 9.5, color = "black", hjust = 0.5) +
      annotate("text",
               x = 2, y = -0.11, label = "Outono 2026",
               hjust = 0.5, size = 9.5, color = "black") +
      # Quadradinhos
      annotate("rect",
               xmin = 0.25, xmax = 0.4, ymin = -0.415, ymax = -0.245,
               fill = "grey30", color = NA) +
      annotate("rect",
               xmin = 0.25, xmax = 0.4, ymin = -0.635, ymax = -0.465,
               fill = "grey75", color = NA) +
      # Rótulos das áreas
      annotate("text",
               x = 0.425, y = -0.33, label = "Área A",
               hjust = 0, size = 7.5, color = "black") +
      annotate("text",
               x = 0.425, y = -0.55, label = "Área B",
               hjust = 0, size = 7.5, color = "black") +
      # Valores
      annotate("text",
               x = 1, y = -0.33, label = indice_a_verao,
               hjust = 0.5, size = 9.5, color = "black") +
      annotate("text",
               x = 1, y = -0.55, label = indice_b_verao,
               hjust = 0.5, size = 9.5, color = "black") +
      annotate("text",
               x = 2, y = -0.33, label = indice_a_outono,
               hjust = 0.5, size = 9.5, color = "black") +
      annotate("text",
               x = 2, y = -0.55, label = indice_b_outono,
               hjust = 0.5, size = 9.5, color = "black") +
      labs(title = indicex,
           x = NULL, y = NULL) +
      coord_cartesian(clip="off") +
      theme_classic() +
      theme(plot.background = element_rect(fill = "white", color = NA),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text = element_text(color = "black", size = 25),
            axis.title = element_text(color = "black", size = 25),
            axis.title.y = element_text(margin = margin(r = 6)),
            axis.title.x = element_text(margin = margin(t = 6)),
            legend.background = element_rect(fill = "white", color = NA),
            legend.key = element_rect(fill = "white", color = NA),
            legend.key.width = unit(28, "pt"),
            legend.text = element_text(size = 25),
            legend.title = element_blank(),
            legend.position = "none",
            legend.direction = "horizontal",
            legend.spacing.x = unit(12, "pt"),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_blank(),   # suprime labels originais
            axis.ticks.y = element_blank(),   # suprime ticks originais
            axis.line.y = element_blank(),
            axis.line.x = element_blank(),
            plot.margin = margin(t=10, r=14, b=20, l=50),
            plot.title = element_text(hjust = 0.5, size = 35)) +
      ggview::canvas(height = 10, width = 16)

  },
  .progress = TRUE)

### Salvar ----

purrr::map2(graficos,
            c("shannon", "simpson", "pielou"),
           ~ggsave(plot = .x,
                   filename = paste0("consultoria_08.06.2026/",
                                     .y,
                                     ".png"),
                   height = 10,
                   width = 16))
