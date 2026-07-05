# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(ggview)

library(ggdendro)

# Dados ----

## Importar ----

dados <- purrr::map(list.files(path = "./consultoria_04.07.2026/",
                               pattern = ".xlsx$",
                               full.names = TRUE),
                    readxl::read_xlsx) |>
  setNames(list.files(path = "./consultoria_04.07.2026/",
                      pattern = ".xlsx$") |>
             stringr::str_remove_all("dados_|.xlsx"))

## Visualizar ----

dados

purrr::map(dados, dplyr::glimpse)

## Tratar para calcular os dados de abundância ----

dados_trat <- purrr::map(dados,
                         purrr::in_parallel(

                           ~.x |>
                             dplyr::summarise(Abundância = dplyr::n(),
                                              .by = c(Campanha,
                                                      `Área Amostral`,
                                                      `Nome científico`,
                                                      Grupo)) |>
                             dplyr::mutate(Campanha = Campanha |> as.character())
                         ),
                         .progress = TRUE)

dados_trat

# Gráfico de riqueza por campanha ----

## Calcular riqueza por campanha ----

riqueza_camapnha <- dados_trat |>
  dplyr::bind_rows() |>
  dplyr::summarise(riqueza = dplyr::n(),
                   .by = Campanha) |>
  dplyr::mutate(Campanha = paste0(Campanha, "ª Campanha"))

riqueza_camapnha

riqueza_camapnha |> dplyr::glimpse()

## Gráfico ----

riqueza_camapnha |>
  ggplot(aes(Campanha, riqueza)) +
  geom_col(color = "#4472C4", fill = "#4472C4", width = 0.5) +
  labs(x = NULL,
       y = "N° de espécies por campanha") +
  scale_y_continuous(limits = c(0, 40),
                     expand = expansion(mult = c(0, 0.05))) +
  theme(panel.background = element_rect(color = "white", fill = "white"),
        axis.text = element_text(color = "black", size = 30),
        axis.text.x = element_text(vjust = -2),
        axis.text.y = element_text(hjust = 0),
        axis.title.y = element_text(vjust = 2),
        axis.title = element_text(color = "black", size = 30),
        axis.ticks = element_blank(),
        axis.line.x = element_line(color = "black", linewidth = 1),
        plot.background = element_rect(color = "black", linewidth = 1),
        plot.margin = margin(t = 10, r = 10, b = 25, l = 15)) +
  ggview::canvas(height = 12, width = 20)

ggsave(filename = "./consultoria_04.07.2026/riqueza_campanha.png",
       height = 12, width = 20)

# Curva de rarefação ----

## Calcular ----

dados_jackknife <- dados_trat |>
  dplyr::bind_rows() |>
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = c(Campanha,
                           `Nome científico`)) |>
  tidyr::pivot_wider(names_from = `Nome científico`,
                     values_from = Abundância,
                     values_fill = 0) |>
  dplyr::arrange(Campanha) |>
  tibble::column_to_rownames(var = "Campanha") |>
  vegan::poolaccum(minsize = 1) |>
  summary(display = c("S", "jack1")) |>
  setNames(c("Sobs", "Jackknife 1")) |>
  purrr::imap(~.x |>
                tibble::as_tibble() |>
                dplyr::rename(riqueza = 2) |>
                dplyr::mutate(tipo = .y)) |>
  dplyr::bind_rows()

dados_jackknife

dados_jackknife |> dplyr::glimpse()

## Gráfico ----

dados_jackknife |>
  ggplot(aes(N, riqueza, linetype = tipo)) +
  geom_line(linewidth = 1) +
  labs(x = NULL,
       y = NULL,
       linetype = NULL) +
  scale_linetype(breaks = c("Sobs", "Jackknife 1")) +
  guides(linetype = guide_legend(keywidth = unit(45, "pt"))) +
  theme(panel.background = element_rect(color = "white", fill = "white"),
        axis.text = element_text(color = "black", size = 30),
        axis.text.x = element_text(angle = 45, hjust = 3),
        axis.ticks = element_blank(),
        axis.line.x = element_line(color = "black", linewidth = 1),
        legend.text = element_text(color = "black", size = 30),
        legend.position = "bottom",
        plot.background = element_rect(color = "black", linewidth = 1)) +
    ggview::canvas(height = 12, width = 20)

ggsave(filename = "./consultoria_04.07.2026/jackknife.png",
       height = 12, width = 20)

## Índices de diversidade ----

## Mamíferos terrestres ----

### Riqueza ----

riqueza_terrestre <- dados_trat[dados_trat |> stringr::str_detect("terrestre")] |>
  dplyr::bind_rows() |>
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = c(`Área Amostral`,
                           `Nome científico`)) |>
  tidyr::pivot_wider(names_from = `Nome científico`,
                     values_from = Abundância,
                     values_fill = 0) |>
  dplyr::arrange(`Área Amostral`) |>
  tibble::column_to_rownames(var = "Área Amostral") |>
  vegan::specnumber()

riqueza_terrestre

### Shannon-Wiener ----

shannon_terrestre <- dados_trat[dados_trat |> stringr::str_detect("terrestre")] |>
  dplyr::bind_rows() |>
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = c(`Área Amostral`,
                           `Nome científico`)) |>
  tidyr::pivot_wider(names_from = `Nome científico`,
                     values_from = Abundância,
                     values_fill = 0) |>
  dplyr::arrange(`Área Amostral`) |>
  tibble::column_to_rownames(var = "Área Amostral") |>
  vegan::diversity()

shannon_terrestre

### Equitabilidade de Pielou ----

pielou_terrestre <- shannon_terrestre / log(riqueza_terrestre)

pielou_terrestre

### Data frame dos valores ----

df_div_terrestre <- tibble::tibble(area_amostral = riqueza_terrestre |> names(),
                                   shannon = shannon_terrestre |> round(2),
                                   Índices = "Shannon_H") |>
  tidyr::pivot_wider(names_from = area_amostral,
                     values_from = shannon) |>
  dplyr::bind_rows(tibble::tibble(area_amostral = riqueza_terrestre |> names(),
                                  pielou = pielou_terrestre |> round(2),
                                  Índices = "Equitabilidade_J") |>
                     tidyr::pivot_wider(names_from = area_amostral,
                                        values_from = pielou))

df_div_terrestre

### Tabela flextable ----

flex_terrestre <- df_div_terrestre |>
  flextable::flextable() |>
  flextable::fontsize(part = "all",
                      size = 8) |>
  flextable::add_header_row(values = c("Índices", "Áreas amostrais"),
                            colwidths = c(1, 10),
                            top = TRUE) |>
  flextable::merge_v(part = "header", j = "Índices") |>
  flextable::align(align = "center", part = "header") |>
  flextable::align(align = "center", j = 2:11, part = "body") |>
  flextable::bg(bg = "#1F3864", part = "header") |>
  flextable::color(color = "white", part = "header") |>
  flextable::bold(part = "header") |>
  flextable::border_remove() |>
  flextable::hline(i = 1,
                   part = "header",
                   border = officer::fp_border(color = "black", width = 1)) |>
  flextable::hline_bottom(part = "header",
                          border = officer::fp_border(color = "black",
                                                      width = 1.5)) |>
  flextable::hline_bottom(part = "body",
                          border = officer::fp_border(color = "black",
                                                      width = 1.5)) |>
  flextable::vline(j = 1, part = "header",
                   border = officer::fp_border(color = "black",
                                               width = 1)) |>
  flextable::vline(j = 2:11, part = "header",
                   border = officer::fp_border(color = "black",
                                               width = 1))

flex_terrestre

flex_terrestre |>
  flextable::save_as_docx(path = "./consultoria_04.07.2026/tabela_diversidade_terrestres.docx")

## Quirópteros ----

### Riqueza ----

riqueza_quiro <- dados_trat[dados_trat |>
                              names() |>
                              stringr::str_detect("_quiropteros_")] |>
  dplyr::bind_rows() |>
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = c(`Área Amostral`,
                           `Nome científico`)) |>
  tidyr::pivot_wider(names_from = `Nome científico`,
                     values_from = Abundância,
                     values_fill = 0) |>
  dplyr::arrange(`Área Amostral`) |>
  tibble::column_to_rownames(var = "Área Amostral") |>
  vegan::specnumber()

riqueza_terrestre

### Shannon-Wiener ----

shannon_quiro <- dados_trat[dados_trat |>
                              names() |>
                              stringr::str_detect("_quiropteros_")] |>
  dplyr::bind_rows() |>
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = c(`Área Amostral`,
                           `Nome científico`)) |>
  tidyr::pivot_wider(names_from = `Nome científico`,
                     values_from = Abundância,
                     values_fill = 0) |>
  dplyr::arrange(`Área Amostral`) |>
  tibble::column_to_rownames(var = "Área Amostral") |>
  vegan::diversity()

shannon_quiro

### Equitabilidade de Pielou ----

pielou_quiro <- shannon_quiro / log(riqueza_quiro)

pielou_quiro

### Data frame dos valores ----

df_div_quiro <- tibble::tibble(area_amostral = riqueza_terrestre |> names(),
                               shannon = shannon_terrestre |> round(2),
                               Índices = "Shannon_H") |>
  tidyr::pivot_wider(names_from = area_amostral,
                     values_from = shannon) |>
  dplyr::bind_rows(tibble::tibble(area_amostral = riqueza_terrestre |> names(),
                                  pielou = pielou_terrestre |> round(2),
                                  Índices = "Equitabilidade_J") |>
                     tidyr::pivot_wider(names_from = area_amostral,
                                        values_from = pielou))

df_div_quiro

### Tabela flextable ----

flex_quiro <- df_div_quiro |>
  flextable::flextable() |>
  flextable::fontsize(part = "all",
                      size = 8) |>
  flextable::add_header_row(values = c("Índices", "Áreas amostrais"),
                            colwidths = c(1, 10),
                            top = TRUE) |>
  flextable::merge_v(part = "header", j = "Índices") |>
  flextable::align(align = "center", part = "header") |>
  flextable::align(align = "center", j = 2:11, part = "body") |>
  flextable::bg(bg = "#1F3864", part = "header") |>
  flextable::color(color = "white", part = "header") |>
  flextable::bold(part = "header") |>
  flextable::border_remove() |>
  flextable::hline(i = 1,
                   part = "header",
                   border = officer::fp_border(color = "black", width = 1)) |>
  flextable::hline_bottom(part = "header",
                          border = officer::fp_border(color = "black",
                                                      width = 1.5)) |>
  flextable::hline_bottom(part = "body",
                          border = officer::fp_border(color = "black",
                                                      width = 1.5)) |>
  flextable::vline(j = 1, part = "header",
                   border = officer::fp_border(color = "black",
                                               width = 1)) |>
  flextable::vline(j = 2:11, part = "header",
                   border = officer::fp_border(color = "black",
                                               width = 1))

flex_quiro

flex_quiro |>
  flextable::save_as_docx(path = "./consultoria_04.07.2026/tabela_diversidade_quiropteros.docx")
