# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(ggview)

library(ggdendro)

# Dados ----

## Importar ----

dados <- purrr::map(list.files(path = "./consultoria_04.07.2026/",
                               full.names = TRUE),
                    readxl::read_xlsx) |>
  setNames(list.files(path = "./consultoria_04.07.2026/") |>
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
                                                      `Nome científico`,
                                                      Grupo)) |>
                             dplyr::mutate(Campanha = Campanha |> as.character())
                         ),
                         .progress = TRUE) |>
  setNames(list.files(path = "./consultoria_04.07.2026/") |>
             stringr::str_remove_all("dados_|.xlsx"))

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
