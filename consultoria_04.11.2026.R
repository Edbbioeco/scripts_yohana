# Pacotes ----

library(googledrive)

library(tidyverse)

library(readxl)

library(vegan)

library(patchwork)

# Dados ----

## Autenticar Google Drive ----

googledrive::drive_auth("edson.nilton@ufpe.br")

## Immportar ----

dados <- purrr::map(
  "Recitech/CGH Cacatu" |>
    googledrive::drive_get() |>
    googledrive::drive_ls() |>
    dplyr::filter(name %in% c("indices cgh Cacatu - mastofauna- 1 a 2- L.O",
                              "Dados Mastofauna - CGH Cacatu")) |>
    dplyr::pull(id),
  \(arquivo){

    arquivo |>
      googledrive::drive_download(file = _,
                                  path = tempfile(fileext = ".xlsx")) |>
      dplyr::pull(local_path) |>
      readxl::read_xlsx()

    },
  .progress = TRUE) |>
  purrr::set_names(
    "Recitech/CGH Cacatu" |>
      googledrive::drive_get() |>
      googledrive::drive_ls() |>
      dplyr::filter(name %in% c("indices cgh Cacatu - mastofauna- 1 a 2- L.O",
                                "Dados Mastofauna - CGH Cacatu")) |>
      dplyr::pull(name) |>
      stringr::str_remove(".xlsx"))

## Visualizar ----

dados

dados |> dplyr::glimpse()

## Tratar ----

dados <- dados |>
  purrr::pluck(1) |>
  dplyr::select(Campanha, Área, Espécie:Quantidade) |>
  dplyr::mutate(Campanha = Campanha |>
                  stringr::str_remove("2- ") |>
                  stringr::str_to_title()) |>
  dplyr::bind_rows(
    dados |>
      purrr::pluck(2) |>
      dplyr::select(Campanha, Área, Espécie,
                    `Nome comum` = `Nome Vulgar`,
                    "Quantidade" = `Nº Ind.2`) |>
      dplyr::mutate(Campanha = Campanha |>
                      stringr::str_replace(stringr::fixed("/"),
                                           " "),
                    Área = Área |>
                      stringr::str_remove("TR."))
    )

dados

dados |> dplyr::glimpse()

# Curva de rarefação ----

## Calcular a riqueza ----

riq <- dados |>
  dplyr::select(1, 3, 5) |>
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Quantidade,
                     values_fill = 0,
                     values_fn = max) |>
  tibble::column_to_rownames(var = "Campanha") |>
  vegan::specaccum(method = "rarefaction") |>
  purrr::pluck("richness")

riq

## Calcular desvio padrão ----

sd <- dados |>
  dplyr::select(1, 3, 5) |>
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Quantidade,
                     values_fill = 0,
                     values_fn = max) |>
  tibble::column_to_rownames(var = "Campanha") |>
  vegan::specaccum(method = "rarefaction") |>
  purrr::pluck("sd")

sd

## Calcular jackknife 1 ----

jackk <- dados |>
  dplyr::select(1, 3, 5) |>
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Quantidade,
                     values_fill = 0,
                     values_fn = max) |>
  tibble::column_to_rownames(var = "Campanha") |>
  vegan::specpool() |>
  purrr::pluck("jack1")

jackk

## Criar data frame com as informações ----

df_rare <- tibble::tibble(Campanhas = 0:3,
                          Espécies = c(0, riq),
                          sd = c(0, sd[1] |> rep(3)))

df_rare

## Gráfico ----

ggplot() +
  geom_line(data = df_rare,
            aes(x = Campanhas, y = Espécies + sd,
                linetype = "Desvio Padrão"),
            color = "black",
            linewidth = 2) +
  geom_line(data = df_rare,
            aes(x = Campanhas, y = Espécies,
                linetype = "Curva de Acumulação de Espécies"),
            color = "black",
            linewidth = 2) +
  geom_line(data = df_rare,
            aes(x = Campanhas, y = Espécies - sd,
                linetype = "Desvio Padrão"),
            color = "black",
            linewidth = 2) +
  labs(y = "Espécies",
       title = "Curva do coletor",
       linetype = NULL) +
  scale_x_continuous(breaks = seq(0, 3, 1),
                     limits = c(0, 3),
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
        plot.margin = margin(10, 14, 8, 8))

ggsave(filename = "gráfico_rarefacao_04.11.2026.png",
       height = 10,
       width = 20)

"gráfico_rarefacao_04.11.2026.png" |>
  googledrive::drive_upload(path = "Recitech/CGH Cacatu",
                            name = "gráfico_rarefacao_04.11.2026.png",
                            overwrite = TRUE)
