# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(vegan)

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
                     values_from = Abundância)

comp

# Diversidade alfa ----

## q = 0 e q = 1 ----

hill_div <- comp |>
  tibble::column_to_rownames(var = "Local") |>
  vegan::renyi(scales = c(0, 1), hill = TRUE) |>
  tibble::rownames_to_column() |>
  dplyr::rename("Local" = 1,
                "q = 0" = 2,
                "q = 1" = 3)

hill_div

## Equitabilidade de Hill ----

hill_eq <- comp |>
  tibble::column_to_rownames(var = "Local") |>
  vegan::renyi(scales = c(1, 2), hill = TRUE) |>
  tibble::rownames_to_column() |>
  dplyr::rename("Local" = 1,
                "q = 1" = 2,
                "q = 2" = 3) |>
  dplyr::summarise(`Hill evenness` = `q = 2` / `q = 1`,
                   .by = Local)

hill_eq

## Unindo os dados ----

hill_data <- hill_div |>
  dplyr:::left_join(hill_eq,
                    by = "Local") |>
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      names_to = "Índice",
                      values_to = "Hill values") |>
  dplyr::mutate(Índice = Índice |> forcats::fct_relevel(c(paste0("q = ", 0:1),
                                                          "Hill evenness")))

hill_data

## Gráfico ----

hill_data |>
  ggplot(aes(Local, `Hill values`)) +
  geom_col(color = "black", width = 0.5) +
  facet_wrap(~Índice, scales = "free_y",
             ncol = 2) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 25),
        axis.title = element_text(color = "black", size = 25),
        legend.text = element_text(color = "black", size = 25),
        legend.position = "bottom",
        strip.text = element_text(color = "black", size = 25),
        strip.background = element_rect(color = "black", fill = "gray",
                                        linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "dissertacao_div_alfa_tax_hill.png", height = 10, width = 12)
