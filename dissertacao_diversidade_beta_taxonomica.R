# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(betapart)

library(reshape2)

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

# Diversidade beta ----

## Calculando a diversidade beta de Bray-Curtis ----

beta_bray_total <- comp |>
  tibble::column_to_rownames(var = "Local") |>
  betapart::beta.multi.abund(index.family = "bray")

beta_bray_total

## Calculando as matrizes de distância ----

beta_bray_matriz <- comp |>
  tibble::column_to_rownames(var = "Local") |>
  betapart::beta.pair.abund(index.family = "bray")

beta_bray_matriz

# Gráfico ----

## Transformando as matrizes em dataframes ----

matriz_em_df <- function(id, indice){

  matriz <- beta_bray_matriz[[id]] |>
    as.matrix()

  matriz[upper.tri(matriz)] <- NA

  df <- matriz |>
    reshape2::melt() |>
    tidyr::drop_na() |>
    dplyr::mutate(Índice = indice,
                  igual = dplyr::case_when(Var1 == Var2 ~ TRUE,
                                           .default = FALSE)) |>
    dplyr::filter(igual == FALSE) |>
    dplyr::select(-5) |>
    dplyr::rename("Beta diversity index" = value)

  assign(paste0("df_beta_",
                indice |> stringr::str_replace_all(" ", "_")),
         df,
         envir = globalenv())

}

id <- 1:3

indice <- c("Balanced Variation", "Abundance Gradient", "Bray-Curtis")

indice

purrr::walk2(id, indice, matriz_em_df)

## Unindo os dados ----

beta_dfs <- ls(pattern = "df_beta_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::mutate(Índice = Índice |> forcats::fct_relevel(indice |> rev()))

beta_dfs

## Gráfico ----

beta_dfs |>
  ggplot(aes(Var1, Var2, fill = `Beta diversity index`)) +
  geom_tile(color = "black", linewidth = 1) +
  facet_wrap(~ Índice) +
  coord_equal() +
  labs(x = NULL,
       y = NULL) +
  scale_fill_viridis_c(guide = guide_colorbar(title.position = "top",
                                              title.hjust = 0.5,
                                              barwidth = 30,
                                              barheight = 2,
                                              frame.colour = "black",
                                              frame.linewidth = 1,
                                              ticks.colour = "black",
                                              ticks.linewidth = 1)) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 25),
        axis.title = element_text(color = "black", size = 25),
        legend.title = element_text(color = "black", size = 25),
        legend.text = element_text(color = "black", size = 25),
        legend.position = "bottom",
        strip.text = element_text(color = "black", size = 25),
        strip.background = element_rect(color = "black", fill = "gray",
                                        linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "dissertacao_div_beta_tax_bray.png", height = 10, width = 12)
