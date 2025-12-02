# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(vegan)

library(BAT)

# Dados ----

## Composição ----

### Importando ----

com <- readxl::read_xlsx("Diversidade de Shannon ABUNDANCIA.xlsx")

### Visualizando ----

com

com |> dplyr::glimpse()

### Tratando ----

com  %<>%
  dplyr::select(1:5) %<>%
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      values_to = "Abundância",
                      names_to = "Local") %<>%
  tidyr::pivot_wider(names_from = Espécies,
                     values_from = Abundância) %<>%
  tibble::column_to_rownames(var = "Local")

com

## Traços funcionais ----

### Importando ----

trat <- readxl::read_xlsx("tracos_funcionais.xlsx")

### Visualizando ----

trat

trat |> dplyr::glimpse()

### Tratando ----

trat %<>%
  tibble::column_to_rownames(var = "Especies")

trat

# Diversidade funcional -----

## Calculando a diversidade beta funcional total ----

BAT::beta.multi(com,
                tree = gower_dist |> as.matrix(),
                abund = TRUE,
                runs = 100) %>%
  .[1:3, 1]

## Calculando as matrizes de distância ----

beta_func_matriz <- BAT::beta(com,
                              tree = gower_dist |> as.matrix(),
                              abund = TRUE,
                              runs = 100)

beta_func_matriz

# Gráfico ----

## Transformando as matrizes em dataframes ----

matriz_em_df_func <- function(id, indice){

  matriz <- beta_func_matriz[[id]] |>
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
    dplyr::rename("Functional Beta diversity index" = value)

  assign(paste0("df_beta_func_",
                indice |> stringr::str_replace_all(" ", "_")),
         df,
         envir = globalenv())

}

id <- 1:3

indice <- c("Beta Diversity", "Species Replacement", "Specis Gain/Loss")

indice

purrr::walk2(id, indice, matriz_em_df_func)

## Unindo os dados ----

beta_dfs_func <- ls(pattern = "df_beta_func_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::mutate(Índice = Índice |> forcats::fct_relevel(indice))

beta_dfs_func

## Gráfico ----

beta_dfs_func |>
  ggplot(aes(Var1, Var2, fill = `Functional Beta diversity index`)) +
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

ggsave(filename = "dissertacao_div_beta_func_bray.png", height = 10, width = 12)
