# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(FD)

library(ade4)

library(picante)

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

## Distânmcia de Gower ----

gower_dist <- trat |>
  as.data.frame() |>
  FD::gowdis()

gower_dist

## Lista ktab ----

ktab_dist <- trat |>
  list() |>
  ade4::ktab.list.df() |>
  ade4::dist.ktab(type = "O")

ktab_dist

## Modelos nulos ----

alfa_nulos <- picante::ses.mpd(samp = com,
                               dis = gower_dist,
                               null.model = "taxa.labels",
                               runs = 999)

alfa_nulos

## Modelos simples ----

div_func_alfa <- FD::dbFD(ktab_dist,
                          com)

div_func_alfa

## Gráfico ----

### Tratando os dados ----

div_func_alfa_trat <- div_func_alfa |>
  dplyr::bind_rows() |>
  tibble::rownames_to_column() |>
  dplyr::select(1, 4, 6:7) |>
  dplyr::rename("Local" = 1,
                "Funcional Richness" = 2,
                "Funcional Eveness" = 3,
                "Funcional Diversity" = 4) |>
  dplyr::mutate(Local = com |> rownames()) |>
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      names_to = "indice",
                      values_to = "Funcional Diversity") |>
  dplyr::mutate(indice = indice |> forcats::fct_relevel(c("Funcional Eveness",
                                                          "Funcional Richness",
                                                          "Funcional Diversity")))

div_func_alfa_trat

### Gráfico ----

div_func_alfa_trat |>
  ggplot(aes(Local, `Funcional Diversity`)) +
  geom_col(color = "black", width = 0.5) +
  facet_wrap(~indice, scales = "free_y",
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

ggsave(filename = "dissertacao_div_alfa_func.png", height = 10, width = 12)

# Diversidade funcional beta -----

## Calculando os valores de diversidade funcional beta ----

