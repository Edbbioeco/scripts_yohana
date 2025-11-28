# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(vegan)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("Dados.xlsx")

## Visualizando ----

dados

dados |> dplyr::glimpse()

## Tratando ----

dados %<>%
  dplyr::mutate(Especie = Especie |> stringr::str_replace("_", " ")) |>
  tidyr::pivot_longer(cols = dplyr::contains("FT"),
                      values_to = "Abundância",
                      names_to = "Área") |>
  dplyr::summarise(Abundância = Abundância |> max(),
                   .by = c(Especie, Área)) |>
  tidyr::pivot_wider(names_from = Área,
                     values_from = Abundância,
                     values_fill = 0)

# UPGMA ----

## Calculando a similaridade de Bray-Curtis

bray <- 1 - dados |> vegan::vegdist()

bray

## Calculando o UPGMA ----

## Gráfico ----

