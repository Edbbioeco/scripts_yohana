# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("Dados.xlsx")

## Visualizando ----

dados

dados |> dplyr::glimpse()

## Matriz de composição ----

matriz <- dados |>
  dplyr::mutate(Especie = Especie |> stringr::str_replace("_", " ")) |>
  dplyr::summarise(Abundancia = Abundancia |> max(),
                   .by = c(Especie, Area)) |>
  tidyr::pivot_wider(names_from = Especie,
                     values_from = Abundância,
                     values_fill = 0) |>
  tibble::column_to_rownames(var = "Area")

matriz

## Dados temporais ----

temp <- dados |>
  dplyr::mutate(`Horário e data` = paste0(Data, " : ", Hora) |>
                  lubridate::as_datetime())

temp

# UPGMA ----

## Calculando a similaridade de Bray-Curtis

bray <- 1 - matriz |> vegan::vegdist()

bray

## Calculando o UPGMA ----

upgma <- bray |>
  hclust(method = "average")

upgma

## Gráfico ----

# Curva de rarefação ----

# Diversidade taxonômica ----

## Riqueza e diversidade ----

## Equitabilidade de Hill ----

# Análises temporais ----
