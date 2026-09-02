# Pacotes ----

library(readxl)

library(tidyverse)

# Dados ----

## Importar ----

com <- readxl::read_xlsx("Diversidade de Shannon ABUNDANCIA.xlsx")

## Visualizar ----

com

com |> dplyr::glimpse()

# Estatística descritiva ----

## Abundância total ----

nt <- com |>
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = "especie",
                      values_to = "abundancia") |>
  dplyr::summarise(nt = abundancia |> sum()) |>
  dplyr::pull(nt)

nt

## % por espécie ----

com |>
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = "especie",
                      values_to = "abundancia") |>
  dplyr::summarise(abundancia = abundancia |> sum(),
                   .by = especie) |>
  dplyr::summarise(abundancia = ((abundancia / nt) * 100) |> round(2),
                   .by = especie) |>
  as.data.frame()
