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

## AbundÂncia total ----

nt <- com |>
  tidyr::pivot_longer(cols = dplyr::everything(),
                      names_to = "especie",
                      values_to = "abundancia") |>
  dplyr::summarise(nt = abundancia |> sum()) |>
  dplyr::pull(nt)

nt
