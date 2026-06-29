# Pacotes ----

library(readxl)

library(tidyverse)

library(ggview)

# Dados ----

## Importar ----

uso <- readxl::read_xlsx("Uso do espaço  (2).xlsx")

## Visualizar ----

uso

uso |> dplyr::glimpse()

# Gráfico ----

## Tratar ----

uso_trat <- uso |>
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      names_to = "Habitat",
                      values_to = "Presença") |>
  dplyr::summarise(`Número de indivíduos` = Presença |> sum(),
                   .by = c(Espécie, Habitat))

uso_trat
