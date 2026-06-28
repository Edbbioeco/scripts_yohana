# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(writexl)

# Dados -----

## Importando ----

uso_esp <- readxl::read_xlsx("Uso do espaço_2026.xlsx")

## Visualizando -----

uso_esp

uso_esp |> dplyr::glimpse()

# Diversidade de habitat ----

## Montando a matriz ----

matriz_uso <- uso_esp |>
  dplyr::summarise(dplyr::across(.cols = dplyr::where(is.numeric),
                                 .fns = ~sum(.)),
                   .by = Espécie)

matriz_uso

## Calculando a diversidade ----

diversidade <- matriz_uso |>
  tibble::column_to_rownames(var = "Espécie") |>
  vegan::renyi(hill = TRUE, scales = 1)

diversidade

## Montando o dataframe dos valores de diversidade ----

diversidade_df <- tibble::tibble(Espécie = diversidade |> names(),
                                 Diversidade = diversidade)

diversidade_df

## Exportando ----

diversidade_df |>
  writexl::write_xlsx("diversidade_uso_esp.xlsx")
