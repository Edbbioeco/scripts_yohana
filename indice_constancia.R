# Pacotes ----

library(readxl)

library(tidyverse)

# Dados ----

## Importando ----

comp <- readxl::read_xlsx("Diversidade de Shannon ABUNDANCIA.xlsx")

## Visualizando ----

comp

comp |> dplyr::glimpse()

# Indice de constancia ----

comp |>
  dplyr::select(1:5) |>
  tidyr::pivot_longer(cols = 2:5,
                      names_to = "Local",
                      values_to = "Abundancia") |>
  as.data.frame() |>
  dplyr::filter(Abundancia > 0) |>
  dplyr::mutate(Porcentagem = (dplyr::n_distinct(Local) / 4 * 100),
                .by = Espécies) |>
  dplyr::mutate(`Indice de coinstancia` = dplyr::case_when(

    Porcentagem |> dplyr::between(0.1, 25.0) ~ "Acidental",
    Porcentagem |> dplyr::between(25.1, 50.0) ~ "Acessória",
    Porcentagem |> dplyr::between(50.1, 1000) ~ "Constante"

                ),
                .by = c(Local, Espécies)) |>
  as.data.frame() |>
  dplyr::distinct(Espécies, .keep_all = TRUE) |>
  dplyr::summarise(Contagem = dplyr::n(),
                   .by = `Indice de coinstancia`) |>
  dplyr::arrange(Contagem |> dplyr::desc())
