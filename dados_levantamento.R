# Pacotes ----

library(tidyverse)

library(writexl)

# Dados ----

## Importando ----

dados_lev <- readr::read_csv2("dados.csv")

## Visualizando ----

dados_lev

dados_lev |> dplyr::glimpse()

# Matriz ----

## Criando a matriz ----

dados_lev_trat <- dados_lev |> 
  dplyr::select(1:8)

dados_lev_trat

colnames(dados_lev_trat) <- c("Espécie",
                              "Guarapuava",
                              "Campina do Simão",
                              "Candói",
                              "Cantagalo",
                              "Goioxim",
                              "Pinhão",
                              "Turvo")

dados_lev_trat

dados_lev_trat_matriz <- dados_lev_trat |> 
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      names_to = "Município",
                      values_to = "Presença") |> 
  dplyr::relocate(Município, .before = Espécie)

dados_lev_trat_matriz

## Exportando ----

dados_lev_trat_matriz |> 
  writexl::write_xlsx("dados_levantamento.xlsx")
