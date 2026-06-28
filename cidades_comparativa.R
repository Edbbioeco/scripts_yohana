# Pacotes ----

library(readxl)

library(tidyverse)

library(writexl)

# Dados ----

## Importando ----

cidades <- readxl::read_xlsx("dados copilados.xlsx")

## Visualizando ----

cidades

# Matriz ----

## Criando a matriz ----

cidades_trat <- cidades |> 
  dplyr::rename("Espécie" = Especies) |> 
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      names_to = "Município",
                      values_to = "Presença") |> 
  dplyr::mutate(City = "Comparative") |> 
  dplyr::relocate(Município, .before = Espécie)

cidades_trat

## Exportando ----

cidades_trat |> 
  writexl::write_xlsx("cidades_comparativas.xlsx")
