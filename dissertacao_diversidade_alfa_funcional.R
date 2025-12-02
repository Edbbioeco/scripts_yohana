# Pacotes ----

library(readxl)

library(tidyverse)

library(FD)

library(picante)

# Dados ----

## Composição ----

### Importando ----

com <- readxl::read_xlsx("Diversidade de Shannon ABUNDANCIA.xlsx")

### Visualizando ----

com

com |> dplyr::glimpse()

### Tratando ----

comp  %<>%
  dplyr::select(1:5) %<>%
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      values_to = "Abundância",
                      names_to = "Local") %<>%
  tidyr::pivot_wider(names_from = Espécies,
                     values_from = Abundância)

comp

## Traços funcionais ----

### Importando ----

trat <- readxl::read_xlsx("tracos_funcionais.xlsx")

### Visualizando ----

trat

trat |> dplyr::glimpse()

# Diversidade funcional -----

## Distânmcia de Gower ----

gower_dist <- trat |>
  dplyr::select(-1) |>
  as.data.frame() |>
  FD::gowdis()

gower_dist

## Diversidade funcional alfa ----

### Modelos nulos ----
