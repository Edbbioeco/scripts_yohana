# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(betapart)

# Dados ----

## Importando ----

comp <- readxl::read_xlsx("Diversidade de Shannon ABUNDANCIA.xlsx")

## Visualizando ----

comp

## Tratando ----

comp  %<>%
  dplyr::select(1:5) %<>%
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      values_to = "Abundância",
                      names_to = "Local") %<>%
  tidyr::pivot_wider(names_from = Espécies,
                     values_from = Abundância)

comp

# Diversidade beta ----
