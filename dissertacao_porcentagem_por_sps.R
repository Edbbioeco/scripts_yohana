# Pacotes ----

library(readxl)

library(tidyverse)

# Dados ----

## Importar ----

com <- readxl::read_xlsx("Diversidade de Shannon ABUNDANCIA.xlsx")

## Visualizar ----

com

com |> dplyr::glimpse()
