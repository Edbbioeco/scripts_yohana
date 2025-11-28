# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(vegan)

# Dados ----

## Importando ----

dados <- readxl::read_xlsx("Dados.xlsx")

## Visualizando ----

dados

dados |> dplyr::glimpse()

# UPGMA ----

## Calculando a similaridade de Bray-Curtis

bray <- 1 - dados |> vegan::vegdist()

bray

## Gráfico ----

