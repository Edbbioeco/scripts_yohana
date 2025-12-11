# Pacotes ----

library(tidyverse)

library(writexl)

# Dados ----

## importando ----

source("analises_consultoria_11.2025.R")

## Visualizando ----

ls() |>
  mget(envir = globalenv())

# UPGMA ----

## Visualizando ---

ramos

## Exportando ----

ramos |>
  writexl::write_xlsx("valores_similaridade_dendograma.xlsx")

## Exportandop ----

# Curva de rarefação ----

## Visualizando ----

dados_curva

## Exportando ----

dados_curva |>
  writexl::write_xlsx("valores_riqueza_curva.xlsx")

# Diagrama de Whitaker ----

## Visualizando ----

matriz_whit

## Exportando ----

matriz_whit |>
  writexl::write_xlsx("valores_riqueza_diagrama_whitaker.xlsx")

