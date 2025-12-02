# Pacotes ----

library(readxl)

library(tidyverse)

library(writexl)

# Dados ----

## Importando ----

func <- readxl::read_xlsx("Matriz_funcional.xlsx",
                          sheet = 2)

## Visualizando ----

func |> as.data.frame()

# Organizando os traços funcionais ----

## Loop para todos os traços ----

org_func <- function(id){

  valores <- cols[[id]]

  traco <- cols[id] |> names()

  tamanho <- 1 + cols[[id]] |> length()

  trac_org <- func |>
    dplyr::select(1, valores) |>
    tidyr::pivot_longer(cols = 2:tamanho,
                        names_to = traco,
                        values_to = "Presenca") |>
    dplyr::filter(Presenca > 0) |>
    dplyr::select(-Presenca) |>
    dplyr::summarise(dplyr::across(1, ~ stringr::str_c(.x, collapse = ", ")),
                     .by = Especies)

  assign(paste0("traco_organizado_", traco),
         trac_org,
         envir = globalenv())

}

cols <- list(Atividade = c(2:4),
             Habitat = c(8:10),
             `Modo Reprodutivo` = c(15:23),
             `Nível de Toxicidade` = c(5:7),
             `Categoria de Tamanho` = c(12:14),
             `Local de Canto` = c(11, 24:28))

cols

purrr::walk(1:6, org_func)

## Unindo os dados

tracos_funcionais <- ls(pattern = "traco_organizado_") |>
  mget(envir = globalenv()) |>
  purrr::reduce(left_join, by = "Especies")

tracos_funcionais

## Exportando ----

tracos_funcionais |>
  writexl::write_xlsx("tracos_funcionais.xlsx")
