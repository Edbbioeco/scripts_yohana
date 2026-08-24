# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(writexl)

# Dados ----

## Importar ----

dados <- readxl::read_xlsx("./caminho_dos_dados/nome_do_arquivo.xlsx")

## Visualizar ----

dados

dados |> dplyr::glimpse()
