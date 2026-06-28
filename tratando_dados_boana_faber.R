# Pacotes ----

library(readxl)

library(tidyverse)

library(writexl)

# Dados ----

## Importando ----

importar_boanas <- function(sheet){

  dados_boana <- readxl::read_xlsx("Dados Boana faber.xlsx",
                                   sheet = sheet) |>
    dplyr::mutate(Data = Data |> lubridate::as_datetime(),
                  Umidade = Umidade |> as.character() |> readr::parse_number())

  assign(paste0("dados_boana_", sheet),
         dados_boana,
         envir = globalenv())

}

purrr::map(1:16, importar_boanas)

## Visualizando ----

ls(pattern = "dados_boana_") |>
  mget(envir = globalenv()) |>
  purrr::map(dplyr::glimpse)

# Tratando ----

dados_boana <- ls(pattern = "dados_boana_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::mutate(`Altura do chão (cm)` = `Altura do chão (cm)` / 100) |>
  dplyr::arrange(ID |> forcats::fct_relevel(c(paste0("B", 1:9),
                                              paste0("B", 10:16))))

dados_boana |>
  dplyr::filter(ID == "B16") |>
  dplyr::select(dplyr::contains("Altura")) |> as.data.frame()

# Exportando ----

dados_boana |>readr::write_csv("Dados Boana faber.csv")
