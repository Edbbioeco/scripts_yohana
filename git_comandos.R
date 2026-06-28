# Pacote ----

library(gert)

library(tidyverse)

# Selecionando o arquivo ----

gert::git_status() |>
  as.data.frame() |>
  dplyr::filter(file |> stringr::str_detect(".R$"))

# Adicionando os arquivos ----

gert::git_add(list.files(pattern = ".csv$|.xlsx$")) |>
  as.data.frame()

# Commitando ----

gert::git_commit("Tabelas")

# Pushando ----

gert::git_push(remote = "origin")

# Pullando ----

gert::git_pull(remote = "origin")

# Resetando ----

gert::git_reset_mixed()

gert::git_reset_soft("HEAD~1")
