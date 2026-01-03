# Pacote ----

library(gert)

# Selecionando o arquivo ----

gert::git_add(".")

# Commitando ----

gert::git_commit("Script de mínimo poígono convexo")

# Pushando ----

gert::git_push(remote = "origin")

# Pullando ----

usethis::git_pul()
