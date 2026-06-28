# Pacotes ----

library(usethis)

# Iniciando ----

usethis::use_git()

# Configure o usuario e email ----

usethis::use_git_config(user.name = "Edbbioeco",
                        user.email = "edsonbbiologia@gmail.com")

# Definindo projeto ----

usethis::proj_get()

# Settando o repositório ----

usethis::use_git_remote(name = "origin",
                        url = "https://github.com/Edbbioeco/scripts_yohana.git",
                        overwrite = TRUE)

# Definindo o branch para o main ----

usethis::git_default_branch_configure()

# Renomear o branch do master para main ----

usethis::git_default_branch_rename(from = "master", to = "main")
