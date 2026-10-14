# Pacotes ----

library(magick)

# Remover fundo ----

## Listar imagens ----

imagens <- paste0("^Rhinella ",
                   c("icterica", "crucifer"),
                   collapse = "|") |>
  list.files(pattern = _)

imagens
