# Pacotes ----

library(magick)

# Remover fundo ----

## Listar imagens ----

imagens <- paste0("^Rhinella ",
                   c("icterica", "crucifer"),
                   collapse = "|") |>
  list.files(pattern = _)

imagens

## Remover fundo ----

imagens_trat <- purrr::map(
  imagens,
  \(imagem){

    magick::image_read(imagem) |>
      magick::image_fill(color = "transparent", point = "+1+1", fuzz = 15)

    }
  )

imagens_trat |>
  purrr::map(plot)
