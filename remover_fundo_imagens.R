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
      magick::image_fill(color = "transparent", point = "+1+1", fuzz = 25)

    }
  ) |>
  setNames(paste0("Rhinella ",
                  c("crucifer", "icterica")))

imagens_trat |>
  purrr::map(plot)

## Exportar imagens ----

purrr::imap(
  imagens_trat,
  \(imagem, sps){

    imagem |> magick::image_write(paste0(sps, ".png"), format = "png")

    },
  .progress = TRUE)
