# Pacotes ----

library(googledrive)

library(readxl)

library(tidyverse)

library(ggimage)

library(ggview)

# Dados ----

## Importar ----

googledrive::drive_auth("edson.nilton@ufpe.br")

caminho <- Sys.getenv("pasta_artigo_rhinella") |>
  googledrive::drive_get() |>
  googledrive::drive_ls() |>
  dplyr::filter(name |> stringr::str_detect("Uso do")) |>
  dplyr::pull(name) |>
  (\(x) googledrive::drive_download(file = x,
                                    path = paste0("./", x),
                                    overwrite = TRUE))() |>
  dplyr::pull(local_path)

uso <- readxl::read_xlsx(caminho)

## Visualizar ----

uso

uso |> dplyr::glimpse()

# Gráfico ----

## Tratar ----

uso_trat <- uso |>
  dplyr::filter(Espécie |> stringr::str_detect("Rhinella")) |>
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      names_to = "Habitat",
                      values_to = "Presença") |>
  dplyr::summarise(`Número de indivíduos` = Presença |> sum(),
                   .by = c(Espécie, Habitat)) |>
  dplyr::mutate(Espécie = dplyr::if_else(
    Espécie |> stringr::str_detect("americanus"),
    "Odontophrynus asper",
    Espécie) |>
      forcats::fct_relevel(c("Rhinella crucifer",
                             "Rhinella icterica")),
                Habitat = dplyr::case_match(

                  Habitat,
                  "Árvore" ~ "Tree",
                  "Bambu" ~ "Bamboo",
                  "Borda de brejo" ~ "Marsh edge",
                  "Chão" ~ "Clean ground",
                  "Embaixo da serrapilheira" ~ "Under leaflitter",
                  "Embaixo de tronco" ~ "Under fallen tree",
                  "Enterrado" ~ "Buried",
                  "Toca no solo" ~ "Burrowed in the ground"

                  ) |>
      forcats::fct_relevel(c(

        "Buried",
        "Burrowed in the ground",
        "Clean ground",
        "Under leaflitter",
        "Under fallen tree",
        "Tree",
        "Bamboo",
        "Marsh edge"

        ))) |>
  as.data.frame()

uso_trat

## Imagens transparentes ----

imagens_sps <- tibble::tibble(x = 6,
                              y = 7.75,
                              sps = paste0(c("Rhinella icterica",
                                             "Rhinella crucifer"),
                                           ".png"),
                              Espécie = c("Rhinella icterica",
                                          "Rhinella crucifer"))

imagens_sps

## Gráfico ----

ggplot(data = uso_trat,
       aes(`Número de indivíduos`, Habitat)) +
  geom_col() +
  ggimage::geom_image(data = imagens_sps,
                      aes(x, y, image = sps),
                      size = 0.35) +
  facet_wrap(~Espécie) +
  labs(y = "Resting habitat",
       x = "Count of individuals") +
  scale_x_continuous(breaks = seq(0, 9, 1),
                     limits = c(0, 9)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 20,
                                  face = "italic"),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.background = element_rect(color = "black", linewidth = 1))

ggsave(filename = "uso_habitat_especies.png",
       height = 12, width = 16)

"uso_habitat_especies.png" |>
  googledrive::drive_upload(media = _,
                            name = "uso_habitat_especies.png",
                            overwrite = TRUE)
