# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(flextable)

# Dados ----

## Importando ----

comp <- readxl::read_xlsx("Diversidade de Shannon ABUNDANCIA.xlsx")

## Visualizando ----

comp

comp |> dplyr::glimpse()

## Tratando ----

comp  %<>%
  dplyr::select(1:5)

comp

# Índice de Constância ----

## Organizando as espécies ----

comp %<>%
  dplyr::arrange(Espécies |> forcats::fct_relevel("Melanophryniscus tumifrons",
                                                  "Rhinella crucifer",
                                                  "Rhinella icterica",
                                                  "Rhinella ornata",
                                                  "Vitreorana uranoscopa",
                                                  "Ischnocnema henselii",
                                                  "Aplastodiscus perviridis",
                                                  "Boana faber",
                                                  "Boana leptolineata",
                                                  "Boana prasina",
                                                  "Dendropsophus minutus",
                                                  "Dendropsophus nanus",
                                                  "Phyllomedusa tetraploidea",
                                                  "Scinax berthae",
                                                  "Scinax catharinae",
                                                  "Scinax fuscovarius",
                                                  "Scinax perereca",
                                                  "Scinax rizibilis",
                                                  "Scinax squalirostris",
                                                  "Trachycephallus typhonius",
                                                  "Leptodactylus fuscus",
                                                  "Leptodactylus latrans",
                                                  "Leptodactylus mystacinus",
                                                  "Leptodactylus plaumanni",
                                                  "Physalaemus cuvieri",
                                                  "Physalaemus gracilis",
                                                  "Elachistocleis bicolor",
                                                  "Elachistocleis ovalis",
                                                  "Proceratophrys avelinoi",
                                                  "Proceratophrys brauni",
                                                  "Odontophrynus americanus")) %<>%
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      names_to = "Área",
                      values_to = "Abundância")

comp |> as.data.frame()

## Cálculo de Índice de Constância -----

c_i <- comp |>
  dplyr::summarise(CI = sum(Abundância > 0) / n(),
                   .by = Espécies) |>
  dplyr::mutate(Category = dplyr::case_when(dplyr::between(CI, 0, 0.25) ~ "Accidental",
                                            dplyr::between(CI, 0.251, 0.5) ~ "Accessory",
                                            dplyr::between(CI, 0.51, 1) ~ "Constant"),
                CI = paste0(CI * 100, "%")) |>
  dplyr::rename("Species" = Espécies)

c_i |> as.data.frame()

## Adicionando as informações de família ----

c_i_trat <- c_i |>
  tibble::add_row(Species = "Bufonidae",
                  CI = NULL, Category = NULL,
                  .before = 1) |>
  tibble::add_row(Species = "Centrolenidae",
                  CI = NULL, Category = NULL,
                  .before = 6) |>
  tibble::add_row(Species = "Craugastoridae",
                  CI = NULL, Category = NULL,
                  .before = 8) |>
  tibble::add_row(Species = "Hylidae",
                  CI = NULL, Category = NULL,
                  .before = 10) |>
  tibble::add_row(Species = "Leptodactylidae",
                  CI = NULL, Category = NULL,
                  .before = 25) |>
  tibble::add_row(Species = "Microhylidae",
                  CI = NULL, Category = NULL,
                  .before = 32) |>
  tibble::add_row(Species = "Odontophrynidae",
                  CI = NULL, Category = NULL,
                  .before = 35)

c_i_trat

## Tabela flextable ----

c_1_flex <- c_i_trat |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(width = 2.5, j = 1) |>
  flextable::italic(j = 1) |>
  flextable::italic(j = 1, i = c(1, 6, 8, 10, 25, 32, 35), italic = FALSE) |>
  flextable::bold(i = c(1, 6, 8, 10, 25, 32, 35)) |>
  flextable::bg(bg = "white", part = "all")

c_1_flex

## Exportando a tabela ----

### Docx ----

c_1_flex |>
  flextable::save_as_docx(path = "dissertacao_tabela_ci.docx")

### Imagem ----

c_1_flex |>
  flextable::save_as_image(path = "dissertacao_tabela_ci.png")
