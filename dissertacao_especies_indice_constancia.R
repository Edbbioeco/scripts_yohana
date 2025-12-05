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
                                                  "Phyllomedusa tetraploidea",
                                                  "Elachistocleis bicolor",
                                                  "Elachistocleis ovalis",
                                                  "Proceratophrys avelinoi",
                                                  "Proceratophrys brauni",
                                                  "Odontophrynus americanus")) %<>%
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      names_to = "Área",
                      values_to = "Abundância")

comp |> as.data.frame()

## Abundância total ----

abund_total <- comp |>
  dplyr::pull(Abunância) |>
  sum()

abund_total

## Cálculo de Índice de Constância -----

c_i <- comp |>
  dplyr::summarise(CI = sum(Abundância > 0) / n(),
                   .by = Espécies) |>
  dplyr::mutate(Category = dplyr::case_when(dplyr::between(CI, 0, 0.25) ~ "Accidental",
                                            dplyr::between(CI, 0.251, 0.5) ~ "Accessory",
                                            dplyr::between(CI, 0.51, 1) ~ "Constant")) |>
  dplyr::rename("Species" = Espécies)

c_i |> as.data.frame()

## Tabela flextable ----

c_1_flex <- c_i |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(width = 2.5, j = 1) |>
  flextable::italic(j = 1) |>
  flextable::bg(bg = "white", part = "all")

c_1_flex

## Exportando a tabela ----

### Docx ----

c_1_flex |>
  flextable::save_as_docx(path = "dissertacao_tabela_ci.docx")

### Imagem ----

c_1_flex |>
  flextable::save_as_image(path = "dissertacao_tabela_ci.png")
