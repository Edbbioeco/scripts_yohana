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

# Tabela ----

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
                                                  "Odontophrynus americanus"))

comp

## Adicionando as informações de família ----

comp_trat <- comp |>
  tibble::add_row(Espécies = "Bufonidae",
                  PR = NULL,  RIO = NULL, CAP = NULL, NBA = NULL,
                  .before = 1) |>
  tibble::add_row(Espécies = "Centrolenidae",
                  FPR = NULL,  RIO = NULL, CAP = NULL, NBA = NULL,
                  .before = 6) |>
  tibble::add_row(Espécies = "Craugastoridae",
                  FPR = NULL,  RIO = NULL, CAP = NULL, NBA = NULL,
                  .before = 8) |>
  tibble::add_row(Espécies = "Hylidae",
                  FPR = NULL,  RIO = NULL, CAP = NULL, NBA = NULL,
                  .before = 10) |>
  tibble::add_row(Espécies = "Leptodactylidae",
                  FPR = NULL,  RIO = NULL, CAP = NULL, NBA = NULL,
                  .before = 25) |>
  tibble::add_row(Espécies = "Microhylidae",
                  FPR = NULL,  RIO = NULL, CAP = NULL, NBA = NULL,
                  .before = 32) |>
  tibble::add_row(Espécies = "Odontophrynidae",
                  FPR = NULL,  RIO = NULL, CAP = NULL, NBA = NULL,
                  .before = 35) |>
  dplyr::rename("Species" = Espécies)

comp_trat

## Tabela flextable ----

comp_flex <- comp_trat |>
  flextable::flextable() |>
  flextable::align(align = "center", part = "all") |>
  flextable::width(width = 2.5, j = 1) |>
  flextable::italic(j = 1) |>
  flextable::italic(j = 1, i = c(1, 6, 8, 10, 25, 32, 35), italic = FALSE) |>
  flextable::bold(i = c(1, 6, 8, 10, 25, 32, 35)) |>
  flextable::bg(bg = "white", part = "all")

comp_flex

## Exportando a tabela ----

### Docx ----

comp_flex |>
  flextable::save_as_docx(path = "dissertacao_tabela_especies.docx")

### Imagem ----

comp_flex |>
  flextable::save_as_image(path = "dissertacao_tabela_especies.png")

# Espécies mais abundantes ----

comp |>
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      names_to = "Local",
                      values_to = "Abundância") |>
  dplyr::summarise(Abundância = Abundância |> sum(),
                   .by = Espécies) |>
  dplyr::arrange(Abundância |> dplyr::desc())
