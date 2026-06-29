# Pacotes ----

library(readxl)

library(tidyverse)

library(ggview)

# Dados ----

## Importar ----

uso <- readxl::read_xlsx("Uso do espaço  (2).xlsx")

## Visualizar ----

uso

uso |> dplyr::glimpse()

# Gráfico ----

## Tratar ----

uso_trat <- uso |>
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
                             "Rhinella icterica",
                             "Boana faber",
                             "Phyllomedusa tetraploidea",
                             "Leptodactylus mystacinus",
                             "Odontophrynus asper"))) |>
  as.data.frame()

uso_trat

## Gráfico ----

ggplot(data = uso_trat,
       aes(Habitat, `Número de indivíduos`)) +
  geom_col() +
  facet_wrap(~Espécie) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.text.x = element_text(color = "black", size = 20,
                                   angle = 45, hjust = 1),
        axis.title = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 20,
                                  face = "italic"),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.background = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "uso_habitat_especies.png",
       height = 10, width = 12)
