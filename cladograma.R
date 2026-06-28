# Pacotes ----

library(ape)

library(tidyverse)

library(tidytree)

library(ggtree)

library(ggtreeExtra)

# Dados ----

## Importando ----

arvore <- ape::read.tree("consenso.phy")

## visualizando ----

arvore

arvore |>
  ggtree::ggtree() +
  ggtree::geom_tiplab(size = 4,
                      color = "black",
                      fontface = "bold.italic",
                      offset = 14.5)

# Tratando a filogenia ----

## Tratando os nomes ----

arvore$tip.label <- arvore$tip.label |>
  stringr::str_replace("_",
                       " ")

## Dataframe com a informações das famílias ----

dados_familias <- data.frame(label = arvore$tip.label) |>
  dplyr::mutate(Family = c(rep("Bufonidae", 2),
                            "Odontophrynidae",
                            "Leptodactylidae",
                            "Hylidae",
                            "Phyllomedusidae"))

dados_familias

## Adicionando a informação das famílias ----

arvore_trat <- tidytree::as.treedata(arvore) |>
  dplyr::left_join(dados_familias, by = "label")

arvore_trat

# Gladograma ----

ggtree::ggtree(arvore_trat, layout = "circular") +
  ggtree::geom_tiplab(size = 4,
                      color = "black",
                      fontface = "bold.italic",
                      offset = 14.5) +
  ggtreeExtra::geom_fruit(geom = geom_tile,
                          mapping = aes(fill = Family),
                          color = "black",
                          width = 10,
                          offset = 0.04) +
  scale_fill_manual(values = c("brown",
                               "forestgreen",
                               "gold",
                               "orange",
                               "limegreen")) +
  xlim(-100, 175) +
  ggtree::theme_tree() +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "cladograma_circular.png", height = 10, width = 12)
