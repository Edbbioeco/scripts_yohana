# Pacotes ----

library(ape)

library(ggtree)

library(ggview)

library(readxl)

library(tidyverse)

library(phytools)

library(patchwork)

# Dados ----

## Filogenia ----

### Importando ----

arvore <- ape::read.tree("consenso.phy")

### Visualizando ----

arvore

ggtree::ggtree(arvore, layout = "circular") +
  ggtree::geom_tiplab(size = 4,
                      color = "black",
                      fontface = "bold.italic",
                      offset = 14.5) +
  xlim(-100, 300) +
  ggtree::theme_tree() +
  ggview::canvas(height = 10, width = 12)

## Diversidade -----

## Importando ----

diversidade <- readxl::read_xlsx("diversidade_uso_esp.xlsx")

## Visualizando ----

diversidade

diversidade |> dplyr::glimpse()

## Tratando ----

diversidade <- diversidade |>
  dplyr::mutate(Espécie = Espécie |> forcats::fct_relevel(arvore$tip.label |>
                                                            rev() |>
                                                            stringr::str_replace("_",
                                                                                 " "))) |>
  dplyr::arrange(Espécie = Espécie |> forcats::fct_relevel(arvore$tip.label |>
                                                             rev() |>
                                                             stringr::str_replace("_",
                                                                                  " ")))

diversidade

# Sinal filogenético ----

## Calculando ----

sinal <- phytools::phylosig(tree = arvore,
                            x = diversidade$Diversidade,
                            method = "K")

sinal

## Gráfico ----

### Cladograma ----

plot_arvore <- ggtree::ggtree(arvore, size = 1) +
  ggtree::theme_tree() +
  ggview::canvas(height = 10, width = 12)

plot_arvore

### Variação da tortuosidade ----

plot_tortuosidade <- diversidade |>
  dplyr::mutate(k = paste0("k = ",
                           sinal |> round(2))) |>
  ggplot(aes(Diversidade, Espécie,
             fill = Diversidade)) +
  geom_col(color = "black",
           width = 0.1) +
  scale_y_discrete(position = "right") +
  #scale_x_continuous(limits = c(0, 1)) +
  scale_fill_viridis_c() +
  labs(y = NULL,
       x = "Hill's Diversity (q = 1)") +
  facet_wrap(~k, nrow = 1, scales = "free_x") +
  theme_minimal() +
  theme(axis.text = element_text(size = 17, color = "black"),
        axis.text.y = element_text(size = 17, color = "black",
                                   hjust = 0,
                                   face = "bold.italic"),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 17, color = "black"),
        legend.position = "none",
        axis.title = element_text(size = 17, color = "black"),
        axis.line.x = element_line(color = "black", linewidth = 1),
        plot.title = element_text(size = 20, color = "black",
                                  hjust = 0.5),
        strip.text = element_text(size = 17, color = "black"),
        strip.background = element_rect(color = "black", fill = "gray")) +
  ggview::canvas(height = 10, width = 12)

plot_tortuosidade

## Sinalização finogenética ----

(plot_arvore + plot_tortuosidade) +
  ggview::canvas(height = 10, width = 14)

ggsave(filename = "sinalizacao_filogenetica_uso_espaco.png",
       height = 10, width = 14)
