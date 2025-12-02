# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(betapart)

library(ggdendro)

library(ggview)

# Dados ----

## Importando ----

comp <- readxl::read_xlsx("Diversidade de Shannon ABUNDANCIA.xlsx")

## Visualizando ----

comp

comp |> dplyr::glimpse()

## Tratando ----

comp  %<>%
  dplyr::select(1:5) %<>%
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      values_to = "Abundância",
                      names_to = "Local") %<>%
  tidyr::pivot_wider(names_from = Espécies,
                     values_from = Abundância) %<>%
  tibble::column_to_rownames(var = "Local")

comp

# Dendograma ----

## Matriz de distância de Bray-Curtis ----

matriz_bray <- comp |>
  betapart::beta.pair.abund()

matriz_bray

## Calculando UPGMA ----

upgma_multi_bray <- function(id, indice){

  upgma_tax <- matriz_bray[[id]] |>
    hclust(method = "average")

  assign(paste0("upgma_tax_", indice),
         upgma_tax,
         envir = globalenv())

}

id <- 1:3

indice <- c("Balanced Variation", "Abundance Gradient", "Bray-Curtis")

indice

purrr::walk2(id, indice, upgma_multi_bray)

## Criando os dados de dendrograma ----

dendo_data_bray <- function(dendro, indice_bray){

  dendro_data_tax <- dendro |>
    as.dendrogram() |>
    ggdendro::dendro_data()

  assign(paste0("dendro_data_tax_", indice_bray),
         dendro_data_tax,
         envir = globalenv())

}

dendro <- ls(pattern = "upgma_tax_") |>
  mget(envir = globalenv())

dendro

indice_bray <- c("Abundance Gradient",
                 "Balanced Variation",
                 "Bray-Curtis")

purrr::walk2(dendro, indice_bray, dendo_data_bray)

## Separando os ramos ----

dendro_taxa <- ls(pattern = "dendro_data_tax_") |>
  mget(envir = globalenv())

dendro_taxa

ramos_bray <- function(id){

  ramos <- dendro_taxa[[id]]$segments |>
    dplyr::mutate(Indice = indice_bray[[id]])

  assign(paste0("ramos_taxa_", indice_bray[[id]]),
         ramos,
         envir = globalenv())

}

purrr::walk(1:3, ramos_bray)

## Nomes dos ramos ----

nomes_ramos_bray <- function(id){

  ramos <- dendro_taxa[[id]]$labels |>
    dplyr::mutate(Indice = indice_bray[[id]])

  assign(paste0("ramos_nomes_taxa_", indice_bray[[id]]),
         ramos,
         envir = globalenv())

}

purrr::walk(1:3, nomes_ramos_bray)

## Ramos e nomes ----

taxa_ramos <- ls(pattern = "ramos_taxa_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::mutate(Indice = Indice |> forcats::fct_relevel("Bray-Curtis",
                                                        "Abundance Gradient",
                                                        "Balanced Variation"))

taxa_ramos

taxa_nomes <- ls(pattern = "ramos_nomes_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::mutate(Indice = Indice |> forcats::fct_relevel("Bray-Curtis",
                                                        "Abundance Gradient",
                                                        "Balanced Variation"))

taxa_nomes

## Gráfico ----

ggplot() +
  geom_segment(data = taxa_ramos, aes(x = x, y = y,
                                 xend = xend, yend = yend),
               linewidth = 2) +
  labs(x = NULL,
       y = "Similaridade de Bray-Curtis") +
  scale_x_continuous(breaks = 1:12,
                     labels = taxa_nomes$label) +
  facet_wrap(~Indice) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5,
                                   color = "black", size = 25),
        axis.text = element_text(color = "black", size = 25),
        axis.title = element_text(color = "black", size = 25),
        panel.grid.major.y = element_line(linewidth = 1, color = "gray80"),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(color = "black", size = 25),
        strip.background = element_rect(color = "black", fill = "gray")) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "dissertacao_upgma_tax.png", height = 10, width = 12)
