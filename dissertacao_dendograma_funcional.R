# Pacotes ----

library(readxl)

library(tidyverse)

library(magrittr)

library(FD)

library(ggdendro)

library(ggview)

# Dados ----

## Composição ----

### Importando ----

com <- readxl::read_xlsx("Diversidade de Shannon ABUNDANCIA.xlsx")

### Visualizando ----

com

com |> dplyr::glimpse()

### Tratando ----

com  %<>%
  dplyr::select(1:5) %<>%
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      values_to = "Abundância",
                      names_to = "Local") %<>%
  tidyr::pivot_wider(names_from = Espécies,
                     values_from = Abundância) %<>%
  tibble::column_to_rownames(var = "Local")

com

## Traços funcionais ----

### Importando ----

trat <- readxl::read_xlsx("tracos_funcionais.xlsx")

### Visualizando ----

trat

trat |> dplyr::glimpse()

### Tratando ----

trat %<>%
  tibble::column_to_rownames(var = "Especies")

trat

# Dendograma ----

## Matriz de distância de Gower ----

matriz_gower <- trat |>
  FD::gowdis()

matriz_gower

## Calculando a diversidade funcional beta ----

beta_func_matriz <- BAT::beta(com,
                              tree = matriz_gower |> as.matrix(),
                              abund = TRUE,
                              runs = 100)

beta_func_matriz

## Calculando UPGMA ----

indice_gow <- c("Beta Diversity", "Species Replacement", "Species Gain/Loss")

indice_gow

upgma_multi_func <- function(id){

  upgma_func <- beta_func_matriz[[id]] |>
    hclust(method = "average")

  assign(paste0("upgma_func_", indice_gow[[id]]),
         upgma_func,
         envir = globalenv())

}

id <- 1:3

purrr::walk(id, upgma_multi_func)

## Criando os dados de dendrograma ----

dendo_data_func <- function(dendro, indice_func){

  dendro_data_tax <- dendro |>
    as.dendrogram() |>
    ggdendro::dendro_data()

  assign(paste0("dendro_data_func_", indice_func),
         dendro_data_tax,
         envir = globalenv())

}

dendro <- ls(pattern = "upgma_func_") |>
  mget(envir = globalenv())

dendro

indice_func <- c("Beta Diversity",
                 "Species Gain/Loss",
                 "Species Replacement")

purrr::walk2(dendro, indice_func, dendo_data_func)

## Separando os ramos ----

dendro_func <- ls(pattern = "dendro_data_func_") |>
  mget(envir = globalenv())

dendro_func

ramos_func <- function(id){

  ramos <- dendro_func[[id]]$segments |>
    dplyr::mutate(Indice = indice_func[[id]])

  assign(paste0("ramos_func_", indice_func[[id]]),
         ramos,
         envir = globalenv())

}

purrr::walk(1:3, ramos_func)

## Nomes dos ramos ----

nomes_ramos_func <- function(id){

  ramos <- dendro_func[[id]]$labels |>
    dplyr::mutate(Indice = indice_func[[id]])

  assign(paste0("ramos_nomes_func_", indice_func[[id]]),
         ramos,
         envir = globalenv())

}

purrr::walk(1:3, nomes_ramos_func)

## Ramos e nomes ----

func_ramos <- ls(pattern = "ramos_func_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::mutate(Indice = Indice |> forcats::fct_relevel("Beta Diversity",
                                                        "Species Replacement",
                                                        "Species Gain/Loss"))

func_ramos

func_nomes <- ls(pattern = "ramos_nomes_func_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::mutate(Indice = Indice |> forcats::fct_relevel("Beta Diversity",
                                                        "Species Replacement",
                                                        "Species Gain/Loss"))

func_nomes

## Gráfico ----

ggplot() +
  geom_segment(data = func_ramos, aes(x = x, y = y,
                                      xend = xend, yend = yend),
               linewidth = 2) +
  labs(x = NULL,
       y = "Gower Dissimilarity") +
  scale_x_continuous(breaks = 1:12,
                     labels = func_nomes$label) +
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

ggsave(filename = "dissertacao_upgma_func.png", height = 10, width = 12)
