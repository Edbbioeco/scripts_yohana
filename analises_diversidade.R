# Pacotes ----

library(readxl)

library(tidyverse)

library(vegan)

library(betapart)

library(ggdendro)

library(ggtext)

library(flextable)

# Dados ----

## Base de dados e levantamento ----

### Importando ----

bases <- readxl::read_xlsx("matriz_registros.xlsx")

levantamento <- readxl::read_xlsx("dados_levantamento.xlsx")

### Visualizando ----

bases

levantamento

### Tratando ----

dados_non_comparative <- dplyr::bind_rows(bases,
                                          levantamento) |> 
  dplyr::mutate(City = "Non compartive")

dados_non_comparative 

### Criando a matriz ----

matriz_1 <- dados_non_comparative |> 
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Presença,
                     values_fn = min,
                     values_fill = 0)

matriz_1 

## Cidades comparativas ----

### Importando ----

cidades_comp <- readxl::read_xlsx("cidades_comparativas.xlsx")

### Visualizando ----

cidades_comp

### Tratando ----

dados_div <- dplyr::bind_rows(dados_non_comparative,
                              cidades_comp) |> 
  dplyr::mutate(Espécie = dplyr::case_when(Espécie |> 
                                             stringr::str_detect("brongersmianus") ~ "Amerotyphlops brongersmianus",
                                           Espécie |> 
                                             stringr::str_detect("vautieri") ~ "Urostrophus vautieri",
                                           Espécie == "Erythrolamprus poecilogyrus" ~ "Erythrolamprus poecylogyrus",
                                           .default = Espécie))

dados_div

### Matriz ----

matriz_2 <- dados_div |> 
  dplyr::select(-City) |> 
  tidyr::pivot_wider(names_from = Espécie,
                     values_from = Presença,
                     values_fn = max,
                     values_fill = 0) |> 
  dplyr::left_join(dados_div |>  
                     dplyr::distinct(Município, .keep_all = TRUE) |> 
                     dplyr::select(1, 4),
                   by = "Município")

matriz_2

matriz_2 |> dplyr::glimpse()

# Setando tema ----

theme_set(theme_bw() +
            theme(axis.text = element_text(size = 15, color = "black"),
                  axis.title = element_text(size = 15, color = "black"),
                  strip.text = element_text(size = 15, color = "black"),
                  strip.background = element_rect(linewidth = 1, color = "black"),
                  legend.text = element_text(size = 15, color = "black"),
                  legend.title = element_text(size = 15, color = "black"),
                  panel.border = element_rect(linewidth = 1, color = "black"),
                  legend.position = "bottom"))

# Estimador de riqueza ----

## Calculando ----

jack1 <- matriz_2 |> 
  dplyr::select(dplyr::where(is.numeric)) |> 
  vegan::poolaccum(permutations = 1000) |> 
  summary(display = c("S", "jack1"))

jack1

## Gráfico ----

jack1_df <- jack1$S |> 
  as.data.frame() |> 
  dplyr::select(1:2, 5) |> 
  dplyr::mutate(Type = "Observed") |> 
  dplyr::rename("Richness" = S) |> 
  dplyr::bind_rows(jack1$jack1 |> 
                     as.data.frame() |> 
                     dplyr::select(1:2, 5) |> 
                     dplyr::mutate(Type = "Estimated") |> 
                     dplyr::rename("Richness" = `Jackknife 1`)) |> 
  dplyr::mutate(Richness = Richness |> round())

jack1_df

jack1_df |> 
  ggplot(aes(N, Richness, , color = Type, fill = Type)) +
  geom_ribbon(aes(x = N,
                  ymin = Richness - Std.Dev,
                  ymax = Richness + Std.Dev),
              alpha = 0.3,
              color = "transparent") +
  geom_line(linewidth = 1) +
  geom_point(size = 5, shape = 21, color = "black") +
  scale_fill_manual(values = c("royalblue", "orange")) +
  scale_color_manual(values = c("royalblue4", "orange4")) +
  scale_x_continuous(breaks = seq(0, 11, 1)) +
  guides(fill = guide_legend(title.position = "top",
                             title.hjust = 0.5),
         color = guide_legend(title.position = "top",
                             title.hjust = 0.5))

ggsave(filename = "curva.png", height = 10, width = 12)

# UPGMA ----

## Calculando as matrizes de distância ----

jacc <- matriz_2 |> 
  tibble::column_to_rownames(var = "Município") |> 
  dplyr::select(dplyr::where(is.numeric)) |> 
  betapart::beta.pair(index.family = "jaccard")

jacc

## Calculando os clusters ----

calcular_cluster <- function(x, id){
  
  cluster <- jacc[[x]] |> 
    hclust(method = "average") |> 
    as.dendrogram() |> 
    ggdendro::dendro_data()
  
  cluster$segments$dendro_id <- id
  
  cluster$labels$dendro_id <- id
  
  cluster$labels <- cluster$labels |> 
    dplyr::left_join(matriz_2 |> 
                       dplyr::select(1, 112) |> 
                       dplyr::rename("label" = Município),
                     by = "label") |> 
    dplyr::rename("grupo" = City)
  
  assign(paste0("cluster_", id),
         cluster,
         envir = globalenv())
  
}

purrr::walk2(1:3, 
             c("Turnover", "Nestedness", "Total Jaccard"),
             calcular_cluster)

lista_clusters <- ls(pattern = "cluster_") |> 
  mget(envir = globalenv())

lista_clusters

## Gráfico ----

segmentos <- dplyr::bind_rows(lapply(lista_clusters, 
                                     function(x) x$segments)) |> 
  dplyr::mutate(dendro_id = dendro_id |> 
                  as.factor() |> 
                  forcats::fct_relevel(c("Total Jaccard",
                                         "Turnover",
                                         "Nestedness")))

segmentos

labels <- dplyr::bind_rows(lapply(lista_clusters, 
                                  function(x) x$labels)) |> 
  dplyr::mutate(dendro_id = dendro_id |> 
                  as.factor() |> 
                  forcats::fct_relevel(c("Total Jaccard",
                                         "Turnover",
                                         "Nestedness")))

labels

ggplot() +
  geom_segment(data = segmentos,
               aes(x = x, y = y, yend = yend, xend = xend),
               linewidth = 1) +
  geom_label(data = labels,
             aes(x = x, y = y, label = label, fill = grupo), 
             angle = 270, 
             hjust = -0.05,
             fontface = "bold",
             size =  5) +
  labs(y = "Jaccard Dissimilarity Index",
       fill = NULL) +
  facet_wrap(~ dendro_id) +
  scale_fill_manual(values = c("royalblue", "orange")) +
  scale_y_continuous(limits = c(-0.25, 1), breaks = seq(0, 1, 0.1)) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", size = 15),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid = element_line(color = "gray50", linewidth = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())

ggsave(filename = "dendogrmas.png", height = 10, width = 12)

# PERMANOVA ----

## Calculando as permanovas ----

calcular_permanova <- function(x, y){
  
  message(paste0("modelo para: ", x))
  
  permanova <- vegan::adonis2(jacc[[y]] ~ City, 
                              data = matriz_2,
                              permutations = 1000)
  
  print(permanova)
  
  assign(paste0("permanova_", x),
         permanova,
         envir = globalenv())
  
}

purrr::walk2(c("Turnover", "Nestedness", "Total Jaccard"),
             1:3, 
             calcular_permanova)

ls(pattern = "permanova_") |> 
  mget(envir = globalenv())

## NMDS ----

### Calculando ----

calcular_nmds <- function(x, y){
  
  message(paste0("NMDS para: ", y))
  
  nmds <- jacc[[x]] |> 
    vegan::metaMDS(distance = "jaccard")
  
  nmds
  
  nmds$stress
  
  scores <- nmds|> 
    vegan::scores() |> 
    as.data.frame() |> 
    tibble::rownames_to_column() |> 
    dplyr::rename("Município" = rowname) |> 
    dplyr::left_join(matriz_2 |> 
                       dplyr::select(1, 112),
                     by = "Município") |> 
    mutate(componente = y)
  
  scores
  
  assign(paste0("nmds_", y),
         scores,
         envir = globalenv())
  
}

purrr::walk2(1:3, 
             c("Turnover", "Nestedness", "Total Jaccard"),
             calcular_nmds)

## Gráfico ----

gg_nmds <- ls(pattern = "nmds_") |> 
  mget(envir = globalenv()) |> 
  dplyr::bind_rows() |> 
  dplyr::mutate(componente = componente |> 
                  forcats::fct_relevel(c("Total Jaccard",
                                         "Turnover",
                                         "Nestedness")))

gg_nmds

estatísticas <- tibble::tibble(NMDS1 = -0.05,
                               NMDS2 = 0.4,
                               estaisticas = c("F<sub>(1, 9)</sub> = 2.26, p = 0.03, R<sup>2</sup> = 0.2",
                                               "F<sub>(1, 9)</sub> = 0.25, p = 0.88, R<sup>2</sup> = 0.02",
                                               "F<sub>(1, 9)</sub> = 5.54, p = 0.046, R<sup>2</sup> = 0.38"),
                               componente = c("Total Jaccard",
                                              "Turnover",
                                              "Nestedness"),
                               City = NA) |> 
  dplyr::mutate(componente = componente |> 
                  forcats::fct_relevel(c("Total Jaccard",
                                         "Turnover",
                                         "Nestedness")))

estatísticas

centroides <- gg_nmds |> 
  dplyr::group_by(City, componente) |> 
  dplyr::summarise(c_x = NMDS1 |> mean(),
                   c_y = NMDS2 |> mean())

centroides

gg_nmds <- gg_nmds |> 
  dplyr::left_join(centroides,
                   by = c("City", "componente"))

gg_nmds

gg_nmds |> 
  ggplot(aes(NMDS1, NMDS2, label = Município, fill = City, color = City)) +
  geom_segment(aes(x = c_x, y = c_y,
                   xend = NMDS1, yend = NMDS2),
               linewidth = 1,
               linetype = "dashed",
               show.legend = FALSE) +
  geom_point(aes(c_x, c_y),
             shape = 24, 
             color = "black",
             size = 3) +
  geom_point(shape = 21, 
             color = "black",
             size = 3) +
  scale_fill_manual(values = c("royalblue", "orange")) +
  scale_color_manual(values = c("royalblue4", "orange4")) +
  labs(fill = NULL,
       color = NULL) +
  ggnewscale::new_scale_colour() +
  ggrepel::geom_text_repel(aes(NMDS1, NMDS2, label = Município,  color = City),
                           fontface = "bold",
                           segment.size = 1.2,
                           bg.color = "black",
                           show.legend = FALSE) +
  scale_color_manual(values = c("royalblue", "orange")) +
  ggtext::geom_richtext(data = estatísticas,
                        aes(NMDS1, NMDS2, label = estaisticas),
                        color = "black",
                        fill = "transparent",
                        label.colour = "transparent",
                        size = 5.75) +
  facet_wrap(~ componente) +
  scale_x_continuous(limits = c(-0.55, 0.5)) +
  labs(fill = NULL,
       color = NULL)

ggsave(filename = "nmds.png", height = 10, width = 12)

#  Comparação entre comunidades ----

## Calculando ----

jacc_tot <- matriz |> 
  dplyr::select(dplyr::where(is.numeric)) |> 
  betapart::beta.multi(index.family = "jaccard")

jacc_tot

calcular_comparacao <- function(x, y){
  
  matriz_dist <- jacc[[x]] |> 
    as.matrix()
  
  matriz_dist[upper.tri(matriz_dist)] <- NA
  
  div_jac <- matriz_dist |> 
    reshape2::melt() |> 
    tidyr::drop_na() |> 
    dplyr::mutate(igual = dplyr::case_when(Var1 == Var2 ~ "sim",
                                           .default = "Não")) |> 
    dplyr::filter(igual == "Não") |> 
    dplyr::select(!igual) |> 
    dplyr::mutate(componente = y)
  
  assign(paste0("df_matriz_", y),
         div_jac,
         envir = globalenv())
  
}

purrr::walk2(1:3, 
             c("Turnover = 0.77", "Nestedness = 0.14", "Total Jaccard = 0.91"),
             calcular_comparacao)

## Gráfico ----

df_comparacoes <- ls(pattern = "df_matriz_") |> 
  mget(envir = globalenv()) |> 
  dplyr::bind_rows() |> 
  dplyr::mutate(componente = componente |> 
                  forcats::fct_relevel(c("Total Jaccard = 0.91",
                                         "Turnover = 0.77",
                                         "Nestedness = 0.14")),
                value = value |> round(2)) 

df_comparacoes


df_comparacoes |>
  ggplot(aes(Var1, Var2, fill = value, label = value)) +
  geom_tile(color = "black",
            linewidth = 0.5) +
  facet_wrap(~componente) +
  geom_text(color = "black",
            size = 3.5,
            fontface = "bold") +
  coord_equal() +
  scale_fill_viridis_c(na.value = "transparent",
                       limits = c(0, 1)) +
  labs(x = NULL,
       y = NULL,
       fill = "Jaccard Dissimilarity Index") +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5,
                                barwidth = 35,
                                barheight = 1,
                                frame.colour = "black",
                                frame.linewidth = 1,
                                ticks.colour = "black",
                                ticks.linewidth = 1)) +
  theme(axis.text.x = element_text(angle = 270,
                                    hjust = 0))

ggsave(filename = "comparacoes_comunidades.png", height = 10, width = 12)

## Médias ----

### Calculando médias ----

comparative_x_compartive <- df_comparacoes |> 
  dplyr::filter(Var1 %in% c("Curitiba",
                            "Ponta Grossa",
                            "Tibagi",
                            "Londrina") &
                  Var2 %in% c("Curitiba",
                              "Ponta Grossa",
                              "Tibagi",
                              "Londrina")) |> 
  dplyr::summarise(Média = value |> mean() |> round(2),
                   `Desvio Padrão` = value |> sd() |> round(2),
                   .by = componente) |> 
  tidyr::unite(col = "Mean ± standard deviation",
               sep = " ± ",
               c(Média, `Desvio Padrão`)) |> 
  dplyr::mutate(Comparation = "Comparative x Compartive",
                componente = componente |> 
                  stringr::str_remove_all(" = |\\d|\\."),
                N = df_comparacoes |> 
                  dplyr::filter(Var1 %in% c("Curitiba",
                                            "Ponta Grossa",
                                            "Tibagi",
                                            "Londrina") &
                                  Var2 %in% c("Curitiba",
                                              "Ponta Grossa",
                                              "Tibagi",
                                              "Londrina")) |> 
                  nrow()) |> 
  dplyr::rename("Component" = componente) |> 
  dplyr::arrange(Component |> match(c("Total Jaccard",
                                      "Turnover",
                                      "Nestedness")))

comparative_x_compartive

comparative_x_non <- df_comparacoes |> 
  dplyr::filter(Var1 %in% c("Curitiba",
                            "Ponta Grossa",
                            "Tibagi",
                            "Londrina") &
                  !(Var2 %in% c("Curitiba",
                              "Ponta Grossa",
                              "Tibagi",
                              "Londrina"))) |> 
  dplyr::summarise(Média = value |> mean() |> round(2),
                   `Desvio Padrão` = value |> sd() |> round(2),
                   .by = componente) |> 
  tidyr::unite(col = "Mean ± standard deviation",
               sep = " ± ",
               c(Média, `Desvio Padrão`)) |> 
  dplyr::mutate(Comparation = "Comparative x Non Compartive",
                componente = componente |> 
                  stringr::str_remove_all(" = |\\d|\\."),
                N = df_comparacoes |> 
                  dplyr::filter(Var1 %in% c("Curitiba",
                                            "Ponta Grossa",
                                            "Tibagi",
                                            "Londrina") &
                                  !(Var2 %in% c("Curitiba",
                                                "Ponta Grossa",
                                                "Tibagi",
                                                "Londrina"))) |> 
                  nrow()) |> 
  dplyr::rename("Component" = componente) |> 
  dplyr::arrange(Component |> match(c("Total Jaccard",
                                      "Turnover",
                                      "Nestedness")))


comparative_x_non

non_x_non <- df_comparacoes |> 
  dplyr::filter(!Var1 %in% c("Curitiba",
                            "Ponta Grossa",
                            "Tibagi",
                            "Londrina") &
                  !(Var2 %in% c("Curitiba",
                                "Ponta Grossa",
                                "Tibagi",
                                "Londrina"))) |> 
  dplyr::summarise(Média = value |> mean() |> round(2),
                   `Desvio Padrão` = value |> sd() |> round(2),
                   .by = componente) |> 
  tidyr::unite(col = "Mean ± standard deviation",
               sep = " ± ",
               c(Média, `Desvio Padrão`)) |> 
  dplyr::mutate(Comparation = "Non Comparative x Non Compartive",
                componente = componente |> 
                  stringr::str_remove_all(" = |\\d|\\."),
                N = df_comparacoes |> 
                  dplyr::filter(!(Var1 %in% c("Curitiba",
                                            "Ponta Grossa",
                                            "Tibagi",
                                            "Londrina")) &
                                  !(Var2 %in% c("Curitiba",
                                                "Ponta Grossa",
                                                "Tibagi",
                                                "Londrina"))) |> 
                  nrow()) |> 
  dplyr::rename("Component" = componente) |> 
  dplyr::arrange(Component |> match(c("Total Jaccard",
                                      "Turnover",
                                      "Nestedness")))


non_x_non

### Unindo ----

comparacoes <- ls(pattern = "_x_") |> 
  mget(envir = globalenv()) |> 
  dplyr::bind_rows()

comparacoes

### Flextable ----

tabela <- comparacoes |> 
  flextable::flextable() |> 
  flextable::width(width = 1.5) |> 
  flextable::align(align = "center", part = "all")

tabela

tabela |> 
  flextable::save_as_docx(path = "tabela_medias.docx")

qf(df1 = 1, df2 = 9, p = 0.05, lower.tail = FALSE)
