# Pacotes ----

library(tidyverse)

library(FactoMineR)

library(magrittr)

library(performance)

library(ggtext)

library(ggview)

# Dados ----

## Discos adesivos de Boana faber ----

### Importando ----

bf <- readxl::read_xlsx("Dados discos adesivos.xlsx")

### Visualizando ----

bf

bf |> dplyr::glimpse()

## Discos adesivos de Phyllomedusa tetraploidea ----

### Importando ----

pt <- readxl::read_xlsx("Dados discos adesivos.xlsx",
                        sheet = 2)

### Visualizando ----

pt

pt |> dplyr::glimpse()

# Deslocamento vertical ----

## Boana faber -----

### Importando ----

deslo_boana <- readxl::read_xlsx("Dados Boana faber.xlsx")

### Visualizando ----

deslo_boana |>
  dplyr::mutate(id = dplyr::row_number()) |>
  dplyr::filter(`Altura (m)` > 10) |>
  dplyr::select(`Altura (m)`, id)

deslo_boana |> dplyr::glimpse()

### Deslocamento vertical máximo ----

deslo_boana_valores <- deslo_boana |>
  dplyr::filter(!ID |> is.na() & !`Altura (m)` |> is.na()) |>
  dplyr::summarise(Altura = `Altura (m)` |> max(),
                   .by = ID) |>
  dplyr::mutate(Peso = bf |> dplyr::pull(`Peso (g)`))

deslo_boana_valores

## Phyllomedusa tetraploidea ----

### Importando ----

deslo_phyllo <- readxl::read_xlsx("Dados Phyllomedusa tetraploidea.xlsx")

### Visualizando ----

deslo_phyllo

deslo_phyllo |> dplyr::glimpse()

# Deslocamento vertical máximo -----

deslo_phyllo_valores <- deslo_phyllo |>
  dplyr::summarise(Altura = `Altura (m)` |> max(),
                   .by = ID) |>
  dplyr::mutate(Peso = pt |> dplyr::pull(`Peso (g)`))

deslo_phyllo_valores

# PCA ----

## Boana faber ----

### Calculando a PCA ----

pca_boana <- bf |>
  dplyr::select(dplyr::contains("AREA")) |>
  FactoMineR::PCA()

### PC1 ----

pc1_boana <- pca_boana$ind$coord[, 1]

pc1_boana

### Unindo os dados ----

deslo_boana_valores %<>%
  dplyr::mutate(PC1 = pc1_boana)

deslo_boana_valores

## Phyllomedusa tetraploidea ----

### Calculando a PCA ----

pca_phyllo <- pt |>
  dplyr::select(dplyr::contains("AREA")) |>
  FactoMineR::PCA()

### PC1 ----

pc1_phyllo <- pca_phyllo$ind$coord[, 1]

pc1_phyllo

### Unindo os dados ----

deslo_phyllo_valores %<>%
  dplyr::mutate(PC1 = pc1_phyllo)

deslo_phyllo_valores

# Média da área dos discos ----

## Boana faber ----

### Calculando a média ----

media_boana <- bf |>
  dplyr::mutate(ID = paste0("BF", 1:16)) |>
  tidyr::pivot_longer(cols = dplyr::contains("AREA"),
                      values_to = "Área dos discos",
                      names_to = "Tipo de área") |>
  dplyr::summarise(Área = `Área dos discos` |> mean(),
                   .by = ID)

media_boana

### Unindo os dados ----

deslo_boana_valores %<>%
  dplyr::left_join(media_boana,
                   by = "ID")

deslo_boana_valores

### Calculando a média ponderada pelo peso ----

deslo_boana_valores %<>%
  dplyr::mutate(`Área ponderada` = Área / Peso)

deslo_boana_valores

## Phyllomedusa teraploidea ----

### Calculando a média ----

media_phyllo <- pt |>
  dplyr::mutate(ID = paste0("PT", 1:10)) |>
  tidyr::pivot_longer(cols = dplyr::contains("AREA"),
                      values_to = "Área dos discos",
                      names_to = "Tipo de área") |>
  dplyr::summarise(Área = `Área dos discos` |> mean(),
                   .by = ID)

media_phyllo

### Unindo os dados ----

deslo_phyllo_valores %<>%
  dplyr::left_join(media_phyllo,
                   by = "ID")

deslo_phyllo_valores

### Calculando a média ponderada pelo peso ----

deslo_phyllo_valores %<>%
  dplyr::mutate(`Área ponderada` = Área / Peso)

deslo_phyllo_valores

# Modelos lineares ----

## Boana faber ----

### PCA ----

#### Criando o modelo -----

modelo_boana_pca <- lm(Altura ~ PC1, data = deslo_boana_valores)

#### Pressupostos do modelo ----

modelo_boana_pca |> performance::check_model(check = c("qq",
                                                       "normality",
                                                       "homogeneity"))

modelo_boana_pca |> performance::check_heteroscedasticity()

modelo_boana_pca |> performance::check_normality()

#### Estatísticas do modelo ----

modelo_boana_pca |> summary()

#### Dataframe das estatísticas do modelo ----

modelo_boana_pca_summary <- modelo_boana_pca |> summary()

modelo_boana_pca_summary

modelo_boana_pca_estatisticas <- modelo_boana_pca_summary$coefficients |>
  as.data.frame() |>
  dplyr::slice(2) |>
  dplyr::rename("β1" = 1,
                "SE" = 2,
                "t" = 3,
                "p" = 4) |>
  dplyr::mutate(dplyr::across(dplyr::everything(),
                              ~.x |> round(2)),
                Estatistica = paste0("β1 ± SE = ",
                                     β1,
                                     " ± ",
                                     SE,
                                     ", t = ",
                                     t,
                                     ", p = ",
                                     p),
                Altura = 7,
                PC1 = -2) |>
  dplyr::select(5:7)

modelo_boana_pca_estatisticas

modelo_boana_pca_p__global <- pf(modelo_boana_pca_summary$fstatistic[1],
                                 modelo_boana_pca_summary$fstatistic[2],
                                 modelo_boana_pca_summary$fstatistic[3],
                                 lower.tail = FALSE) |>
  round(2)

modelo_boana_pca_p__global

modelo_boana_pca_f_estaticas <- paste0("F<sub>(",
                                       modelo_boana_pca_summary$fstatistic[2],
                                       ", ",
                                       modelo_boana_pca_summary$fstatistic[3],
                                       ")</sub> = ",
                                       modelo_boana_pca_summary$fstatistic[1] |>
                                         round(2),
                                       ", p = ",
                                       modelo_boana_pca_p__global,
                                       ", R² = ",
                                       modelo_boana_pca_summary$adj.r.squared |>
                                         round(2))

modelo_boana_pca_f_estaticas

#### Gráfico -----

deslo_boana_valores |>
  ggplot(aes(PC1, Altura)) +
  geom_point(size = 5) +
  ggtext::geom_richtext(data = modelo_boana_pca_estatisticas,
                        aes(PC1, Altura, label = Estatistica),
                        size = 7.5,
                        label.colour = NA) +
  labs(title = modelo_boana_pca_f_estaticas) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        plot.title = ggtext::element_markdown(color = "black", size = 25)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "modelo_linear_disco_boana_faber_pca.png",
       height = 10, width = 12)

### Média ----

#### Criando o modelo -----

modelo_boana_media <- lm(Altura ~ `Área ponderada`, data = deslo_boana_valores)

#### Pressupostos do modelo ----

modelo_boana_media |> performance::check_model(check = c("qq",
                                                       "normality",
                                                       "homogeneity"))

modelo_boana_media |> performance::check_heteroscedasticity()

modelo_boana_media |> performance::check_normality()

#### Estatísticas do modelo ----

modelo_boana_media |> summary()

#### Dataframe das estatísticas do modelo ----

modelo_boana_media_summary <- modelo_boana_media |> summary()

modelo_boana_media_summary

modelo_boana_media_estatisticas <- modelo_boana_media_summary$coefficients |>
  as.data.frame() |>
  dplyr::slice(2) |>
  dplyr::rename("β1" = 1,
                "SE" = 2,
                "t" = 3,
                "p" = 4) |>
  dplyr::mutate(β1 = β1 |> round(4),
                dplyr::across(2:4,
                              ~.x |> round(2)),
                Estatistica = paste0("β1 ± SE = ",
                                     β1,
                                     " ± ",
                                     SE,
                                     ", t = ",
                                     t,
                                     ", p = ",
                                     p),
                Altura = 7,
                `Área ponderada` = 200) |>
  dplyr::select(5:7)

modelo_boana_media_estatisticas

modelo_boana_media_p__global <- pf(modelo_boana_media_summary$fstatistic[1],
                                   modelo_boana_media_summary$fstatistic[2],
                                   modelo_boana_media_summary$fstatistic[3],
                                   lower.tail = FALSE) |>
  round(2)

modelo_boana_media_p__global

modelo_boana_media_f_estaticas <- paste0("F<sub>(",
                                         modelo_boana_media_summary$fstatistic[2],
                                       ", ",
                                       modelo_boana_media_summary$fstatistic[3],
                                       ")</sub> = ",
                                       modelo_boana_media_summary$fstatistic[1] |>
                                         round(3),
                                       ", p = ",
                                       modelo_boana_media_p__global,
                                       ", R² = ",
                                       modelo_boana_media_summary$adj.r.squared |>
                                         round(2))

modelo_boana_media_f_estaticas

#### Gráfico -----

deslo_boana_valores |>
  ggplot(aes(`Área ponderada`, Altura)) +
  geom_point(size = 5) +
  ggtext::geom_richtext(data = modelo_boana_media_estatisticas,
                        aes(`Área ponderada`, Altura, label = Estatistica),
                        size = 7.5,
                        label.colour = NA) +
  labs(title = modelo_boana_media_f_estaticas) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        plot.title = ggtext::element_markdown(color = "black", size = 25)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "modelo_linear_disco_boana_faber_media.png",
       height = 10, width = 12)

## Phyllomedusa tetraploidea ----

### PCA ----

#### Criando o modelo -----

modelo_phyllo_pca <- lm(Altura ~ PC1, data = deslo_phyllo_valores)

#### Pressupostos do modelo ----

modelo_phyllo_pca |> performance::check_model(check = c("qq",
                                                        "normality",
                                                        "homogeneity"))

modelo_phyllo_pca |> performance::check_heteroscedasticity()

modelo_phyllo_pca |> performance::check_normality()

#### Estatísticas do modelo ----

modelo_phyllo_pca |> summary()

#### Dataframe das estatísticas do modelo ----

modelo_phyllo_pca_summary <- modelo_phyllo_pca |> summary()

modelo_phyllo_pca_summary

modelo_phyllo_pca_estatisticas <- modelo_phyllo_pca_summary$coefficients |>
  as.data.frame() |>
  dplyr::slice(2) |>
  dplyr::rename("β1" = 1,
                "SE" = 2,
                "t" = 3,
                "p" = 4) |>
  dplyr::mutate(dplyr::across(dplyr::everything(),
                              ~.x |> round(2)),
                Estatistica = paste0("β1 ± SE = ",
                                     β1,
                                     " ± ",
                                     SE,
                                     ", t = ",
                                     t,
                                     ", p = ",
                                     p),
                Altura = 75,
                PC1 = 3) |>
  dplyr::select(5:7)

modelo_phyllo_pca_estatisticas

modelo_phyllo_pca_p__global <- pf(modelo_phyllo_pca_summary$fstatistic[1],
                                  modelo_phyllo_pca_summary$fstatistic[2],
                                  modelo_phyllo_pca_summary$fstatistic[3],
                                  lower.tail = FALSE) |>
  round(2)

modelo_phyllo_pca_p__global

modelo_phyllo_pca_f_estaticas <- paste0("F<sub>(",
                                        modelo_phyllo_pca_summary$fstatistic[2],
                                        ", ",
                                        modelo_phyllo_pca_summary$fstatistic[3],
                                        ")</sub> = ",
                                        modelo_phyllo_pca_summary$fstatistic[1] |>
                                          round(2),
                                        ", p = ",
                                        modelo_phyllo_pca_p__global,
                                        ", R² = ",
                                        modelo_phyllo_pca_summary$adj.r.squared |>
                                         round(2))

modelo_phyllo_pca_f_estaticas

#### Gráfico -----

deslo_phyllo_valores |>
  ggplot(aes(PC1, Altura)) +
  geom_point(size = 5) +
  ggtext::geom_richtext(data = modelo_phyllo_pca_estatisticas,
                        aes(PC1, Altura, label = Estatistica),
                        size = 7.5,
                        label.colour = NA) +
  labs(title = modelo_phyllo_pca_f_estaticas) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        plot.title = ggtext::element_markdown(color = "black", size = 25)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "modelo_linear_disco_phyllomedusa_tetraploidea_pca.png",
       height = 10, width = 12)

### Média ----

#### Criando o modelo -----

modelo_phyllo_media <- lm(Altura ~ `Área ponderada`, data = deslo_phyllo_valores)

#### Pressupostos do modelo ----

modelo_phyllo_media |> performance::check_model(check = c("qq",
                                                          "normality",
                                                          "homogeneity"))

modelo_phyllo_media |> performance::check_heteroscedasticity()

modelo_phyllo_media |> performance::check_normality()

#### Estatísticas do modelo ----

modelo_phyllo_media |> summary()

#### Dataframe das estatísticas do modelo ----

modelo_phyllo_media_summary <- modelo_phyllo_media |> summary()

modelo_phyllo_media_summary

modelo_phyllo_media_estatisticas <- modelo_phyllo_media_summary$coefficients |>
  as.data.frame() |>
  dplyr::slice(2) |>
  dplyr::rename("β1" = 1,
                "SE" = 2,
                "t" = 3,
                "p" = 4) |>
  dplyr::mutate(β1 = β1 |> round(4),
                dplyr::across(2:4,
                              ~.x |> round(2)),
                Estatistica = paste0("β1 ± SE = ",
                                     β1,
                                     " ± ",
                                     SE,
                                     ", t = ",
                                     t,
                                     ", p = ",
                                     p),
                Altura = 75,
                `Área ponderada` = 0.23) |>
  dplyr::select(5:7)

modelo_phyllo_media_estatisticas

modelo_phyllo_media_p__global <- pf(modelo_phyllo_media_summary$fstatistic[1],
                                    modelo_phyllo_media_summary$fstatistic[2],
                                    modelo_phyllo_media_summary$fstatistic[3],
                                    lower.tail = FALSE) |>
  round(2)

modelo_phyllo_media_p__global

modelo_phyllo_media_f_estaticas <- paste0("F<sub>(",
                                          modelo_phyllo_media_summary$fstatistic[2],
                                         ", ",
                                          modelo_phyllo_media_summary$fstatistic[3],
                                          ")</sub> = ",
                                          modelo_phyllo_media_summary$fstatistic[1] |>
                                            round(3),
                                          ", p = ",
                                          modelo_phyllo_media_p__global,
                                          ", R² = ",
                                          modelo_phyllo_media_summary$adj.r.squared |>
                                           round(2))

modelo_phyllo_media_f_estaticas

#### Gráfico -----

deslo_phyllo_valores |>
  ggplot(aes(`Área ponderada`, Altura)) +
  geom_point(size = 5) +
  ggtext::geom_richtext(data = modelo_phyllo_media_estatisticas,
                        aes(`Área ponderada`, Altura, label = Estatistica),
                        size = 7.5,
                        fill = NA,
                        label.colour = NA) +
  labs(title = modelo_phyllo_media_f_estaticas) +
  theme_classic() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        plot.title = ggtext::element_markdown(color = "black", size = 25)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "modelo_linear_disco_phyllomedusa_teraploidea_media.png",
       height = 10, width = 12)
