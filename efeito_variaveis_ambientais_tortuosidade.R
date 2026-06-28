# Pacotes ----

library(readxl)

library(tidyverse)

library(parzer)

library(geosphere)

library(sf)

library(janitor)

library(adehabitatHR)

library(reshape2)

library(ggview)

library(lmerTest)

library(performance)

library(glmmTMB)

library(DHARMa)

library(ggeffects)

library(ggtext)

# Dados ----

## Dados de movimentação ----

### Importando ----

importar_mov <- function(especies){

  dados <- readxl::read_xlsx(paste0("Dados ", especies, ".xlsx")) |>
    dplyr::mutate(Espécie = especies)

  assign(paste0("dados_mov_", especies |> stringr::word(2)),
         dados,
         envir = globalenv())

}

especies <- c("Boana faber",
              "Phyllomedusa tetraploidea",
              "Rhinella crucifer",
              "Rhinella icterica",
              "Odontophrynus asper",
              "Leptodactylus mystacinus")

purrr::map(especies, importar_mov)

### Visualizando ----

ls(pattern = "dados_mov_") |>
  mget(envir = globalenv())

### Unindo os dados ----

sps_mov_dados <- ls(pattern = "dados_mov_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

sps_mov_dados

sps_mov_dados |> dplyr::glimpse()

## Dados ambientais -----

### Importando -----

amb <- readxl::read_xlsx("DADOS AMBIENTAIS COM AS 6 ESPÉCIES INDIVIDUAIS.xlsx")

### Visualizando -----

amb

amb |> dplyr::glimpse()

### Tratando ----

amb_trat <- amb |>
  dplyr::summarise(dplyr::across(.cols = dplyr::where(is.numeric),
                                 .fns = ~mean(.)),
                   .by = INDIVIDUO) |>
  dplyr::rename("ID" = INDIVIDUO) |>
  dplyr::mutate(ID = ID |> stringr::str_replace("B", "BF")) |>
  janitor::clean_names()

amb_trat

amb_trat |> dplyr::glimpse()

# Tratando as coordenadas ----

## Convertendo as coordenadas ----

sps_mov_dados <- sps_mov_dados |>
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

sps_mov_dados

## Completando as coordenadas ----

convertendo_coords <- function(x){

  if(sps_mov_dados$Longitude[x] |> is.na() | sps_mov_dados$Latitude[x] |> is.na()){

    dest <- geosphere::destPoint(c(sps_mov_dados$Longitude[x-1],
                                   sps_mov_dados$Latitude[x-1]),
                                 sps_mov_dados$`Angulo de virada`[x-1],
                                 sps_mov_dados$`Distância (m)`[x-1])

    sps_mov_dados$Longitude[x] <<- dest[1]

    sps_mov_dados$Latitude[x] <<- dest[2]

  }

}

purrr::map(1:nrow(sps_mov_dados), convertendo_coords)

sps_mov_dados |> as.data.frame()

# Tortuosidade ----

## Shapefile do trajeto total ----

trajeto <- sps_mov_dados |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674) |>
  dplyr::group_by(ID) |>
  dplyr::summarise(do_union = FALSE) |>
  sf::st_cast("LINESTRING") |>
  dplyr::left_join(sps_mov_dados |>
                     dplyr::select(ID, Espécie) |>
                     dplyr::distinct(ID, .keep_all = TRUE),
                   by = "ID")

trajeto

ggplot() +
  geom_sf(data = trajeto, aes(color = Espécie))

## Shapefile do trajeto mínimo ----

trajeto_minimo <- sps_mov_dados |>
  dplyr::group_by(ID) |>
  dplyr::slice(c(1, dplyr::n())) |>
  dplyr::ungroup() |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674) |>
  dplyr::group_by(ID) |>
  dplyr::summarise(do_union = FALSE) |>
  sf::st_cast("LINESTRING") |>
  dplyr::left_join(sps_mov_dados |>
                     dplyr::select(ID, Espécie) |>
                     dplyr::distinct(ID, .keep_all = TRUE),
                   by = "ID")

trajeto_minimo

ggplot() +
  geom_sf(data = trajeto_minimo, aes(color = Espécie))

## Índice de tortuosidade ----

comprimento_trajeto <- trajeto |>
  sf::st_transform(crs = 32725) |>
  sf::st_length()

comprimento_trajeto

comprimento_trajeto_minimo <- trajeto_minimo |>
  sf::st_transform(crs = 32725) |>
  sf::st_length()

comprimento_trajeto_minimo

tortuosidade <- 1 - as.numeric(comprimento_trajeto_minimo / comprimento_trajeto)

tortuosidade

## Adicionar coluna de tortuosidade ----

amb_trat <- amb_trat |>
  dplyr::mutate(Tortuosidade = tortuosidade) |>
  dplyr::mutate(Tortuosidade = dplyr::case_when(Tortuosidade == 1 ~ 0.9999999,
                                                Tortuosidade == 0 ~ 0.0000001,
                                                .default = Tortuosidade)) |>
  dplyr::left_join(sps_mov_dados |>
                     dplyr::select(1, 8) |>
                     dplyr::rename("id" = 1),
                   by = "id")

amb_trat

amb_trat |> dplyr::glimpse()

# Multicolinearidade ----

## Calculando ----

multicor <- amb_trat |>
  dplyr::select(2:8) |>
  cor(method = "spearman")

multicor

## Gráfico ----

multicor_matriz <- multicor |> as.matrix()

multicor_matriz[upper.tri(multicor_matriz)] <- NA

multicor_matriz |>
  reshape2::melt() |>
  dplyr::filter(! value |> is.na()) |>
  dplyr::mutate(`Spearman's correlation index` = value |> round(2),
                Igual = dplyr::if_else(Var1 == Var2,
                                       "Sim",
                                       "Não")) |>
  dplyr::filter(Igual == "Não") |>
  dplyr::select(1:2, 4) |>
  ggplot(aes(Var1, Var2, fill = `Spearman's correlation index`,
             label = `Spearman's correlation index`)) +
  geom_tile(color = "black", linewidth = 1) +
  coord_equal() +
  geom_text(size = 6) +
  labs(x = NULL,
       y = NULL) +
  scale_fill_gradientn(colours = c(viridis::viridis(n = 10) |> rev(),
                                   viridis::viridis(n = 10)),
                       breaks = seq(-1, 1, 0.25),
                       limits = c(-1, 1),
                       guide = guide_colorbar(title.hjust = 0.5,
                                              frame.colour = "black",
                                              ticks.colour = "black",
                                              ticks.linewidth = 0.75,
                                              barheight = 20)) +
  theme_minimal() +
  theme(axis.text = element_text(size = 17, color = "black"),
        axis.text.x = element_text(size = 17, color = "black",
                                   angle = 90, hjust = 1),
        legend.text = element_text(size = 15, color = "black"),
        legend.title = element_text(size = 17, color = "black"),
        panel.grid = element_line(color = "gray50", linetype = "dashed")) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "multicolinearidade_temperatura_umidade_cap1.png",
       height = 10, width = 12)

# Modelos lineares ----

## Modelo linear misto ----

### Criando o modelo ----

lmm_tortuosidade <- lmerTest::lmer(Tortuosidade ~ temperatura_maxima +
                                     umidade_relativa_maxima+
                                     precipitacao +
                                     (1 | Espécie),
                                   data = amb_trat)

### Pressupostos do modelo ----

lmm_tortuosidade |> performance::check_normality()

lmm_tortuosidade |> performance::check_heteroscedasticity()

lmm_tortuosidade |> performance::check_model(check = c("vif",
                                                       "qq",
                                                       "normality",
                                                       "homogeneity"))

### Avaliação do modelo ----

lmm_tortuosidade |> summary()

## Modelo Linear Generalizado Misto -----

### Criando o modelo ----

glmm_tortuosidade <- glmmTMB::glmmTMB(Tortuosidade ~ temperatura_maxima +
                                        umidade_relativa_maxima+
                                        precipitacao +
                                        (1 | Espécie),
                                      data = amb_trat,
                                      family = glmmTMB::beta_family())

### Pressupostos do modelo ----

glmm_tortuosidade |> DHARMa::simulateResiduals(plot = TRUE)

### Avaliação do modelo ----

glmm_tortuosidade |> summary()

# Gráficos ----

## Linha de tendencia -----

## Nome das variáveis ----

variaveis <- amb_trat |>
  dplyr::select(temperatura_maxima,
              umidade_relativa_maxima,
              precipitacao) |>
  names()

variaveis

### Vetorizando ----

criar_linhas <- function(variaveis){

  tendencia <- ggeffects::ggpredict(model = glmm_tortuosidade,
                                    terms = variaveis) |>
    as.data.frame() |>
    dplyr::select(1:2) |>
    dplyr::mutate(Preditor = variaveis) |>
    dplyr::rename("Predictor value" = 1,
                  "Predicted" = 2)

  assign(paste0("tendencia_tortuosidade_",
                variaveis),
         tendencia,
         envir = globalenv())

}

purrr::map(variaveis, criar_linhas)

### Unindo os dados ----

df_tendencia <- ls(pattern = "^tendencia_tortuosidade_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::group_by(Preditor) |>
  dplyr::slice(c(1, n()))

df_tendencia

## Médias dos valores ----

medianas_valores <- amb_trat |>
  tidyr::pivot_longer(cols = c(3, 6, 8),
                      names_to = "Preditor",
                      values_to = "Predictor value") |>
  dplyr::summarise(`Predictor value` = `Predictor value` |>
                     range() |>
                     median(),
                   .by = Preditor)

medianas_valores

## Dataframe das estatísticas ----

glmm_summary <- glmm_tortuosidade |> summary()

sts_glmm <- glmm_summary$coefficients$cond |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::filter(rowname != "(Intercept)") |>
  dplyr::rename("Preditor" = 1) |>
  dplyr::mutate(Preditor = Preditor |> stringr::str_remove_all("`"),
                Significante = dplyr::if_else(`Pr(>|z|)` < 0.05,
                                              "Sim",
                                              "Não"),
                Tortuosidade = 1.05,
                sts = paste0("<b>β1 ± SE = ",
                             Estimate |> round(2),
                             " ± ",
                             `Std. Error` |> round(2),
                             "<br>t<sub>",
                             37,
                             "</sub> = ",
                             `z value` |> round(2),
                             ", p ",
                             dplyr::if_else(`Pr(>|z|)` < 0.01,
                                            "< 0.01",
                                            paste0("= ", `Pr(>|z|)` |> round(3))),
                             "</b>")) |>
  dplyr::select(1, 6:8) |>
  dplyr::left_join(medianas_valores,
                   by = "Preditor")

sts_glmm

## Gráfico ----

amb_trat |>
  tidyr::pivot_longer(cols = c(3, 6, 8),
                      names_to = "Preditor",
                      values_to = "Predictor value") |>
  dplyr::left_join(sts_glmm |>
                     dplyr::select(1, 2),
                   by = "Preditor") |>
  ggplot(aes(`Predictor value`, Tortuosidade)) +
  geom_point(size = 3.5) +
  geom_line(data = df_tendencia |>
              dplyr::left_join(sts_glmm |>
                                 dplyr::select(1:2),
                               by = "Preditor") |>
              dplyr::filter(Significante == "Sim"),
            aes(`Predictor value`, Predicted),
            color = "blue",
            linewidth = 1) +
  ggtext::geom_richtext(data = sts_glmm,
                        aes(`Predictor value`, Tortuosidade, label = sts),
                        label.colour = NA,
                        fill = NA,
                        size = 7.5) +
  facet_wrap(~Preditor, scales = "free_x") +
  scale_y_continuous(limits = c(0, 1.1)) +
  labs(y = "Tortuosity") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 20),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "modelo_linear_misto_variaveis_ambientais_tortuosidade.png",
       height = 10, width = 12)
