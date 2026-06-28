# Pacotes ----

library(readxl)

library(tidyverse)

library(parzer)

library(geosphere)

library(sf)

library(adehabitatHR)

library(reshape2)

library(ggview)

library(lmerTest)

library(performance)

library(lme4)

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

## Dados morfológivos

### Importando ----

importar_morf <- function(id){

  dados <- readxl::read_xlsx("médias das espécies.xlsx",
                             sheet = id) |>
    dplyr::mutate(Espécie = sps[id])

  assign(paste0("dados_morf_", sps[id] |> stringr::word(2)),
         dados,
         envir = globalenv())

}

sps <- c("Rhinella icterica",
         "Rhinella crucifer",
         "Boana faber",
         "Phyllomedusa tetraploidea",
         "Odontophrynus asper",
         "Leptodactylus mystacinus")

sps

purrr::map(1:6, importar_morf)

### Visualizando ----

ls(pattern = "dados_morf_") |>
  mget(envir = globalenv())

### Unindo os dados ----

sps_morf_dados <- ls(pattern = "dados_morf_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

sps_morf_dados

sps_morf_dados |> dplyr::glimpse()

### Tratando ----

sps_morf_dados <- sps_morf_dados |>
  dplyr::filter(!ID %in% c("MÉDIA", "MEDIA", "DP")) |>
  as.data.frame()

sps_morf_dados

sps_morf_dados |> dplyr::glimpse()

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

## Transformando um shapefile ----

sf_dados <- sps_mov_dados |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674)

sf_dados

ggplot() +
  geom_sf(data = sf_dados, aes(color = Espécie))

# Movimentação ----

## Individuos ----

ind <- sf_dados |>
  dplyr::pull(ID) |>
  unique()

ind

## Área do mínimo polígono convexo ----

mcp_sps <- function(id){

  mcp_100 <- sf_dados |>
    dplyr::filter(ID == id) |>
    sf::st_transform(crs = 32725) |>
    sf::as_Spatial() |>
    adehabitatHR::mcp(percent = 100) |>
    sf::st_as_sf() |>
    sf::st_area() |>
    as.numeric()

  mcp_095 <- sf_dados |>
    dplyr::filter(ID == id) |>
    sf::st_transform(crs = 32725) |>
    sf::as_Spatial() |>
    adehabitatHR::mcp(percent = 95) |>
    sf::st_as_sf() |>
    sf::st_area() |>
    as.numeric()

  vetor_mcp100 <<- c(vetor_mcp100, mcp_100)

  vetor_mcp095 <<- c(vetor_mcp095, mcp_095)

}

vetor_mcp100 <- c()

vetor_mcp095 <- c()

purrr::map(ind, mcp_sps)

vetor_mcp100

vetor_mcp095

## Área da área de vida ----

kernel_sps <- function(id){

  kernel_95 <- sf_dados |>
    dplyr::filter(ID == id) |>
    sf::st_transform(crs = 32725) |>
    sf::as_Spatial() |>
    adehabitatHR::kernelUD(h = "href",
                           extent = 3) |>
    adehabitatHR::getverticeshr(percent = 95) |>
    sf::st_as_sf() |>
    sf::st_area() |>
    as.numeric()

  kernel_50 <- sf_dados |>
    dplyr::filter(ID == id) |>
    sf::st_transform(crs = 32725) |>
    sf::as_Spatial() |>
    adehabitatHR::kernelUD(h = "href",
                           extent = 3) |>
    adehabitatHR::getverticeshr(percent = 50) |>
    sf::st_as_sf() |>
    sf::st_area() |>
    as.numeric()

  vetor_kernel95 <<- c(vetor_kernel95, kernel_95)

  vetor_kernel50 <<- c(vetor_kernel50, kernel_50)

}

vetor_kernel95 <- c()

vetor_kernel50 <- c()

purrr::map(ind, kernel_sps)

vetor_kernel95

vetor_kernel50

## Unindo os dados ----

mov_df <- ls(pattern = "vetor_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_cols() |>
  dplyr::mutate(ID = ind) |>
  as.data.frame()

mov_df

mov_df |> dplyr::glimpse()

## Unindo aos dados de morfologia ----

morf_df <- sps_morf_dados |>
  dplyr::left_join(mov_df,
                   by = "ID") |>
  as.data.frame()

morf_df

morf_df |> dplyr::glimpse()

# Multicolinearidade ----

## Calculando ----

multicor <- morf_df |>
  dplyr::select(2, 3, 6, 7) |>
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

ggsave(filename = "multicolinearidade_morfologia_cap1.png",
       height = 10, width = 12)

# Modelos para MCP ----

## 100% ----

### Criando o modelo ----

modelo_mcp100 <- lmerTest::lmer(vetor_mcp100 ~ peso +
                                  `Proporção CMP` +
                                  `Proporção CMA` +
                                  (1 | Espécie),
                                data = morf_df)

### Pressupostos do modelo ----

modelo_mcp100 |> performance::check_normality()

modelo_mcp100 |> performance::check_heteroscedasticity()

modelo_mcp100 |> performance::check_model(check = c("vif",
                                                    "qq",
                                                    "normality",
                                                    "homogeneity"))

### Avaliação do modelo ----

modelo_mcp100 |> summary()

### Criando o modelo para GLM gamma ----

modelo_glm_mcp100 <- lme4::glmer(vetor_mcp100 ~ peso +
                                   `Proporção CMP` +
                                   `Proporção CMA` +
                                   (1 | Espécie),
                                 data = morf_df,
                                 family = Gamma(link = "log"))

### Pressupostos do modelo ----

modelo_glm_mcp100 |> DHARMa::simulateResiduals(plot = TRUE)

### Avaliação do modelo ----

modelo_glm_mcp100 |> summary()

## 95% ----

### Criando o modelo ----

modelo_mcp095 <- lmerTest::lmer(vetor_mcp095 ~ peso +
                                  `Proporção CMP` +
                                  `Proporção CMA` +
                                  (1 | Espécie),
                                data = morf_df)

### Pressupostos do modelo ----

modelo_mcp095 |> performance::check_normality()

modelo_mcp095 |> performance::check_heteroscedasticity()

modelo_mcp095 |> performance::check_model(check = c("vif",
                                                    "qq",
                                                    "normality",
                                                    "homogeneity"))

### Avaliação do modelo ----

modelo_mcp095 |> summary()

### Criando o modelo para GLM gamma ----

modelo_glm_mcp095 <- lme4::glmer(vetor_mcp095 ~ peso +
                                   `Proporção CMP` +
                                   `Proporção CMA` +
                                   (1 | Espécie),
                                 data = morf_df,
                                 family = Gamma(link = "log"))

### Pressupostos do modelo ----

modelo_glm_mcp095 |> DHARMa::simulateResiduals(plot = TRUE)

### Avaliação do modelo ----

modelo_glm_mcp095 |> summary()

# Modelo para os kernels ----

## 95% ----

### Criando o modelo ----

modelo_kernel95 <- lmerTest::lmer(vetor_kernel95 ~ peso +
                                    `Proporção CMP` +
                                    `Proporção CMA` +
                                    (1 | Espécie),
                                  data = morf_df)

### Pressupostos do modelo ----

modelo_kernel95 |> performance::check_normality()

modelo_kernel95 |> performance::check_heteroscedasticity()

modelo_kernel95 |> performance::check_model(check = c("vif",
                                                      "qq",
                                                      "normality",
                                                      "homogeneity"))

### Avaliação do modelo ----

modelo_kernel95 |> summary()

### Criando o modelo para GLM gamma ----

modelo_glm_kernel95 <- lme4::glmer(vetor_kernel95 ~ peso +
                                     `Proporção CMP` +
                                     `Proporção CMA` +
                                     (1 | Espécie),
                                   data = morf_df,
                                   family = Gamma(link = "log"))

### Pressupostos do modelo ----

modelo_glm_kernel95 |> DHARMa::simulateResiduals(plot = TRUE)

### Avaliação do modelo ----

modelo_glm_kernel95 |> summary()

## 50% ----

### Criando o modelo ----

modelo_kernel50 <- lmerTest::lmer(vetor_kernel50 ~ peso +
                                    `Proporção CMP` +
                                    `Proporção CMA` +
                                    (1 | Espécie),
                                  data = morf_df)

### Pressupostos do modelo ----

modelo_kernel50 |> performance::check_normality()

modelo_kernel50 |> performance::check_heteroscedasticity()

modelo_kernel50 |> performance::check_model(check = c("vif",
                                                      "qq",
                                                      "normality",
                                                      "homogeneity"))

### Avaliação do modelo ----

modelo_kernel50 |> summary()

### Criando o modelo para GLM gamma ----

modelo_glm_kernel50 <- lme4::glmer(vetor_kernel50 ~ peso +
                                     `Proporção CMP` +
                                     `Proporção CMA` +
                                     (1 | Espécie),
                                   data = morf_df,
                                   family = Gamma(link = "log"))

### Pressupostos do modelo ----

modelo_glm_kernel50 |> DHARMa::simulateResiduals(plot = TRUE)

### Avaliação do modelo ----

modelo_glm_kernel50 |> summary()

# Gráficos ----

## Linhas de tendência dos modelos ----

### Listando os modelos ----

modelos <- ls(pattern = "^modelo_glm_") |>
  rep(each = 3) |>
  mget(envir = globalenv())

modelos

### Nomes das variáveis ----

variaveis <- morf_df |>
  dplyr::select(peso, `Proporção CMA`, `Proporção CMP`) |>
  names() |>
  rep(times = 4)

variaveis

### Gerando as linhas ----

criar_linhas <- function(id){

  tipo_movimento <- modelos[id] |>
    names() |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_extract("\\w+$")

  tendencia <- ggeffects::ggpredict(model = modelos[[id]],
                                    terms = variaveis[id]) |>
    as.data.frame() |>
    dplyr::select(1:2) |>
    dplyr::mutate(Preditor = variaveis[id],
                  movimentacao = tipo_movimento) |>
    dplyr::rename("Predictor value" = 1,
                  "Predicted" = 2)

  assign(paste0("tendencia_",
                tipo_movimento,
                "_",
                variaveis[id]),
         tendencia,
         envir = globalenv())

}

purrr::map(1:length(modelos),
           criar_linhas)

### Unindo os dados ----

df_tendencia <- ls(pattern = "^tendencia_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  dplyr::group_by(Preditor, movimentacao) |>
  dplyr::slice(c(1, n()))

df_tendencia

## Médias dos valores ----

medianas_valores <- morf_df |>
  tidyr::pivot_longer(cols = c(2, 6, 7),
                      names_to = "Preditor",
                      values_to = "Predictor value") |>
  dplyr::summarise(`Predictor value` = `Predictor value` |>
                     range() |>
                     median(),
                   .by = Preditor)

medianas_valores

## MCP 100 % ----

### Dataframe das estatísticas ----

mcp100_summary <- modelo_glm_mcp100 |> summary()

sts_mcp100 <- mcp100_summary$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::filter(rowname != "(Intercept)") |>
  dplyr::rename("Preditor" = 1) |>
  dplyr::mutate(Preditor = Preditor |> stringr::str_remove_all("`"),
                Significante = dplyr::if_else(`Pr(>|z|)` < 0.05,
                                              "Sim",
                                              "Não"),
                vetor_mcp100 = 17500,
                sts = paste0("<b>β1 ± SE = ",
                             Estimate |> round(2),
                             " ± ",
                             `Std. Error` |> round(2),
                             "<br>t<sub>",
                             37,
                             "</sub> = ",
                             `t value` |> round(2),
                             ", p = ",
                             dplyr::if_else(`Pr(>|z|)` < 0.01,
                                            "< 0.01",
                                            `Pr(>|z|)` |> round(3) |> as.character()),
                             "</b>")) |>
  dplyr::select(1, 6:8) |>
  dplyr::left_join(medianas_valores,
                   by = "Preditor")

sts_mcp100

### Gráfico ----

morf_df |>
  tidyr::pivot_longer(cols = c(2, 6, 7),
                      names_to = "Preditor",
                      values_to = "Predictor value") |>
  dplyr::left_join(sts_mcp100 |>
                     dplyr::select(1, 2),
                   by = "Preditor") |>
  ggplot(aes(`Predictor value`, vetor_mcp100)) +
  geom_point(size = 3.5) +
  geom_line(data = df_tendencia |>
              dplyr::left_join(sts_mcp100 |>
                                 dplyr::select(1:2),
                               by = "Preditor") |>
              dplyr::filter(movimentacao == "mcp100" &
                              Significante == "Sim"),
            aes(`Predictor value`, Predicted),
            color = "blue",
            linewidth = 1) +
  ggtext::geom_richtext(data = sts_mcp100,
                        aes(`Predictor value`, vetor_mcp100, label = sts),
                        label.colour = NA,
                        fill = NA,
                        size = 7.5) +
  facet_wrap(~Preditor, scales = "free_x") +
  labs(y = "MCP 100% Area") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 20),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "modelo_linear_misto_morfologia_mcp100.png",
       height = 10, width = 12)

## MCP 95% ----

### Dataframe das estatísticas ----

mcp095_summary <- modelo_glm_mcp095 |> summary()

sts_mcp095 <- mcp095_summary$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::filter(rowname != "(Intercept)") |>
  dplyr::rename("Preditor" = 1) |>
  dplyr::mutate(Preditor = Preditor |> stringr::str_remove_all("`"),
                Significante = dplyr::if_else(`Pr(>|z|)` < 0.05,
                                              "Sim",
                                              "Não"),
                vetor_mcp100 = 6000,
                sts = paste0("<b>β1 ± SE = ",
                             Estimate |> round(2),
                             " ± ",
                             `Std. Error` |> round(2),
                             "<br>t<sub>",
                             37,
                             "</sub> = ",
                             `t value` |> round(2),
                             ", p = ",
                             dplyr::if_else(`Pr(>|z|)` < 0.01,
                                            "< 0.01",
                                            `Pr(>|z|)` |> round(3) |> as.character()),
                             "</b>")) |>
  dplyr::select(1, 6:8) |>
  dplyr::left_join(medianas_valores,
                   by = "Preditor")

sts_mcp095

### Gráfico ----

morf_df |>
  tidyr::pivot_longer(cols = c(2, 6, 7),
                      names_to = "Preditor",
                      values_to = "Predictor value") |>
  dplyr::left_join(sts_mcp095 |>
                     dplyr::select(1, 2),
                   by = "Preditor") |>
  ggplot(aes(`Predictor value`, vetor_mcp095)) +
  geom_point(size = 3.5) +
  geom_line(data = df_tendencia |>
              dplyr::left_join(sts_mcp095 |>
                                 dplyr::select(1:2),
                               by = "Preditor") |>
              dplyr::filter(movimentacao == "mcp100" &
                              Significante == "Sim"),
            aes(`Predictor value`, Predicted),
            color = "blue",
            linewidth = 1) +
  ggtext::geom_richtext(data = sts_mcp095,
                        aes(`Predictor value`, vetor_mcp100, label = sts),
                        label.colour = NA,
                        fill = NA,
                        size = 7.5) +
  facet_wrap(~Preditor, scales = "free_x") +
  labs(y = "MCP 095% Area") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 20),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "modelo_linear_misto_morfologia_mcp095.png",
       height = 10, width = 12)

## Kernel 95% ----

### Dataframe das estatísticas ----

kde95_summary <- modelo_glm_kernel95 |> summary()

sts_kde095 <- kde95_summary$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::filter(rowname != "(Intercept)") |>
  dplyr::rename("Preditor" = 1) |>
  dplyr::mutate(Preditor = Preditor |> stringr::str_remove_all("`"),
                Significante = dplyr::if_else(`Pr(>|z|)` < 0.05,
                                              "Sim",
                                              "Não"),
                vetor_mcp100 = 25000,
                sts = paste0("<b>β1 ± SE = ",
                             Estimate |> round(2),
                             " ± ",
                             `Std. Error` |> round(2),
                             "<br>t<sub>",
                             37,
                             "</sub> = ",
                             `t value` |> round(2),
                             ", p = ",
                             dplyr::if_else(`Pr(>|z|)` < 0.01,
                                            "< 0.01",
                                            `Pr(>|z|)` |> round(3) |> as.character()),
                             "</b>")) |>
  dplyr::select(1, 6:8) |>
  dplyr::left_join(medianas_valores,
                   by = "Preditor")

sts_kde095

### Gráfico ----

morf_df |>
  tidyr::pivot_longer(cols = c(2, 6, 7),
                      names_to = "Preditor",
                      values_to = "Predictor value") |>
  dplyr::left_join(sts_kde095 |>
                     dplyr::select(1, 2),
                   by = "Preditor") |>
  ggplot(aes(`Predictor value`, vetor_kernel95)) +
  geom_point(size = 3.5) +
  geom_line(data = df_tendencia |>
              dplyr::left_join(sts_kde095 |>
                                 dplyr::select(1:2),
                               by = "Preditor") |>
              dplyr::filter(movimentacao == "kernel95" &
                              Significante == "Sim"),
            aes(`Predictor value`, Predicted),
            color = "blue",
            linewidth = 1) +
  ggtext::geom_richtext(data = sts_kde095,
                        aes(`Predictor value`, vetor_mcp100, label = sts),
                        label.colour = NA,
                        fill = NA,
                        size = 7.5) +
  facet_wrap(~Preditor, scales = "free_x") +
  labs(y = "KDE 095% Area") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 20),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "modelo_linear_misto_morfologia_kde095.png",
       height = 10, width = 12)

## Kernel 50% ----

### Dataframe das estatísticas ----

kernel50_summary <- modelo_glm_kernel50 |> summary()

sts_kernel50 <- kernel50_summary$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::filter(rowname != "(Intercept)") |>
  dplyr::rename("Preditor" = 1) |>
  dplyr::mutate(Preditor = Preditor |> stringr::str_remove_all("`"),
                Significante = dplyr::if_else(`Pr(>|z|)` < 0.05,
                                              "Sim",
                                              "Não"),
                vetor_mcp100 = 5000,
                sts = paste0("<b>β1 ± SE = ",
                             Estimate |> round(2),
                             " ± ",
                             `Std. Error` |> round(2),
                             "<br>t<sub>",
                             37,
                             "</sub> = ",
                             `t value` |> round(2),
                             ", p = ",
                             dplyr::if_else(`Pr(>|z|)` < 0.01,
                                            "< 0.01",
                                            `Pr(>|z|)` |> round(3) |> as.character()),
                             "</b>")) |>
  dplyr::select(1, 6:8) |>
  dplyr::left_join(medianas_valores,
                   by = "Preditor")

sts_kernel50

### Gráfico ----

morf_df |>
  tidyr::pivot_longer(cols = c(2, 6, 7),
                      names_to = "Preditor",
                      values_to = "Predictor value") |>
  dplyr::left_join(sts_kernel50 |>
                     dplyr::select(1, 2),
                   by = "Preditor") |>
  ggplot(aes(`Predictor value`, vetor_kernel50)) +
  geom_point(size = 3.5) +
  geom_line(data = df_tendencia |>
              dplyr::left_join(sts_kernel50 |>
                                 dplyr::select(1:2),
                               by = "Preditor") |>
              dplyr::filter(movimentacao == "kernel50" &
                              Significante == "Sim"),
            aes(`Predictor value`, Predicted),
            color = "blue",
            linewidth = 1) +
  ggtext::geom_richtext(data = sts_kernel50,
                        aes(`Predictor value`, vetor_mcp100, label = sts),
                        label.colour = NA,
                        fill = NA,
                        size = 7.5) +
  facet_wrap(~Preditor, scales = "free_x") +
  labs(y = "KDE 50% Area") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 20),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "modelo_linear_misto_morfologia_kde50.png",
       height = 10, width = 12)
