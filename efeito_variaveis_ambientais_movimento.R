# Pacotes ----

library(readxl)

library(tidyverse)

library(parzer)

library(geosphere)

library(sf)

library(adehabitatHR)

library(janitor)

library(reshape2)

library(ggview)

library(lmerTest)

library(performance)

library(lme4)

library(ggeffects)

library(DHARMa)

# Dados ----

## Dados de movimentação ----

### Importando ----

importar_dados <- function(especies){

  dados <- readxl::read_xlsx(paste0("Dados ", especies, ".xlsx")) |>
    dplyr::mutate(especie = especies)

  assign(paste0("dados_", especies |> stringr::word(2)),
         dados,
         envir = globalenv())

}

especies <- c("Boana faber",
              "Phyllomedusa tetraploidea",
              "Rhinella crucifer",
              "Rhinella icterica",
              "Odontophrynus asper",
              "Leptodactylus mystacinus")

sps_dados <- purrr::map(especies,
                        \(especies){

                          dados <- readxl::read_xlsx(paste0("Dados ",
                                                            especies,
                                                            ".xlsx")) |>
                            dplyr::mutate(especie = especies)

                          }) |>
  dplyr::bind_rows()

### Visualizando ----

sps_dados

sps_dados |> dplyr::glimpse()

## Dados ambientais -----

### Importando -----

amb <- readxl::read_xlsx("DADOS AMBIENTAIS COM AS 6 ESPÉCIES INDIVIDUAIS.xlsx")

### Visualizando -----

amb

amb |> dplyr::glimpse()

# Tratando as coordenadas ----

## Convertendo as coordenadas ----

sps_dados <- sps_dados |>
  dplyr::mutate(Longitude = Longitude |> parzer::parse_lon(),
                Latitude = Latitude |> parzer::parse_lat())

sps_dados

## Completando as coordenadas ----

purrr::map(1:nrow(sps_dados),
           \(x){

             if(sps_dados$Longitude[x] |> is.na() |
                sps_dados$Latitude[x] |> is.na()){

               dest <- geosphere::destPoint(c(sps_dados$Longitude[x-1],
                                              sps_dados$Latitude[x-1]),
                                            sps_dados$`Angulo de virada`[x-1],
                                            sps_dados$`Distância (m)`[x-1])

               sps_dados$Longitude[x] <<- dest[1]

               sps_dados$Latitude[x] <<- dest[2]

             }

           },
           .progress = TRUE)

sps_dados |> as.data.frame()

## Transformando um shapefile ----

sf_dados <- sps_dados |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = 4674)

sf_dados

ggplot() +
  geom_sf(data = sf_dados, aes(color = especie))

# Movimentação ----

## Individuos ----

ind <- sf_dados |>
  dplyr::pull(ID) |>
  unique()

ind

## Área do mínimo polígono convexo ----

df_area <- purrr::map(ind,
                      purrr::in_parallel(

                        \(id){

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

                          tibble::tibble(INDIVIDUO = id,
                                         mcp100 = mcp_100,
                                         mcp95 = mcp_095,
                                         Kernel95 = kernel_95,
                                         kernel50 = kernel_50)

                          }
                        ),
                      .progress = TRUE) |>
  dplyr::bind_rows() |>
  dplyr::left_join(sps_dados |>
                     dplyr::select(ID, especie) |>
                     dplyr::rename("INDIVIDUO" = ID) |>
                     dplyr::distinct(INDIVIDUO, .keep_all = TRUE),
                   by = "INDIVIDUO")

df_area

df_area |> dplyr::glimpse()

## Média dos dados para os indivíduos ----

amb_trat <- amb |>
  dplyr::summarise(dplyr::across(.cols = dplyr::where(is.numeric),
                                 .fns = ~mean(.)),
                   .by = INDIVIDUO) |>
  dplyr::mutate(INDIVIDUO = INDIVIDUO |> stringr::str_replace("B", "BF")) |>
  dplyr::left_join(df_area,
                   by = "INDIVIDUO") |>
  janitor::clean_names()

amb_trat

amb_trat |> dplyr::glimpse()

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
  dplyr::filter(Var1 != Var2) |>
  dplyr::mutate(`Spearman's correlation index` = value |> round(2)) |>
  tidyr::drop_na() |>
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

# Modelo para os MCPs ----

## 100% ----

### Criando o modelo ----

modelo_mcp100 <- lmerTest::lmer(mcp100 ~ temperatura_maxima +
                                  umidade_relativa_maxima +
                                  precipitacao +
                                  (1 | especie),
                                data = amb_trat)

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

modelo_glm_mcp100 <- lme4::glmer(mcp100 ~ temperatura_maxima +
                                   umidade_relativa_maxima +
                                   precipitacao +
                                   (1 | especie),
                                 family = Gamma(link = "log"),
                                 data = amb_trat)

### Pressupostos do modelo ----

modelo_glm_mcp100 |> DHARMa::simulateResiduals(plot = TRUE)

### Avaliação do modelo ----

modelo_glm_mcp100 |> summary()

## 95% ----

### Criando o modelo ----

modelo_mcp095 <- lmerTest::lmer(mcp95 ~ temperatura_maxima +
                                  umidade_relativa_maxima +
                                  precipitacao +
                                  (1 | especie),
                                data = amb_trat)

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

modelo_glm_mcp095 <- lme4::glmer(mcp95 ~ temperatura_maxima +
                                   umidade_relativa_maxima +
                                   precipitacao +
                                   (1 | especie),
                                 family = Gamma(link = "log"),
                                 data = amb_trat)

### Pressupostos do modelo ----

modelo_glm_mcp095 |> DHARMa::simulateResiduals(plot = TRUE)

### Avaliação do modelo ----

modelo_glm_mcp095 |> summary()

# Modelo para os kernels ----

## 95% ----

### Criando o modelo ----

modelo_kernel95 <- lmerTest::lmer(kernel95 ~ temperatura_maxima +
                                    umidade_relativa_maxima +
                                    precipitacao +
                                    (1 | especie),
                                  data = amb_trat)

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

modelo_glm_kernel95 <- lme4::glmer(kernel95 ~ temperatura_maxima +
                                     umidade_relativa_maxima +
                                     precipitacao +
                                     (1 | especie),
                                   family = Gamma(link = "log"),
                                   data = amb_trat)

### Pressupostos do modelo ----

modelo_glm_kernel95 |> DHARMa::simulateResiduals(plot = TRUE)

### Avaliação do modelo ----

modelo_glm_kernel95 |> summary()

## 50% ----

### Criando o modelo ----

modelo_kernel50 <- lmerTest::lmer(kernel50 ~ temperatura_maxima +
                                    umidade_relativa_maxima +
                                    precipitacao +
                                    (1 | especie),
                                  data = amb_trat)

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

modelo_glm_kernel50 <- lme4::glmer(kernel50 ~ temperatura_maxima +
                                     umidade_relativa_maxima +
                                     precipitacao +
                                     (1 | especie),
                                   family = Gamma(link = "log"),
                                   data = amb_trat)

### Pressupostos do modelo ----

modelo_glm_kernel50 |> DHARMa::simulateResiduals(plot = TRUE)

### Avaliação do modelo ----

modelo_glm_kernel50 |> summary()

# Gráfico -----

## Médias dos valores ----

medianas_valores <- amb_trat |>
  tidyr::pivot_longer(cols = c(temperatura_maxima,
                               umidade_relativa_maxima,
                               precipitacao),
                      names_to = "Preditor",
                      values_to = "Predictor value") |>
  dplyr::summarise(`Predictor value` = `Predictor value` |>
                     range() |>
                     median(),
                   .by = Preditor)

medianas_valores

## Linhas de tendencia ----

### Listando modelos ----

modelos <- ls(pattern = "^modelo_glm_") |>
  rep(each = 3) |>
  mget(envir = globalenv())

modelos

### Variáveis ambientais ----

variaveis <- amb_trat |>
  dplyr::select(temperatura_maxima,
                umidade_relativa_maxima,
                precipitacao) |>
  names() |>
  rep(times = 4)

variaveis

### Vetorização ----

df_tendencia <- purrr::map(1:length(modelos),
                           purrr::in_parallel(

                             \(id){

                               tipo_movimento <- modelos[id] |>
                                 names() |>
                                 stringr::str_replace_all("_", " ") |>
                                 stringr::str_extract("\\w+$")

                               ggeffects::ggpredict(model = modelos[[id]],
                                    terms = variaveis[id]) |>
                                 as.data.frame() |>
                                 dplyr::select(1:2) |>
                                 dplyr::mutate(Preditor = variaveis[id],
                                               movimentacao = tipo_movimento) |>
                                 dplyr::rename("Predictor value" = 1,
                                               "Predicted" = 2)

                               }
                             ),
                           .progress = TRUE) |>
  dplyr::bind_rows() |>
  dplyr::group_by(Preditor, movimentacao) |>
  dplyr::slice(c(1, n()))

df_tendencia

## MCP 100% ----

### Dataframe das estatísticas ----

sts_mcp100 <- modelo_glm_mcp100 |>
  summary() %>%
  .$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::filter(rowname != "(Intercept)") |>
  dplyr::rename("Preditor" = 1) |>
  dplyr::mutate(Preditor = Preditor |> stringr::str_remove_all("`"),
                Significante = dplyr::if_else(`Pr(>|z|)` < 0.05,
                                              "Sim",
                                              "Não"),
                mcp100 = 17500,
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

amb_trat |>
  tidyr::pivot_longer(cols = c(temperatura_maxima,
                               umidade_relativa_maxima,
                               precipitacao),
                      names_to = "Preditor",
                      values_to = "Predictor value") |>
  dplyr::left_join(sts_mcp100 |>
                     dplyr::select(1, 2),
                   by = "Preditor") |>
  ggplot(aes(`Predictor value`, mcp100)) +
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
                        aes(`Predictor value`, mcp100, label = sts),
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

ggsave(filename = "modelo_linear_misto_var_amb_mcp100.png",
       height = 10, width = 12)

## MCP 95% ----

### Dataframe das estatísticas ----

sts_mcp095 <- modelo_glm_mcp095 |>
  summary() %>%
  .$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::filter(rowname != "(Intercept)") |>
  dplyr::rename("Preditor" = 1) |>
  dplyr::mutate(Preditor = Preditor |> stringr::str_remove_all("`"),
                Significante = dplyr::if_else(`Pr(>|z|)` < 0.05,
                                              "Sim",
                                              "Não"),
                mcp095 = 17500,
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

amb_trat |>
  tidyr::pivot_longer(cols = c(temperatura_maxima,
                               umidade_relativa_maxima,
                               precipitacao),
                      names_to = "Preditor",
                      values_to = "Predictor value") |>
  dplyr::left_join(sts_mcp095 |>
                     dplyr::select(1, 2),
                   by = "Preditor") |>
  ggplot(aes(`Predictor value`, mcp100)) +
  geom_point(size = 3.5) +
  geom_line(data = df_tendencia |>
              dplyr::left_join(sts_mcp095 |>
                                 dplyr::select(1:2),
                               by = "Preditor") |>
              dplyr::filter(movimentacao == "mcp095" &
                              Significante == "Sim"),
            aes(`Predictor value`, Predicted),
            color = "blue",
            linewidth = 1) +
  ggtext::geom_richtext(data = sts_mcp095,
                        aes(`Predictor value`, mcp095, label = sts),
                        label.colour = NA,
                        fill = NA,
                        size = 7.5) +
  facet_wrap(~Preditor, scales = "free_x") +
  labs(y = "MCP 95% Area") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 20),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "modelo_linear_misto_var_amb_mcp095.png",
       height = 10, width = 12)

## KDE 95% ----

### Dataframe das estatísticas ----

sts_kernel095 <- modelo_glm_kernel95 |>
  summary() %>%
  .$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::filter(rowname != "(Intercept)") |>
  dplyr::rename("Preditor" = 1) |>
  dplyr::mutate(Preditor = Preditor |> stringr::str_remove_all("`"),
                Significante = dplyr::if_else(`Pr(>|z|)` < 0.05,
                                              "Sim",
                                              "Não"),
                kernel95 = 22500,
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

sts_kernel095

### Gráfico ----

amb_trat |>
  tidyr::pivot_longer(cols = c(temperatura_maxima,
                               umidade_relativa_maxima,
                               precipitacao),
                      names_to = "Preditor",
                      values_to = "Predictor value") |>
  dplyr::left_join(sts_kernel095 |>
                     dplyr::select(1, 2),
                   by = "Preditor") |>
  ggplot(aes(`Predictor value`, kernel95)) +
  geom_point(size = 3.5) +
  geom_line(data = df_tendencia |>
              dplyr::left_join(sts_kernel095 |>
                                 dplyr::select(1:2),
                               by = "Preditor") |>
              dplyr::filter(movimentacao == "kernel95" &
                              Significante == "Sim"),
            aes(`Predictor value`, Predicted),
            color = "blue",
            linewidth = 1) +
  ggtext::geom_richtext(data = sts_kernel095,
                        aes(`Predictor value`, kernel95, label = sts),
                        label.colour = NA,
                        fill = NA,
                        size = 7.5) +
  facet_wrap(~Preditor, scales = "free_x") +
  labs(y = "KDE 95% Area") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        axis.title = element_text(color = "black", size = 20),
        strip.text = element_text(color = "black", size = 20),
        strip.background = element_rect(color = "black", linewidth = 1),
        panel.border = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "modelo_linear_misto_var_amb_kernel095.png",
       height = 10, width = 12)

## KDE 50% ----

### Dataframe das estatísticas ----

sts_kernel050 <- modelo_glm_kernel50 |>
  summary() %>%
  .$coefficients |>
  as.data.frame() |>
  tibble::rownames_to_column() |>
  dplyr::filter(rowname != "(Intercept)") |>
  dplyr::rename("Preditor" = 1) |>
  dplyr::mutate(Preditor = Preditor |> stringr::str_remove_all("`"),
                Significante = dplyr::if_else(`Pr(>|z|)` < 0.05,
                                              "Sim",
                                              "Não"),
                kernel50 = 5000,
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

sts_kernel050

### Gráfico ----

amb_trat |>
  tidyr::pivot_longer(cols = c(temperatura_maxima,
                               umidade_relativa_maxima,
                               precipitacao),
                      names_to = "Preditor",
                      values_to = "Predictor value") |>
  dplyr::left_join(sts_kernel050 |>
                     dplyr::select(1, 2),
                   by = "Preditor") |>
  ggplot(aes(`Predictor value`, kernel50)) +
  geom_point(size = 3.5) +
  geom_line(data = df_tendencia |>
              dplyr::left_join(sts_kernel050 |>
                                 dplyr::select(1:2),
                               by = "Preditor") |>
              dplyr::filter(movimentacao == "kernel50" &
                              Significante == "Sim"),
            aes(`Predictor value`, Predicted),
            color = "blue",
            linewidth = 1) +
  ggtext::geom_richtext(data = sts_kernel050,
                        aes(`Predictor value`, kernel50, label = sts),
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

ggsave(filename = "modelo_linear_misto_var_amb_kernel050.png",
       height = 10, width = 12)
