# Pacotes ----

library(tidyverse)

df <- data.frame(
  campanha = rep(c("Primavera 2025", "Verão 2026"), each=2),
  area     = rep(c("Área A", "Área B"), times=2),
  h        = c(0, 0.301, 0.8451, 0.7782)
)


ggplot(df, aes(x=campanha, y=h, fill=area)) +
  annotate("segment", x=0.6, xend=2.6, y= seq(0, 1, 0.1), yend= seq(0, 1, 0.1),
           linewidth=0.4, color="gray") +
  geom_col(position="dodge", width=0.6) +
  scale_fill_manual(values=c("Área A"="grey30", "Área B"="grey75")) +
  scale_x_discrete(expand=c(0, 0.4)) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.1),
    labels = \(x) gsub("\\.", ",", x),
    limits = c(-0.67, 1),
    expand = c(0, 0)
  ) +
  # Labels do eixo y redesenhados junto ao geom_vline
  annotate("text",
           x = 0.575,
           y = seq(0, 1, 0.1),
           label = seq(0, 1, 0.1) |>
             as.character() |>
             stringr::str_replace("\\.", ","),
           size=7.5,
           hjust = 1, size = 3, color = "black"
  ) +
  # Linhas horizontais da tabela
  annotate("segment",
           x = 0.6, xend = 2.6, y = 0.00, yend = 0.00,
           linewidth = 0.4, color="black") +
  annotate("segment",
           x = 0.20, xend = 2.6, y = -0.22, yend = -0.22,
           linewidth=0.4, color="black") +
  annotate("segment",
           x = 0.20, xend = 2.6, y = -0.44, yend = -0.44,
           linewidth = 0.4, color = "black") +
  annotate("segment",
           x = 0.20, xend = 2.6, y = -0.66, yend = -0.66,
           linewidth = 0.4, color = "black") +
  # Linhas verticais da tabela
  annotate("segment",
           x = 0.2, xend = 0.2, y = -0.67, yend = -0.22,
           linewidth = 0.4, color = "black") +
  annotate("segment",
           x = 0.6, xend = 0.6, y = -0.67, yend = -0,
           linewidth = 0.4, color = "black") +
  annotate("segment",
           x = 1.55, xend = 1.55, y = -0.66, yend = 0,
           inewidth = 0.4, color = "black") +
  annotate("segment",
           x = 2.60, xend = 2.60, y = -0.66, yend = 0,
           linewidth = 0.4, color = "black") +
  # Cabeçalho
  annotate("text",
           x = 1, y = -0.11, label = "Primavera 2025",
           hjust = 0.5, size = 7.5, color = "black", hjust = 0.5) +
  annotate("text",
           x = 2, y = -0.11, label = "Verão 2026",
           hjust = 0.5, size = 7.5, color = "black") +
  # Quadradinhos
  annotate("rect",
           xmin = 0.25, xmax = 0.4, ymin = -0.415, ymax = -0.245,
           fill = "grey30", color = NA) +
  annotate("rect",
           xmin = 0.25, xmax = 0.4, ymin = -0.635, ymax = -0.465,
           fill = "grey75", color = NA) +
  # Rótulos das áreas
  annotate("text",
           x = 0.425, y = -0.33, label = "Área A",
           hjust = 0, size = 7.5, color = "black") +
  annotate("text",
           x = 0.425, y = -0.55, label = "Área B",
           hjust = 0, size = 7.5, color = "black") +
  # Valores
  annotate("text",
           x = 1, y = -0.33, label = "0",
           hjust = 0.5, size = 7.5, color = "black") +
  annotate("text",
           x = 1, y = -0.55, label = "0,301",
           hjust = 0.5, size = 7.5, color = "black") +
  annotate("text",
           x = 2, y = -0.33, label = "0,8451",
           hjust = 0.5, size = 7.5, color = "black") +
  annotate("text",
           x = 2, y = -0.55, label = "0,7782",
           hjust = 0.5, size = 7.5, color = "black") +
  labs(title = "Índice de Diversidade de Shannon-Wiener",
       x = NULL, y = NULL) +
  coord_cartesian(clip="off") +
  theme_classic() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text = element_text(color = "black", size = 25),
        axis.title = element_text(color = "black", size = 25),
        axis.title.y = element_text(margin = margin(r = 6)),
        axis.title.x = element_text(margin = margin(t = 6)),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA),
        legend.key.width = unit(28, "pt"),
        legend.text = element_text(size = 25),
        legend.title = element_blank(),
        legend.position = "none",
        legend.direction = "horizontal",
        legend.spacing.x = unit(12, "pt"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_blank(),   # suprime labels originais
        axis.ticks.y = element_blank(),   # suprime ticks originais
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        plot.margin = margin(t=10, r=14, b=20, l=50),
        plot.title = element_text(hjust = 0.5, size = 25)) +
  ggview::canvas(height = 10, width = 16)
