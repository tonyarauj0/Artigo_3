# Analise Exploratoria ideologia
library(tidyverse)

# 1. Carregar os dados ----------------------------------------------------

dados <- readRDS(file = here::here("data", "clean", "base_estimacao_completa_1.rds"))

# 2 Selecionar apenas quem votou na EMA 22 ---

base <- dados |>
    filter(ema22 == "Sim" | ema22 == "Não") |>
    mutate_if(is.character, str_to_upper) |>
    mutate_if(is.factor, str_to_upper) |>
    mutate_if(is.character, as.factor)


# 3. Boxplot ideologia --------------------------------------
rain_height <- .1
(gr_ideologia <- base |> select(deputado_id, dim1, ema22) |>
     ggplot(aes(x = "", y = dim1, fill = ema22)) +
     introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                    position = position_nudge(x = rain_height+.05)) +
     geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE,
                  outlier.shape = NA,
                  position = position_nudge(x = -rain_height*2)) +
     stat_summary(fun.data = mean_cl_normal, mapping = aes(color = ema22), show.legend = FALSE,
                  position = position_nudge(x = rain_height * 3)) +
     scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
     coord_flip() +
     scale_fill_manual(values = c("firebrick", "dodgerblue4")) +
     scale_colour_manual(values = c("firebrick", "dodgerblue4")) +
     theme_minimal() +
     labs(fill = "", y = "") +
     theme(panel.grid.major.y = element_blank(),
           legend.background = element_rect(fill = "white", color = "white"))
)

ggplot2::ggsave(
    filename = here::here("figures", "dados", "ideologia", "ideologia.png"),
    plot = gr_ideologia ,
    dpi = 600,
    width = 8,
    height = 6
)
