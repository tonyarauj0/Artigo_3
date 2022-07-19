# Analise Exploratoria Financiamento
library(tidyverse)

# 1. Carregar os dados ----------------------------------------------------

dados <- readRDS(file = here::here("data", "clean", "base_estimacao_completa_1.rds"))

# 1.1 Selecionar apenas quem votou na EMA 22

base <- dados |>
    filter(ema22 == "Sim" | ema22 == "Não") |>
    mutate_if(is.character, str_to_upper) |>
    mutate_if(is.factor, str_to_upper) |>
    mutate_if(is.character, as.factor)

# 1.2 Modificar formato do banco de dados
base.1 <- base |>
    select(ema22, per_cand, per_pol, per_pf, per_pj, per_prop, receita_total, patrimonio,
           nm_atual_partido, regiao, deputado_sigla_uf, deputado_id) |>
    pivot_longer(cols = c(per_cand, per_pol, per_pf, per_pj, per_prop),
                 names_to = "tipo_receita",
                 values_to = "valor")

base.2 <- base.1 |>
    mutate(valor = case_when(
        valor == 0 ~ 1,
        TRUE ~ valor
    ),
    logvalor = log10(valor))




# 2. Boxplot fontes de financiamento --------------------------------------
rain_height <- .1

(gr_receita_fontes <- ggplot(base.1, aes(x = "", y = valor, fill = ema22)) +
    # clouds
    introdataviz::geom_flat_violin(trim=FALSE, alpha = 0.4,
                                   position = position_nudge(x = rain_height+.05)) +
    # rain
    # geom_point(aes(colour = ema22), size = 2, alpha = .5, show.legend = FALSE,
    #            position = position_jitter(width = rain_height, height = 0)) +
    # boxplots
    geom_boxplot(width = rain_height, alpha = 0.4, show.legend = FALSE,
                 outlier.shape = NA,
                 position = position_nudge(x = -rain_height*2)) +
    # mean and SE point in the cloud
    stat_summary(fun.data = mean_cl_normal, mapping = aes(color = ema22), show.legend = FALSE,
                 position = position_nudge(x = rain_height * 3)) +
    # adjust layout
    scale_x_discrete(name = "", expand = c(rain_height*3, 0, 0, 0.7)) +
    scale_y_continuous(name = "") +
                           # breaks = seq(-2, 10, 2),
                           # limits = c(-2, 10)) +
    coord_flip() +
    facet_wrap(~factor(tipo_receita,
                       levels = c("per_pj", "per_pf", "per_cand", "per_prop", "per_pol"),
                       labels = c("PJ", "PF", "OUTROS CANDIDATOS",
                                  "PROPRIOS", "PARTIDO")),
               ncol = 2) +
    # custom colours and theme
    scale_fill_manual(values = c("firebrick", "dodgerblue4")) +
    scale_colour_manual(values = c("firebrick", "dodgerblue4")) +
    theme_minimal() +
    labs(fill = "") +
    theme(panel.grid.major.y = element_blank(),
          legend.position = c(.8, .1),
          legend.background = element_rect(fill = "white", color = "white"))
)


ggplot2::ggsave(
    filename = here::here("figures", "dados", "financeiros", "receita_fontes.png"),
    plot = gr_receita_fontes ,
    dpi = 600,
    width = 12,
    height = 6
)

# 3. Boxplot receita total --------------------------------------

(gr_receita_total <- base |> select(deputado_id, receita_total, ema22) |>
     ggplot(aes(x = "", y = log(receita_total), fill = ema22)) +
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
    filename = here::here("figures", "dados", "financeiros", "receita_total.png"),
    plot = gr_receita_total ,
    dpi = 600,
    width = 12,
    height = 6
)


# 4. Boxplot patrimonio --------------------------------------

(gr_patrimonio <- base |> select(deputado_id, patrimonio, ema22) |>
     ggplot(aes(x = "", y = log(patrimonio), fill = ema22)) +
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
    filename = here::here("figures", "dados", "financeiros", "patrimonio.png"),
    plot = gr_patrimonio ,
    dpi = 600,
    width = 8,
    height = 6
)
