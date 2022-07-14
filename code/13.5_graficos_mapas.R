# Mapas

#Carregar pacotes
pacman::p_load(rgdal, raster, sf, rasterVis,
               tmap, tmaptools, tidyverse)

# 1 Carregar os dados ----
dados <- readRDS(file = here::here("data", "clean", "base_estimacao_completa_1.rds"))

# 2 Selecionar apenas quem votou na EMA 22 ----
base <- dados |>
    filter(ema22 == "Sim" | ema22 == "Não") |>
    mutate_if(is.character, str_to_upper) |>
    mutate_if(is.factor, str_to_upper)

base.1 <- base |>
    group_by(deputado_sigla_uf, ema22) |>
    summarise(n = n()) |>
    ungroup() |>
    group_by(deputado_sigla_uf) |>
    mutate(total_uf = sum(n),
           percentual = round((n/total_uf)*100,2)) |>
    ungroup()

# 3 Carregar contorno dos estados ----
sh_estados <- geobr::read_state()


# 4 Merge dos shapes com a base ---------------------------------------------
base.3 <- sh_estados |>
    left_join(base.1 |> filter(ema22 == "SIM"),
              by = c("abbrev_state" = "deputado_sigla_uf"))

# 5. Mapa % votou sim p Estado ----
tmap_options(check.and.fix = TRUE) #corrigir algum erro


(mapa_uf <- base.3 |>
    tm_shape() +
    tm_fill(
        "percentual",
        palette = "RdYlBu",
        style = "fixed",
        breaks = seq(10,100,20),
        legend.format = list(
            digits = 0,
            text.separator = "├" ,
            decimal.mark = ",",
            big.mark = ".",
            suffix = "%"
        ),
        legend.show = T,
        title = ""
    ) +
    tm_borders("black",
               lwd = .5,
               alpha = .5,
               lty = 3) +
    tm_shape(sh_estados) +
    tm_text("abbrev_state", size = .5, fontface = "bold") +
    tm_borders("black", lwd = .5, alpha = .5) +
    tm_layout(
        panel.label.bg.color = "NA",
        frame = FALSE,
        bg.color = "transparent",
        asp = 0,
        legend.position = c("right", "bottom")
    )
)


# Salvar
tmap_save(
    mapa_uf,
    filename = here::here("figures", "dados",
                          "mapas", "mapa_uf.png"),
    width = 8,
    height = 6,
    dpi = 600,
    asp = 0
)


# 6. Merge percentual finaciado PJ e shapes ----
base.4 <- sh_estados |>
    left_join(base |>
                  dplyr::select(deputado_id, per_pj, deputado_sigla_uf) |>
                  group_by(deputado_sigla_uf) |>
                  summarise(media = mean(per_pj),
                            mediana = median(per_pj)),
              by = c("abbrev_state" = "deputado_sigla_uf"))


# 7. Mapa Media PJ --------------------------------------------------------------

(mapa_pj <- base.4 |>
     tm_shape() +
     tm_fill(
         "media",
         palette = "RdYlBu",
         style = "fixed",
         breaks = seq(10,100,20),
         legend.format = list(
             digits = 0,
             text.separator = "├" ,
             decimal.mark = ",",
             big.mark = ".",
             suffix = "%"
         ),
         legend.show = T,
         title = ""
     ) +
     tm_borders("black",
                lwd = .5,
                alpha = .5,
                lty = 3) +
     tm_shape(sh_estados) +
     tm_text("abbrev_state", size = .5, fontface = "bold") +
     tm_borders("black", lwd = .5, alpha = .5) +
     tm_layout(
         panel.label.bg.color = "NA",
         frame = FALSE,
         bg.color = "transparent",
         asp = 0,
         legend.position = c("right", "bottom")
     )
)


# Salvar
tmap_save(
    mapa_pj,
    filename = here::here("figures", "dados",
                          "mapas", "mapa_pj.png"),
    width = 8,
    height = 6,
    dpi = 600,
    asp = 0
)


# 8. Mapa Media PJ --------------------------------------------------------------

(mapa_mediana_pj <- base.4 |>
     tm_shape() +
     tm_fill(
         "mediana",
         palette = "RdYlBu",
         style = "fixed",
         breaks = seq(0,100,20),
         legend.format = list(
             digits = 0,
             text.separator = "├" ,
             decimal.mark = ",",
             big.mark = ".",
             suffix = "%"
         ),
         legend.show = T,
         title = ""
     ) +
     tm_borders("black",
                lwd = .5,
                alpha = .5,
                lty = 3) +
     tm_shape(sh_estados) +
     tm_text("abbrev_state", size = .5, fontface = "bold") +
     tm_borders("black", lwd = .5, alpha = .5) +
     tm_layout(
         panel.label.bg.color = "NA",
         frame = FALSE,
         bg.color = "transparent",
         asp = 0,
         legend.position = c("right", "bottom")
     )
)


# Salvar
tmap_save(
    mapa_mediana_pj,
    filename = here::here("figures", "dados",
                          "mapas", "mapa_mediana_pj.png"),
    width = 8,
    height = 6,
    dpi = 600,
    asp = 0
)
