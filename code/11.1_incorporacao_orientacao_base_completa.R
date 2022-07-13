# Incluir orientacao na base de estimacao


# 1 Carregar base de estimacao --------------------------------------------

base <- readRDS(file = here::here("data", "clean", "base_estimacao_completa.rds"))


# 2 Carregar base de orientacao dos partidos ------------------------------

orientacao <- readRDS(file = here::here("data", "clean", "orientacao_partido.rds"))


# 3 Merge da base geral com as orientacoes -----------------------------------

base.1 <- base |>
    dplyr::left_join(orientacao,
                     by = c("deputado_sigla_partido" = "sigla"))


# 4 Criar variavel de orientacao contraria do partido --------------------------------
base.1 <- base.1 |>
    dplyr::mutate(or_contra_ema22 =
                      dplyr::case_when(
                          orientacao_ema22 == "Não" ~ 1,
                          TRUE ~ 0
                      ),
                  or_contra_ema32 =
                      dplyr::case_when(
                          orientacao_ema32 == "Não" ~ 1,
                          TRUE ~ 0),
                  or_contra_ema10 =
                      dplyr::case_when(
                          orientacao_ema10 == "Não" ~ 1,
                          TRUE ~ 0),
                  or_contra_x2turno =
                      dplyr::case_when(
                          orientacao_x2turno == "Não" ~ 1,
                          TRUE ~ 0)
    )


# salvar ------------------------------------------------------------------

saveRDS(base.1,
    file = here::here("data", "clean", "base_estimacao_completa_1.rds"))
