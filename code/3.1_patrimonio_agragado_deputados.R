# Criar patrimonio total por deputado de interesse


# 1. Carregar dados de patrimonio -----------------------------------------

patrimonio <- readRDS(file = here::here("data","raw","patrimonio.rds"))


# 2 Somar valores por deputado --------------------------------------------

patrimonio_total <- patrimonio |>
    dplyr::group_by(id_candidato_bd) |>
    dplyr::summarise(patrimonio = sum(valor_item)) |>
    dplyr::ungroup()

# 3. salvar ---------------------------------------------------------------

saveRDS(patrimonio_total, file = here::here("data", "clean","patrimonio_agragado_deputados.rds"))
