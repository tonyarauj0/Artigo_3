# Filtrar deputados de interesse e casar com demais bases


# 1 Carregar as votacoes nominais ligadas ao financiamento de campanha --------

votacoes_nominais_financiamento <- readRDS( file = here::here("data", "clean", "votacoes_financiamento.rds"))

# 2 Carregar ids e cpfs dos Deputados -----
id_cpf_dep <- readRDS(file = here::here("data", "raw", "identificador_deputados.rds"))


# 3. Merge id-cpf para casar com demais bases -------------------------------

base <- votacoes_nominais_financiamento |>
    dplyr::select(- c(deputado_uri_partido, deputado_uri, deputado_id_legislatura, deputado_url_foto)) |>
    dplyr::left_join(id_cpf_dep, by = c("deputado_id" = "id"))


# Salvar ------------------------------------------------------------------

saveRDS(base,
        file = here::here("data", "clean", "merge_votos_cpfs_deputados"),
        compress = F)
