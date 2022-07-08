# Baixar dados sobre votacao dos candidatos

# 1 Baixar dados através do pacote basedosdados ----
# 1.1 id de projeto no Big Query
basedosdados::set_billing_id("projeto-base-dados")

# 1.2. Montar query
query <- "SELECT * FROM `basedosdados.br_tse_eleicoes.resultados_candidato`
WHERE ano = 2014 AND cargo = 'deputado federal' "

# 1.3 Atribuindo a um data frame
votacao_candidatos_deputado_federal <- basedosdados::read_sql(query)


# Salvar ------------------------------------------------------------------
saveRDS(votacao_candidatos_deputado_federal,
        file = here::here("data", "raw", "votacao_deputado.rds"),
        compress = T)


