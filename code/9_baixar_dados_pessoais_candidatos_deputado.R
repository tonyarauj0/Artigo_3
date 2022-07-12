# Baixar dados pessoais dos candidatos a deputado

# 1 Baixar dados através do pacote basedosdados ----
# 1.1 id de projeto no Big Query
basedosdados::set_billing_id("projeto-base-dados")

# 1.2. Montar query
query <- "SELECT * FROM `basedosdados.br_tse_eleicoes.candidatos`
WHERE ano = 2014 AND cargo = 'deputado federal'
"

# 1.3 Atribuindo a um data frame
dados_pessoais_cand_deputado <- basedosdados::read_sql(query)


# Salvar ------------------------------------------------------------------
saveRDS(dados_pessoais_cand_deputado,
        file = here::here("data", "raw", "dados_pessoais.rds"),
        compress = T)
