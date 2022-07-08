# Baixar dados sobre patrimonio dos candidatos

# 1 Baixar dados através do pacote basedosdados ----
# 1.1 id de projeto no Big Query
basedosdados::set_billing_id("projeto-base-dados")

# 1.2. Montar query
query <- "SELECT * FROM `basedosdados.br_tse_eleicoes.bens_candidato`
WHERE ano = 2014"

# 1.3 Atribuindo a um data frame
patrimonio_dep_fed <- basedosdados::read_sql(query)


# Salvar ------------------------------------------------------------------
saveRDS(patrimonio_dep_fed,
        file = here::here("data", "clean", "patrimonio.rds"),
        compress = T)

# Pacotes -----------------------------------------------------------------
usethis::use_package("here")
