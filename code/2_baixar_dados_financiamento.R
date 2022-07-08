# Baixar dados do financiamento de campanha

# Pacotes

install.packages("pacman")
pacman::p_load(tidyverse, basedosdados)

# 1 Baixar dados através do pacote basedosdados ----
# 1.1 id de projeto no Big Query
basedosdados::set_billing_id("projeto-base-dados")

# 1.2. Montar query
query <- "SELECT *
FROM `basedosdados.br_tse_eleicoes.receitas_candidato`
WHERE ano = 2014
AND cargo = 'deputado federal'"

# 1.3 Atribuindo a um data frame
fc_dep_fed_14 <- basedosdados::read_sql(query)

usethis::use_package("basedosdados")
# Salvar ------------------------------------------------------------------
saveRDS(fc_dep_fed_14,
        file = here::here("data", "clean", "financiamento.rds"),
        compress = T)

# Pacotes -----------------------------------------------------------------
usethis::use_package("basedosdados")
usethis::use_package("pacman")
