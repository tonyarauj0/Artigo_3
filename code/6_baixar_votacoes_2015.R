# Buscar votacoes sobre financiamento de campanha

# Pacote
devtools::install_github("RobertMyles/congressbr")


# Baixar as votacoes em 2015 ---------------------------------------
votacoes_nominais_2015 <- congressbr::cham_votes_year(2015)

# Salvar ------------------------------------------------------------------
saveRDS(votacoes_nominais_2015,
        file = here::here("data", "raw", "votacoes_2015.rds"),
        compress = T)

# Pacotes -----------------------------------------------------------------
usethis::use_package("usethis")
usethis::use_dev_package("congressbr")
