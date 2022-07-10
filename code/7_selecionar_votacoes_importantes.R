# Selecionar votacoes da PEC 182 ligadas ao financiamento de campanha


# 1. Carregar os arquivos com todas votacoes de 2015 ----------------------

votacoes_15 <- readRDS(file = here::here("data", "raw", "votacoes_2015.rds"))


# 2 Selecionar PEC 182 ----------------------------------------------------

votacoes_pec_182 <- votacoes_15 |>
    dplyr::filter(type_bill == "PEC",
                  number_bill == 182)

# 3. Selecionar votacoes ligadas ao financiamento de campanha ----------------------
# 3.1 Emendas Aglutinativas que discorrem sobre o financiamento
# Ver planilha


