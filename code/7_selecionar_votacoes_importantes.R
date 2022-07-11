# Selecionar votacoes da PEC 182 ligadas ao financiamento de campanha


# 1. Carregar os arquivos com todas votacoes de 2015 ----------------------

votacoes_15 <- readRDS(file = here::here("data", "raw", "votacoes_2015.rds"))


# 2 Selecionar PEC 182 ----------------------------------------------------

votacoes_pec_182 <- votacoes_15 |>
    dplyr::filter(type_bill == "PEC",
                  number_bill == 182)

# 3. Selecionar votacoes ligadas ao financiamento de campanha ----------------------
# 3.1 Emendas Aglutinativas que discorrem sobre o financiamento
#obs: ver planilha, demais emas foram prejudicadas.
emas <- paste0("EMENDA AGLUTINATIVA Nº ", c(10, 22, 32))

# 3.2 Filtrar emas selecionadas e votacao em segundo turno
votacoes_financiamento_pec182 <- votacoes_pec_182 |>
    dplyr::filter(rollcall_subject %in% emas |
                  rollcall_subject == "§ 5º DO ART. 17 DA C.F., CONSTANTE DO ART. 1º DA PEC") |>
    dplyr::select(rollcall_id, decision_summary, decision_date,
                  decision_time, rollcall_subject, session_id) |>
    dplyr::distinct() |>
    dplyr::mutate(decision_date = lubridate::dmy(decision_date),
                  decision_time = lubridate::hm(decision_time))


# 4. Selecionar votacoes destacadas em 3 na base de votacoes nomin --------
# 4.1 Carregar os dados
load(file = here::here("data", "raw", "votacoes_nominais_camara.Rda"))

# 4.2 Criar variaveis de data, e hora.
df_votacoes_nominais.1 <- df_votacoes_nominais |>
    tidyr::separate(data_hora_voto,
                    into = c("data","hora"),
                    sep = "T") |>
    dplyr::mutate(data = lubridate::as_date(data),
                  hora = lubridate::hms(hora))

# 4.3 Filtrar votacoes na mesma data que as votacoes de financiamento
df_votacoes_nominais.2 <- df_votacoes_nominais.1 |>
    dplyr::filter(data %in% votacoes_financiamento_pec182$decision_date)


# 4.4 Filtrar votacoes na mesma hora que as votacoes de financiamnto
df_votacoes_nominais.3 <- df_votacoes_nominais.2 |>
    dplyr::mutate(
        objeto = dplyr::case_when(
            data == lubridate::as_date("2015-05-26") &
                hora > lubridate::hms("23:53:0")
            ~ "EMA22",
            data == lubridate::as_date ("2015-05-27") &
                hora > lubridate::hms("19:21:0") &
                hora < lubridate::hms("20:08:0")

        ~ "EMA10",
        data == lubridate::as_date ("2015-05-27") &
            hora > lubridate::hms("20:09:0") &
            hora < lubridate::hms("21:08:0")

~ "EMA32",
data == lubridate::as_date("2015-08-12") &
    hora > lubridate::hms("21:37:0")
~ "2TURNO",
TRUE ~ NA_character_
)
) |> dplyr::filter(!is.na(objeto))



# 5. Salvar ---------------------------------------------------------------
saveRDS(df_votacoes_nominais.3,
        file = here::here("data", "clean", "votacoes_financiamento.rds"),
        compress = F)

# Pacotes -----------------------------------------------------------------
usethis::use_package("lubridate")

# OBS: Outra forma de buscar temas e proposicoes----


# Filtrar objetos referenes as votacoes pretendidas.
# Cada votação é uma decisão sobre uma e somente uma Proposição, que no Dados Abertos é o chamado objeto da votação. Muitas vezes, porém, a proposição que realmente é votada não é identificada, e em alguns casos não é nem mesmo cadastrada.
# Nestes arquivos, separados por ano de ocorrência das votações, cada linha/registro traz os identificadores de uma votação e de uma Proposição cadastrada que, pelos dados disponíveis, pode ter sido o objeto dessa votação. Para cada votação, podem existir várias proposições listadas como "possível objeto", e, principalmente nas votações do Plenário, a proposição realmente votada pode não ser qualquer uma delas.


# Baixar dados brutos dos objetos
pagina_objetos <- "http://dadosabertos.camara.leg.br/arquivos/votacoesObjetos/csv/votacoesObjetos-2015.csv"

download.file(pagina_objetos,
              destfile = here::here ("data", "raw", "votacoes_objetos_15.csv"))

# Carregar os dados baixados
votacoes_objetos_15 <- readr::read_delim("data/raw/votacoes_objetos_15.csv",
                                  delim = ";",
                                  escape_double = FALSE,
                                  trim_ws = TRUE)

