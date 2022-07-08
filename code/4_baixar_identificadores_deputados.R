# Baixar caracteristicas dos deputados do API da camara

# pacotes
pacman::p_load(httr, jsonlite, tibble, tidyverse)

# 1. Parametros basicos do  API -----
# 1.1 Base da API
base <- "https://dadosabertos.camara.leg.br/api/v2/deputados/"

# 1.2 Id do Deputado
# 1.2.1 Carregar os deputados da legislatura (Base advém do artigo 2)
load(file = here::here("data", "raw", "votacoes_nominais_camara.Rda"))

# 1.2.2 Selecinar ids unicos
ids <- df_votacoes_nominais$deputado_id |> unique()


# 2. Montar a chamada à API ----
chamada_api = c()
for (i in ids) {
    chamada_api <- append(chamada_api ,
                          paste(base,
                                i,
                                sep = ""))

}

# 3. Criar uma funcao para requisitar os dados ----
chamada = function(x){
    GET(x)
}

# 4. Fazer a requisicao para todos os anos
dados_api <- lapply(chamada_api,chamada)

# 5. Criar uma funcao para extrair os dados.
extracao = function(x){
    content(x, as="text" , encoding="UTF-8")
}

# 6. Fazer a extração do conteúdo
dados_api_txt <- lapply(dados_api , extracao )

# 7. Transformar os dados de txt para Json.
json = function(x){
    fromJSON (x,
              # simplifyVector = TRUE,
              # simplifyDataFrame = TRUE,
              # simplifyMatrix = TRUE,
              flatten = TRUE)
}

dados_api_json <- lapply (dados_api_txt , json)

# 8. Extrair os dados para um data frame (colunas selecionadas)
extrair_dados = function(x){
    tibble::as_tibble (x[["dados"]][c("id", "cpf", "nomeCivil")])
}

dados_api_extracao <-  lapply(dados_api_json , extrair_dados)


# 9. Juntar elementos da lista em um data frame
id_cpf_dep <- dplyr::bind_rows(dados_api_extracao)

# Salvar ------------------------------------------------------------------
saveRDS(id_cpf_dep,
        file = here::here("data", "raw", "identificador_deputados.rds"),
        compress = T)

# Pacotes -----------------------------------------------------------------
usethis::use_package("httr")
usethis::use_package("jsonlite")
usethis::use_package("tibble")
usethis::use_package("dplyr")


