# Criar estrutura do projeto

# 1. Criar estrutura de pacote ----
usethis::create_package(".", check_name = F)


# 2 Criar pastas  -----------------------------------------------------------
fs::dir_create("data")
fs::dir_create("data/raw")
fs::dir_create("data/clean")
fs::dir_create("code")
fs::dir_create("figures")
fs::dir_create("tables")
fs::dir_create("manuscripts")


usethis::use_package("fs")
