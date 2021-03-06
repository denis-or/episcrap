# Carregar pacote
pkgload::load_all()

banco_mun <- episcrap::get_data_vaccine()

# exportar csv
write.csv(banco_mun, "inst/base_vacina_mun.csv")

# escrever a mensagem de commit
commit_message <-
  paste0("episcrap vaccine update on ", Sys.time())

# salvar a mensagem de commit
writeLines(commit_message, "mensagem-comit.txt")
