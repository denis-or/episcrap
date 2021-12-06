# corpo da requisicao
body <- list(
  size = 0,
  aggs = list(
    f1 = list(
      terms = list(
        field = "paciente_endereco_coIbgeMunicipio",
        size = 6000
      ),
      aggs = list(
        f2 = list(
          terms = list(
            field = "vacina_descricao_dose.keyword",
            size = 20
          )
        )
      )
    )
  )
)

# pegar a base
r <- httr::POST(url = Sys.getenv("url"),
                httr::authenticate(Sys.getenv("usuario"), Sys.getenv("senha")),
                body = body,
                encode = "json") |>
  httr::content(simplifyDataFrame = T)

# executar a primeira formatacao da base
banco_mun <- purrr::pluck(r,"aggregations","f1","buckets")|>
  tidyr::unnest(f2)|>
  tidyr::unnest(buckets, names_repair = "unique", keep_empty = T)|>
  dplyr::select(cod_mun = key...1, dose = key...5, n = doc_count...6)|>
  dplyr::arrange(cod_mun, dose)|>
  tidyr::pivot_wider(names_from = dose, values_from = n)|>
  janitor::clean_names()

# escrever a mensagem de commit
commit_message <-
  paste0("[GitHub Actions] Base atualizada em ", Sys.time() )

# salvar a mensagem de commit
writeLines(commit_message, "mensagem-comit.txt")

# exportar csv
write.csv(banco_mun, "inst/base_vacina_mun.csv")
