get_data_vaccine <- function(){

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
  dplyr::rename("cod_mun" = "key", "n_total" = "doc_count")|>
  tidyr::unnest(.data$f2)|>
  tidyr::unnest(buckets, names_repair = "unique", keep_empty = T)|>
  dplyr::rename("dose" = "key", "n" = "doc_count")|>
  dplyr::select(.data$cod_mun, .data$dose, .data$n)|>
  dplyr::arrange(.data$cod_mun, .data$dose)|>
  tidyr::pivot_wider(names_from = .data$dose, values_from = .data$n)|>
  janitor::clean_names()

banco_mun

}
