scrape_vaccine <- function(){


  # corpo da requisicao
  body <- list(
    size = 0,
    aggs = list(
      f1 = list(
        terms = list(
          field = "paciente_endereco_coIbgeMunicipio",
          size = 7000
        ),
        aggs = list(
          f2 = list(
            terms = list(
              field = "vacina_descricao_dose", #retirei o ponto keyword
              size = 30
            )
          )
        )
      )
    )

  )

  # pegar a base
  r <- httr::POST(
    url = Sys.getenv("SCRAP_URL"),
    httr::authenticate(user = Sys.getenv("SCRAP_USUARIO"),
                       password = Sys.getenv("SCRAP_SENHA")),
    body = body,
    httr::config(connecttimeout = 60),
    encode = "json") |>
    httr::content(simplifyDataFrame = T)

  # executar a primeira formatacao da base
  banco_mun <- purrr::pluck(r, "aggregations", "f1", "buckets") |>
    dplyr::rename("cod_mun" = "key", "n_total" = "doc_count") |>
    tidyr::unnest(.data$f2) |>
    tidyr::unnest(.data$buckets, names_repair = "unique", keep_empty = T) |>
    dplyr::rename("dose" = "key", "n" = "doc_count") |>
    dplyr::select(.data$cod_mun, .data$dose, .data$n) |>
    dplyr::arrange(.data$cod_mun, .data$dose) |>
    tidyr::pivot_wider(names_from = .data$dose, values_from = .data$n) |>
    janitor::clean_names()

  banco_mun <- banco_mun |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dose1 = sum(
        x1a_dose_revacinacao,
        x1a_dose,
        dose,
        dose_inicial,
        tratamento_com_uma_dose,
        na.rm = T
      ),
      dose2 = sum(
        x2a_dose,
        x2a_dose_revacinacao,
        unica,
        na.rm = T
      ),

      reforco_ = sum(
        x3a_dose,
        x4a_dose,
        x1o_reforco,
        dose_adicional,
        reforco,
        x2o_reforco,
        x3a_dose_revacinacao,
        x3o_reforco,
        revacinacao,
        tratamento_com_dezessete_doses,
        na.rm = T
      )
    ) |>
    dplyr::select(cod_mun, dose1, dose2, reforco_) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~tidyr::replace_na(., 0)))

  banco_mun

}


get_data_vaccine <- function(){

  # banco_mun <- readr::read_csv("https://raw.githubusercontent.com/denis-or/episcrap/master/inst/base_vacina_mun.csv")
  banco_mun <- episcrap::scrape_vaccine()

  banco_mun

}
