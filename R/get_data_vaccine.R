scrape_vaccine <- function(){


  # seção pega dados feminino -----------------------------------------------



  body <- list(
    size = 0,
    query = list(
      match = list(
        "paciente_enumSexoBiologico" = "F"
      )
    ),
    aggs = list(
      f1 = list(
        terms = list(
          field = "paciente_endereco_coIbgeMunicipio",
          size = 7000
        ),
        aggs = list(
          f2 = list(
            terms = list(
              field = "vacina_descricao_dose",
              size = 30
            )
          )
        )
      )
    )
  )

  r <- httr::POST(
    url = Sys.getenv("SCRAP_URL"),
    httr::authenticate(user = Sys.getenv("SCRAP_USUARIO"),
                       password = Sys.getenv("SCRAP_SENHA")),
    body = body,
    httr::config(connecttimeout = 60),
    encode = "json") |>
    httr::content(simplifyDataFrame = T)

  bd2 <- purrr::pluck(r, "aggregations", "f1", "buckets") |>
    dplyr::rename("cod_mun" = "key", "n_total" = "doc_count") |>
    tidyr::unnest(.data$f2) |>
    tidyr::unnest(.data$buckets, names_repair = "unique", keep_empty = T) |>
    dplyr::rename("dose" = "key", "n" = "doc_count") |>
    dplyr::select(.data$cod_mun, .data$dose, .data$n) |>
    dplyr::arrange(.data$cod_mun, .data$dose) |>
    tidyr::pivot_wider(names_from = .data$dose, values_from = .data$n) |>
    janitor::clean_names() |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dose1_F = sum(
        x1a_dose_revacinacao,
        x1a_dose,
        dose,
        dose_inicial,
        tratamento_com_uma_dose,
        na.rm = T
      ),
      dose2_F = sum(
        x2a_dose,
        x2a_dose_revacinacao,
        unica,
        na.rm = T
      ),

      reforco_F = sum(
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
    dplyr::select(cod_mun, dose1_F, dose2_F, reforco_F) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~tidyr::replace_na(., 0)))


  # Pega dados masculino ----------------------------------------------------



  body <- list(
    size = 0,
    query = list(
      match = list(
        "paciente_enumSexoBiologico" = "M"
      )
    ),
    aggs = list(
      f1 = list(
        terms = list(
          field = "paciente_endereco_coIbgeMunicipio",
          size = 7000
        ),
        aggs = list(
          f2 = list(
            terms = list(
              field = "vacina_descricao_dose",
              size = 30
            )
          )
        )
      )
    )
  )

  r <- httr::POST(
    url = Sys.getenv("SCRAP_URL"),
    httr::authenticate(user = Sys.getenv("SCRAP_USUARIO"),
                       password = Sys.getenv("SCRAP_SENHA")),
    body = body,
    httr::config(connecttimeout = 60),
    encode = "json") |>
    httr::content(simplifyDataFrame = T)

  bd3 <- purrr::pluck(r, "aggregations", "f1", "buckets") |>
    dplyr::rename("cod_mun" = "key", "n_total" = "doc_count") |>
    tidyr::unnest(.data$f2) |>
    tidyr::unnest(.data$buckets, names_repair = "unique", keep_empty = T) |>
    dplyr::rename("dose" = "key", "n" = "doc_count") |>
    dplyr::select(.data$cod_mun, .data$dose, .data$n) |>
    dplyr::arrange(.data$cod_mun, .data$dose) |>
    tidyr::pivot_wider(names_from = .data$dose, values_from = .data$n) |>
    janitor::clean_names() |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dose1_M = sum(
        x1a_dose_revacinacao,
        x1a_dose,
        dose,
        dose_inicial,
        tratamento_com_uma_dose,
        na.rm = T
      ),
      dose2_M = sum(
        x2a_dose,
        x2a_dose_revacinacao,
        unica,
        na.rm = T
      ),

      reforco_M = sum(
        x3a_dose,
        x4a_dose,
        x1o_reforco,
        dose_adicional,
        reforco,
        x2o_reforco,
        x3a_dose_revacinacao,
        x3o_reforco,
        revacinacao,
        # tratamento_com_dezessete_doses,
        na.rm = T
      )
    ) |>
    dplyr::select(cod_mun, dose1_M, dose2_M, reforco_M) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~tidyr::replace_na(., 0)))



  # Pega dados ignorado -----------------------------------------------------



  body <- list(
    size = 0,
    query = list(
      match = list(
        "paciente_enumSexoBiologico" = "I"
      )
    ),
    aggs = list(
      f1 = list(
        terms = list(
          field = "paciente_endereco_coIbgeMunicipio",
          size = 7000
        ),
        aggs = list(
          f2 = list(
            terms = list(
              field = "vacina_descricao_dose",
              size = 30
            )
          )
        )
      )
    )
  )

  r <- httr::POST(
    url = Sys.getenv("SCRAP_URL"),
    httr::authenticate(user = Sys.getenv("SCRAP_USUARIO"),
                       password = Sys.getenv("SCRAP_SENHA")),
    body = body,
    httr::config(connecttimeout = 60),
    encode = "json") |>
    httr::content(simplifyDataFrame = T)

  bd4 <- purrr::pluck(r, "aggregations", "f1", "buckets") |>
    dplyr::rename("cod_mun" = "key", "n_total" = "doc_count") |>
    tidyr::unnest(.data$f2) |>
    tidyr::unnest(.data$buckets, names_repair = "unique", keep_empty = T) |>
    dplyr::rename("dose" = "key", "n" = "doc_count") |>
    dplyr::select(.data$cod_mun, .data$dose, .data$n) |>
    dplyr::arrange(.data$cod_mun, .data$dose) |>
    tidyr::pivot_wider(names_from = .data$dose, values_from = .data$n) |>
    janitor::clean_names() |>
    dplyr::rowwise() |>
    dplyr::mutate(
      dose1_I = sum(
        # x1a_dose_revacinacao,
        x1a_dose,
        dose,
        # dose_inicial,
        # tratamento_com_uma_dose,
        na.rm = T
      ),
      dose2_I = sum(
        x2a_dose,
        # x2a_dose_revacinacao,
        unica,
        na.rm = T
      ),

      reforco_I = sum(
        x3a_dose,
        x4a_dose,
        # x1o_reforco,
        dose_adicional,
        reforco,
        # x2o_reforco,
        # x3a_dose_revacinacao,
        # x3o_reforco,
        # revacinacao,
        # tratamento_com_dezessete_doses,
        na.rm = T
      )
    ) |>
    dplyr::select(cod_mun, dose1_I, dose2_I, reforco_I) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~tidyr::replace_na(., 0)))

  # Pega dados todos --------------------------------------------------------


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
              field = "paciente_enumSexoBiologico"
            )
          )
        )
      )
    )
  )

  r <- httr::POST(
    url = Sys.getenv("SCRAP_URL"),
    httr::authenticate(user = Sys.getenv("SCRAP_USUARIO"),
                       password = Sys.getenv("SCRAP_SENHA")),
    body = body,
    httr::config(connecttimeout = 60),
    encode = "json") |>
    httr::content(simplifyDataFrame = T)

  bd1 <- purrr::pluck(r, "aggregations", "f1", "buckets") |>
    tidyr::unnest(.data$f2) |>
    dplyr::select(cod_mun = key, tot_vacinado = doc_count, buckets) |>
    tidyr::unnest(.data$buckets,
                  names_repair = "unique",
                  keep_empty = T) |>
    dplyr::select(cod_mun,
                  tot_vacinado,
                  sexo = key,
                  tot_vac_sexo = doc_count) |>
    tidyr::pivot_wider(names_from = sexo, values_from = tot_vac_sexo) |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(., 0)))



  # Juntar todos ------------------------------------------------------------

  bd_final <- bd1 |>
    dplyr::left_join(bd2, by = "cod_mun") |>
    dplyr::left_join(bd3, by = "cod_mun") |>
    dplyr::left_join(bd4, by = "cod_mun") |>
    dplyr::mutate(dplyr::across(where(is.numeric), ~ tidyr::replace_na(., 0))) |>
    dplyr::arrange(.data$cod_mun)

  bd_final

}


get_data_vaccine <- function(){

  banco_mun <- episcrap::scrape_vaccine()

  banco_mun

}
