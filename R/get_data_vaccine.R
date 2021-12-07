get_data_vaccine <- function(){

  banco_mun <- readr::read_csv("https://raw.githubusercontent.com/denis-or/episcrap/master/inst/base_vacina_mun.csv")

  banco_mun

}
