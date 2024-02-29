library(dplyr)
library(readr)
library(tidyr)
library(stringr)

collect_income_data <- function() {
  wiid <- read_delim("~/Downloads/wiid-data.csv", delim = ";")

  wiid_c <-
    wiid %>%
    rename_all(~ tolower(.x) %>% str_replace_all("\\(.+\\)", "")) %>%
    pivot_longer(-c(iso, country, year))

  ppp <- read_csv("ppp_2017.csv")
  # https://www.xe.com/currencytables/?from=EUR&date=2017-05-01#table-section
  euro_exchange <- read_csv("euro_currency_exchange.csv")
  country_currency <- read_csv("country_currency.csv")

  excluded_countries <-
    country_currency %>%
    count(country) %>%
    filter(n > 1) %>%
    pull(country)

  excluded_countries <- c(
    excluded_countries,
    # countries with no currency value in database
    c(
      "Mauritania", "São Tomé and Príncipe", "Sierra Leone", "Somaliland",
      "South Sudan", "Transnistria"
    )
  )

  country_currency <-
    country_currency %>%
    filter(!country %in% excluded_countries)

  currency_exchange <-
    select(country_currency, currency_code, country) %>%
    left_join(euro_exchange, by = c("currency_code" = "currency")) %>%
    select(country, eur_per_units)

  all_income <-
    wiid_c %>%
    left_join(ppp, by = c("country")) %>%
    left_join(currency_exchange, by = c("country")) %>%
    select(country, name, value, Ppp_2017, eur_per_units) %>%
    mutate(
      income_year = value * Ppp_2017 * eur_per_units,
      income_month = income_year / 12
    ) %>%
    select(country, name, income_year, income_month) %>%
    mutate(name = trimws(name)) %>%
    drop_na()

  all_income
}

# Modified function to process the request
process_income_data <- function(country_name, income_number) {
  all_income <- collect_income_data()
  data_country <- all_income %>% filter(country == country_name)

  if (nrow(data_country) == 0) {
    return(list(below = NA, above = NA, found = FALSE))
  }

  below_income <- sum(data_country$income_month < income_number) / nrow(data_country) * 100
  above_income <- sum(data_country$income_month >= income_number) / nrow(data_country) * 100
  income_position <- which.min(abs(data_country$income_month - income_number))
  return(list(below = below_income, above = above_income, position = income_position, found = TRUE))
}
