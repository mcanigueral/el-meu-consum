
# Parse items from DynamoDB -----------------------------------------------

parse_item <- function(item) {
  tibble(
    id = item$id$S,
    datetime = as_datetime(as.numeric(item$timestamp$N)/1000, tz = config$tzone) %>% floor_date("5 minutes"),
    map_df(item$data$M, ~ .x$N) %>% mutate_all(as.numeric)
  )
}


# Power-current conversion -----------------------------------------------

power_from_current <- function(current, n_phases) {
  ifelse(n_phases == 1, 240, sqrt(3)*400)*current
}


# Peak hours --------------------------------------------------------------

get_tariff <- function(data) {
  data %>%
    mutate(
      tariff = ifelse(
        wday(datetime, week_start = 1) > 5,
        'vall',
        ifelse(
          hour(datetime) < 8,
          'vall',
          ifelse(
            hour(datetime) < 10,
            'pla',
            ifelse(
              hour(datetime) < 14,
              'punta',
              ifelse(
                hour(datetime) < 18,
                'pla',
                ifelse(
                  hour(datetime) < 22,
                  'punta',
                  ifelse(
                    hour(datetime) < 24,
                    'pla',
                    NA
                  )
                )
              )
            )

          )
        )

      )
    )
}



# Power to Energy ---------------------------------------------------------

get_kWh_from_W <- function(power_df) {
  power_df %>%
    mutate(datetime = round_date(datetime, 'hour')) %>%
    group_by(datetime) %>%
    summarise(energy = round(sum(power/1000/12, na.rm=T), 2))
    # summarise(energy = mean(power, na.rm = T))
}


