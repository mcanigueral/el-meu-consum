
source('global.R')

start_date <- dmy(01022022) # today() - days(1)
end_date <- today() - days(-1)

# Query for one user ------------------------------------------------------

user_demand <- query_timeseries_data_table_py(
  power_table, 'id', 'F814', 'timestamp',
  start_date, end_date
) %>%
  mutate(
    datetime = floor_date(as_datetime(timestamp/1000, tz = config$tzone), '5 minutes'),
    map_dfr(data, ~ .x)
  ) %>%
  mutate(power = power_from_current(current, n_phases = 1)) %>%
  select(datetime, id, power)


# Query from all users in the metadata file -------------------------------
users_demand <- map_dfr(
  users_metadata$id_power %>% set_names,
  ~ query_timeseries_data_table_py(
      power_table, 'id', .x, 'timestamp',
      start_date, end_date
    ),
  .id = 'id'
) %>%
  mutate(
    datetime = floor_date(as_datetime(timestamp/1000, tz = config$tzone), '5 minutes'),
    map_dfr(data, ~ .x)
  ) %>%
  mutate(power = power_from_current(current, n_phases = 1)) %>%
  select(datetime, id, power)


users_demand %>%
  mutate(datetime = datetime_to_timestamp(datetime)) %>%
  hchart(hcaes(x = datetime, y = power, group = id),  type = 'line') %>%
  hc_xAxis(type = 'datetime')

users_demand %>% writexl::write_xlsx('consum_de_tots.xlsx')
