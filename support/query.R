
# source('global.R')
#
#
# # Query from all users in the metadata file -------------------------------
# start_date <- today() - days(1)
# end_date <- start_date + days(2)
#
# users_demand <- map_dfr(
#   users_metadata$id_power %>% set_names,
#   ~ query_timeseries_data_table_py(
#       power_table, 'id', .x, 'timestamp',
#       start_date, end_date
#     ),
#   .id = 'id'
# ) %>%
#   mutate(
#     datetime = floor_date(as_datetime(timestamp/1000, tz = config$tzone), '5 minutes'),
#     map_dfr(data, ~ .x)
#   ) %>%
#   mutate(power = power_from_current(current, n_phases = 1)) %>%
#   select(datetime, id, power)
#
#
# users_demand %>%
#   mutate(datetime = datetime_to_timestamp(datetime)) %>%
#   hchart(hcaes(x = datetime, y = power, group = id),  type = 'line') %>%
#   hc_xAxis(type = 'datetime')
