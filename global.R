
library(shiny)
# library(shinyWidgets)
library(shinythemes)
library(shinydashboard)
library(dutils)
library(auth0)
library(highcharter)
library(dygraphs)
library(dplyr)
library(lubridate)
library(purrr)
library(tidyr)
library(waiter)
options(scipen=999) # To avoid scientific notation
source('support/server_utils.R')
source('support/ui_utils.R')
# shiny::runApp(port = 8080, launch.browser = TRUE)


a0_info <- auth0::auth0_info()

# Python configuration
config <- config::get(file = 'config.yml')
Sys.setenv(TZ=config$tzone)

# Python environment ------------------------------------------------------
reticulate::use_python(config$python_path, required = T) # Restart R session to change the python env
boto3 <- reticulate::import("boto3")

# Metadata ---------------------------------------------------------------
# download.file(url=config$metadata_url, destfile="metadata.xlsx")
users_metadata <- readxl::read_xlsx('metadata.xlsx')


# Database import --------------------------------------------------
sensors_dynamodb <- get_dynamodb_py(
  aws_access_key_id = config$dynamodb$access_key_id,
  aws_secret_access_key = config$dynamodb$secret_access_key,
  region_name = config$dynamodb$region_name
)


power_table <- get_dynamo_table_py(sensors_dynamodb, config$dynamodb$power_table_name)


# Highcharter global options ----------------------------------------------

hc_global <- getOption("highcharter.global")
hc_global$useUTC <- FALSE
hc_global$timezoneOffset <- 60
options(highcharter.global = hc_global)

# # Test to get data from specific user -------------------------------------
# rs <- query_timeseries_data_table_py(
#   power_table, 'id', '9CCE', 'timestamp',
#   today()-days(30), today()+days(1)
# ) %>%
#   mutate(
#     datetime = floor_date(as_datetime(timestamp/1000, tz = config$tzone), '5 minutes'),
#     map_dfr(data, ~ .x)
#   ) %>%
#   mutate(power = current*230) %>%
#   # power_from_current(n_phases = 1) %>%
#   select(datetime, power)
#
# rs %>%
#   df_to_ts() %>%
#   hchart(type = "area", name = "Potència (W)") %>%
#   hc_navigator(enabled = T) %>%
#   hc_rangeSelector(
#     buttons = list(
#       list(type = 'all', text = 'Tot', title = 'Tot'),
#       list(type = 'month', count = 1, text = '1m', title = '1 mes'),
#       list(type = 'day', count = 1, text = '1d', title = '1 dia'),
#       list(type = 'hour', count = 6, text = '6h', title = '6 hores'),
#       list(type = 'hour', count = 1, text = '1h', title = '1 hora')
#     ),
#     selected = 2
#   ) %>%
#   hc_exporting(enabled = T)
