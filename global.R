
library(shiny)
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

