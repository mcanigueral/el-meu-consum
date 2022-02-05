auth0_ui(fluidPage(

  useShinydashboard(),
  theme = shinytheme("darkly"),
  use_waiter(),
  # # This removes the "code=XXX" of the URL after login, so avoids the error after refreshing
  tags$script(JS("setTimeout(function(){history.pushState({}, 'Page Title', '/');}, 2000);")),

  # Application title
  titlePanel("El meu consum elèctric"),
  hr(),

  # Menu
  uiOutput("menu"),

  # Body
  fluidRow(
    infoBoxOutput('power_now'),
    infoBoxOutput('energy_today'),
    infoBoxOutput('week_total')
  ),
  hr(),
  fluidRow(
    highchartOutput('plot_timeseries'),
    highchartOutput('plot_columns')
  ),
  hr(),
  uiOutput('month_demand'),
  hr(),

  # Download data
  downloadButton("download", "Descarrega't les dades (Excel)"),
  hr()
), info = a0_info)
