auth0_ui(fluidPage(
  shinyWidgets::useShinydashboard(),
  theme = shinytheme("darkly"),
  use_waiter(),
  # # This removes the "code=XXX" of the URL after login, so avoids the error after refreshing
  tags$script(JS("setTimeout(function(){history.pushState({}, 'Page Title', '/');}, 2000);")),

  # Application title
  titlePanel(tagList(
    # img(src = "udg_logo_short.png", height = 40),
    # HTML("&nbsp;"),
    span(strong("El meu consum")),
    span(
      logoutButton(
        "", icon = icon('sign-out-alt'),
        style = "border-radius: 20px;"
      ),
      style = "position:absolute;right:1em;"
    )
  ), windowTitle = "El meu consum"),
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
    column(
      12,
      radioButtons(
        'columns_unit', label = NULL, inline = T,
        choices = c("Diari" = "day", "Setmanal" = "week", "Mensual" = "month", "Anual" = "year")
      )
    ),
    highchartOutput('plot_columns')
  ),
  hr(),
  uiOutput('month_demand'),
  hr(),

  # Download data
  downloadButton("download", "Descarrega't les dades (Excel)"),
  hr()
), info = a0_info)
