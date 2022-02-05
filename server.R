auth0_server(function(input, output, session) {

  user_metadata <- reactive({
    # users_metadata[1, ] %>% as.list()
    print(session$userData$auth0_info)
    user_data <- users_metadata %>%
      filter(mail == session$userData$auth0_info$name) %>%
      as.list()
    user_data[['filename']] <- paste0('db/', user_data$id_power, '.xlsx')
    return(user_data)
  })

  power_data <- reactive({
    req(user_metadata())
    waiter_show(html = waiting_screen("Consultant el consum elèctric..."), color = "#00000080")
    if (file.exists(user_metadata()$filename)) {
      last_power_data <- readxl::read_excel(user_metadata()$filename)
      last_date <- max(last_power_data$datetime)
    } else {
      last_power_data <- tibble()
      last_date <- dmy(01012022)
    }
    current_tbl <- query_timeseries_data_table_py(
      power_table, 'id', user_metadata()$id_power, 'timestamp', last_date, today()+days(1)
    )
    if (!is.null(current_tbl)) {
      new_power_data <- current_tbl %>%
        mutate(
          datetime = floor_date(as_datetime(timestamp/1000, tz = config$tzone), '5 minutes'),
          map_dfr(data, ~ .x),
          power = power_from_current(current, user_metadata()$phases)
        ) %>%
        select(datetime, power)
      power_tbl <- bind_rows(last_power_data, new_power_data)
      writexl::write_xlsx(power_tbl, user_metadata()$filename)
    } else {
      power_tbl <- tibble(datetime = today(), power = NA)
    }
    waiter_hide()
    return(power_tbl)
  })

  energy_data <- reactive({
    get_kWh_from_W(power_data())
  })

  output$power_now <- renderInfoBox({
    req(user_metadata())
    infoBox(
      title = 'Consum actual',
      value = paste(round(power_data()$power[nrow(power_data())], 2), "W"),
      subtitle = power_data()$datetime[nrow(power_data())],
      icon = icon("bolt"),
      color = 'blue',
      width = 6,
      fill = T
    )
  })

  output$energy_today <- renderInfoBox({
    energy_today <- energy_data() %>%
      filter(date(datetime) == today())

    infoBox(
      title = "Consum d'avui",
      value = paste(round(sum(energy_today$energy), 2), "kWh"),
      subtitle = energy_today$date,
      icon = icon("lightbulb"),
      color = 'blue',
      width = 6,
      fill = T
    )
  })

  output$week_total <- renderInfoBox({
    week_total <- energy_data() %>%
      filter(yearweek(datetime) == yearweek(today()))

    infoBox(
      title = "Consum setmanal",
      value = paste(sum(week_total$energy), "kWh"),
      subtitle = paste("Setmana del", get_week_from_datetime(week_total$datetime)[1]),
      icon = icon("plug"),
      color = 'blue',
      width = 6,
      fill = T
    )
  })

  output$plot_timeseries <- renderHighchart({
    power_data() %>%
      # dyplot(ylab = 'Potència (W)', group = "a") %>%
      # dySeries('power', 'Consum', 'navy', fillGraph = T) %>%
      # dyRangeSelector()
      df_to_ts() %>%
      hchart(type = "area", name = "Potència (W)") %>%
      hc_navigator(enabled = T)
  })

  output$plot_columns <- renderHighchart({
    energy_data() %>%
      group_by(date = date(datetime)) %>%
      summarise(energy = sum(energy)) %>%
      # dyplot(ylab = "Energia (kWh)", group = "b") %>%
      # dySeries('energy', 'Consum', 'navy') %>%
      # dyBarChart()
      df_to_ts() %>%
      hchart(type = "column", name = "Energia (kWh)")
  })

  month_data <- reactive({
    month_data <- energy_data() %>%
      get_tariff() %>%
      group_by(yearmonth = yearmonth(datetime), tariff) %>%
      summarise(energy = sum(energy))
    month_data <- month_data %>%
      filter(yearmonth == max(month_data$yearmonth)) %>%
      pivot_wider(names_from = 'tariff', values_from = 'energy')
    month_data
  })

  output$month_demand <- renderUI({
    consum_total_mes <- sum(month_data()$vall, month_data()$pla, month_data()$punta)
    wellPanel(
      h4("Aquest mes has consumit:"),
      fluidRow(
        infoBox(
          title = "Consum hores vall",
          value = paste(round(sum(month_data()$vall, na.rm = T), 2), "kWh"),
          subtitle = paste0(round(month_data()$vall/consum_total_mes*100), "% del total del mes"),
          icon = icon("long-arrow-alt-down"),
          color = 'blue',
          width = 4,
          fill = T
        ),
        infoBox(
          title = "Consum hores pla",
          value = paste(round(sum(month_data()$pla, na.rm = T), 2), "kWh"),
          subtitle = paste0(round(month_data()$pla/consum_total_mes*100), "% del total del mes"),
          icon = icon("arrows-alt-h"),
          color = 'blue',
          width = 4,
          fill = T
        ),
        infoBox(
          title = "Consum hores punta",
          value = paste(round(sum(month_data()$punta, na.rm = T), 2), "kWh"),
          subtitle = paste0(round(month_data()$punta/consum_total_mes*100), "% del total del mes"),
          icon = icon("long-arrow-alt-up"),
          color = 'blue',
          width = 4,
          fill = T
        )
      )
    )
  })


  output$download <- downloadHandler(
    filename = function() {
      paste0("consum_", today(), ".xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(power_data(), file)
    }
  )

}, info = a0_info)
