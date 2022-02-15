auth0_server(function(input, output, session) {

  user_metadata <- reactive({
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
      last_power_data <- readxl::read_excel(user_metadata()$filename) %>%
        mutate(datetime = with_tz(datetime, tz = config$tzone))
      last_date <- as_date(max(last_power_data$datetime))
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
      power_tbl <- bind_rows(last_power_data, new_power_data) %>%
        distinct()
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
      value = paste(round(power_data()$power[nrow(power_data())]), "W"),
      subtitle = paste("A les", strftime(power_data()$datetime[nrow(power_data())], format = "%H:%M")),
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
      subtitle = strftime(energy_today$datetime[1], format = "%d/%m/%Y"),
      icon = icon("lightbulb"),
      color = 'blue',
      width = 6,
      fill = T
    )
  })

  output$week_total <- renderInfoBox({
    week_data <- energy_data() %>%
      mutate(yweek = tsibble::yearweek(datetime)) %>%
      filter(yweek == tsibble::yearweek(energy_data()$datetime[nrow(energy_data())]))

    infoBox(
      title = "Consum setmanal",
      value = paste(sum(week_data$energy), "kWh"),
      subtitle = paste("Setmana del", strftime(week_data$yweek[nrow(week_data)], format = "%d/%m/%Y")),
      icon = icon("plug"),
      color = 'blue',
      width = 6,
      fill = T
    )
  })

  output$plot_timeseries <- renderHighchart({
    power_data() %>%
      df_to_ts() %>%
      hchart(type = "area", name = "Potència (W)") %>%
      hc_navigator(enabled = T) %>%
      hc_rangeSelector(
        buttons = list(
          list(type = 'all', text = 'Total', title = 'Totes les dades'),
          list(type = 'month', count = 1, text = '1m', title = '1 mes'),
          list(type = 'week', count = 1, text = '1w', title = '1 setmana'),
          list(type = 'day', count = 1, text = '1d', title = '1 dia'),
          list(type = 'hour', count = 6, text = '6h', title = '6 hores'),
          list(type = 'hour', count = 1, text = '1h', title = '1 hora')
        ),
        selected = 3
      ) %>%
      hc_exporting(enabled = T)
  })

  output$plot_columns <- renderHighchart({
    energy_data() %>%
      mutate(date = floor_date(datetime, unit = input$columns_unit, week_start = 1)) %>%
      group_by(date) %>%
      summarise(energy = sum(energy)) %>%
      df_to_ts() %>%
      hchart(type = "column", name = "Energia (kWh)") %>%
      hc_rangeSelector(enabled = F) %>%
      hc_exporting(enabled = T)
  })

  month_data <- reactive({
    month_data <- energy_data() %>%
      get_tariff() %>%
      group_by(yearmonth = tsibble::yearmonth(datetime), tariff) %>%
      summarise(energy = sum(energy))
    month_data <- month_data %>%
      filter(yearmonth == max(month_data$yearmonth)) %>%
      pivot_wider(names_from = 'tariff', values_from = 'energy')
    month_data
  })

  output$month_demand <- renderUI({
    consum_vall <- sum(month_data()$vall, na.rm = T)
    consum_pla <- sum(month_data()$pla, na.rm = T)
    consum_punta <- sum(month_data()$punta, na.rm = T)
    consum_total <- sum(consum_vall, consum_pla, consum_punta)

    cost_vall <- round(consum_vall*0.318605, 2)
    cost_pla <- round(consum_pla*0.389136, 2)
    cost_punta <- round(consum_punta*0.494932, 2)
    cost_total <- sum(cost_vall, cost_pla, cost_punta)

    wellPanel(
      h4("Aquest mes has consumit:"),
      fluidRow(
        infoBox(
          title = "Consum hores vall",
          value = paste0(round(consum_vall, 2), "kWh (", cost_vall, "€)"),
          subtitle = paste0(round(consum_vall/consum_total*100), "% del total del mes"),
          icon = icon("long-arrow-alt-down"),
          color = 'blue',
          width = 3,
          fill = T
        ),
        infoBox(
          title = "Consum hores pla",
          value = paste0(round(consum_pla, 2), "kWh (", cost_pla, "€)"),
          subtitle = paste0(round(consum_pla/consum_total*100), "% del total del mes"),
          icon = icon("arrows-alt-h"),
          color = 'blue',
          width = 3,
          fill = T
        ),
        infoBox(
          title = "Consum hores punta",
          value = paste0(round(consum_punta, 2), "kWh (", cost_punta, "€)"),
          subtitle = paste0(round(consum_punta/consum_total*100), "% del total del mes"),
          icon = icon("long-arrow-alt-up"),
          color = 'blue',
          width = 3,
          fill = T
        ),
        infoBox(
          title = "Consum total",
          value = paste0(round(consum_total, 2), "kWh (", round(cost_total, 2), "€)"),
          # subtitle = paste0(round(month_data()$punta/consum_total*100), "% del total del mes"),
          icon = icon("calendar"),
          color = 'blue',
          width = 3,
          fill = T
        )
      ),
      HTML('La tarifa elèctrica utilitzada és l\'actual de SomEnergia de tres períodes
      (<a href = "https://www.somenergia.coop/ca/tarifes-d-electricitat/#preus-20td-amb-impostos">Tarifa 2.0TD SOM</a>),
           considerant impostos i l\'IVA del 21%.')
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
