# filter module --------------------------------------------------------------------------------------------
md.filter_UI = function(id = 'filter'){
  ns = NS(id)

  tagList(
    p(h3('Filters'))
    ,wd.datepicker(id = ns('date'), label = 'Date')
    ,wd.pickerinput(id = ns('airline'), label = 'Airline', multiple = TRUE)
    ,br()
    ,actionButton(inputId = ns('apply'), label = 'Apply')

  )

}

md.filter = function(id = 'filter', rv){
  moduleServer(id, function(input, output, session) {

    observe({

      updatePickerInput(
        inputId = 'airline'
        ,session = session
        ,choices = rv$airlines$carrier
        ,selected = rv$airlines$carrier
      )

    })

    observe({

      if(!is.na(input$date[2])) {
        rv$filter.date = seq.Date(input$date[1], input$date[2], 1)
      } else {
        rv$filter.date = input$date[1]
      }

      rv$filter.airline = input$airline

    }) %>%
      bindEvent(input$apply)

  })
}

# dashboard module ----------------------------------------------------------------
md.dashboard_UI = function(id = 'dashboard'){
  ns = NS(id)

  page_fillable(

    layout_columns(
      col_widths = c(12,12)

      ,layout_columns(
        col_widths = c(3,4,5)

        ,tagList(
          uiOutput(outputId = ns('valuebox'))
          ,reactableOutput(outputId = ns('tbl.airline_names'))
        )

        ,card(
          card_header('Flights per month')
          ,echarts4rOutput(ns('plot.month_total'))
          ,full_screen = TRUE
        )

        ,card(
          card_header('Flights per airline')
          ,echarts4rOutput(ns('plot.airlines'))
          ,full_screen = TRUE
        )

      )

      ,layout_columns(
        col_widths = c(6,6)
        ,card(
          card_header('Arrival and departure delay means')
          ,reactableOutput(ns('tbl.delay_means'))
          ,full_screen = TRUE
          ,height = '75%'
        )
        ,card(
          card_header('Airport location')
          ,leafletOutput(ns('map.airport'))
          ,full_screen = TRUE
        )
      )

    )

  )
}

md.dashboard = function(id = 'dashboard', rv){
  moduleServer(id, function(input, output, session) {

    # reactives ---------------------------------------------------------------------
    rc.flights = reactive({
      rv$flights
    })

    rc.flights_filtered = reactive({
      req(rc.flights())
      rc.flights %>% filter_df(rv = rv)
    })

    # outputs -----------------------------------------------------------------------
    output$valuebox = renderUI({

      bslib::value_box(
        title = 'Total of flights'
        ,value = rc.flights_filtered() %>% nrow()
        ,showcase = bs_icon('airplane')
        ,theme = value_box_theme(bg = '#AFD4FE')
      )

    })

    output$tbl.airline_names = renderReactable({
      rv$airlines %>%
        reactable(filterable = FALSE, resizable = TRUE,compact = TRUE, searchable = FALSE, showPageSizeOptions = FALSE, pagination = FALSE)
    })

    output$plot.month_total = renderEcharts4r({

      validate(
        need(
          nrow(rc.flights_filtered()) > 0
          ,message = "No data to show."
        )
      )

      rc.flights_filtered() %>%
        plot.timeline(by = 'carrier')

    })

    output$plot.airlines = renderEcharts4r({

      validate(
        need(
          nrow(rc.flights_filtered()) > 0
          ,message = "No data to show."
        )
      )

      rc.flights_filtered() %>%
        plot.pie_radius(col = 'carrier')

    })

    output$tbl.delay_means = renderReactable({

      validate(
        need(
          nrow(rc.flights_filtered()) > 0
          ,message = "No data to show."
        )
      )

      rc.flights_filtered() %>%
        summarise(
          total = n()
          ,dep_delay_mean = dep_delay %>%  mean(na.rm = TRUE)
          ,arr_delay_mean = arr_delay %>% mean(na.rm = TRUE)
          ,.by = carrier
        ) %>%
        plot.reactable()
    })

    output$map.airport = renderLeaflet({
      rv$airports %>%
        leaflet() %>%
        addTiles() %>%
        addMarkers(
          lng = ~lon
          ,lat = ~lat
          ,popup = ~name
          ,clusterOptions = markerClusterOptions(spiderfyOnMaxZoom = TRUE)
          ,options = markerOptions(riseOnHover = TRUE)
        )
    })

  })
}
