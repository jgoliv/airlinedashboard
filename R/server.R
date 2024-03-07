server = function(input, output, session){

  # reactiveVals --------------------------------------------------------------------------------------
  rv = reactiveValues()

  observe({

    # dataframes
    rv$flights =
      nycflights13::flights %>%
      format_date() %>%
      mutate(month = date %>% lubridate::month(label = TRUE, abbr = TRUE))

    rv$airlines = nycflights13::airlines
    rv$airports = nycflights13::airports
    rv$planes = nycflights13::planes
    rv$weather = nycflights13::weather

    # filters
    rv$filter.date = rv$flights %>% pull(date) %>% unique()
    rv$filter.airline = rv$airlines %>% pull(carrier)

  })

  # modules ------------------------------------------------------------------------------------------
  md.filter(rv = rv)
  md.dashboard(rv = rv)

}
