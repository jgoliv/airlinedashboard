# filter dataframe ---------------------------------------------------------------------
filter_df = function(reactive_df, rv){

  reactive_df() %>%
    {
      if(!is.null(rv$filter.date) | !is.null(rv$filter.airline)){
        (.) %>%
          filter(
            date %in% rv$filter.date
            ,carrier %in% rv$filter.airline
          )
      }
    }

}

# column colors list ------------------------------------------------------------------
col_list = function(df) {

  cols = c("total", "dep_delay_mean", "arr_delay_mean")

  cols %>%
    set_names() %>%
    map(\(x) {

      if (x %in% 'total') {
        scale = viridis::plasma(5, direction = -1)
      }

      if(x %in% 'dep_delay_mean') {
        scale = viridis::mako(5, direction = -1)
      }

      if (x %in% 'arr_delay_mean') {
        scale = viridis::rocket(5, direction = -1)
      }

      col_def =
        colDef(
          cell =
            reactablefmtr::data_bars(
              data = df,
              fill_color = scale
              ,background = 'grey90'
              ,text_size = 10
              ,text_position = 'outside-end'
              ,text_color = 'black'
              ,number_fmt = scales::label_number(trim = TRUE, accuracy = 0.01)
            )
          ,align = 'center'
        )

      col_def

    })

}
