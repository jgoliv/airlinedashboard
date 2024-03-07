# rosetype radius pie plot -------------------------------------------------------------------------
plot.pie_radius = function(df, col){

  df %>%
    summarise(Total = n(), .by = {{col}}) %>%
    arrange(desc(Total)) %>%
    e_charts_(col) %>%
    e_pie(
      Total
      ,roseType = 'radius'
      ,label =
        list(formatter = htmlwidgets::JS("
      function(params) {
        return params.name + ' : ' + params.value + ' (' + params.percent + '%)';
      }
  "))
    ) %>%
    e_tooltip(trigger = 'item') %>%
    e_legend(type = 'scroll') %>%
    e_theme('caravan')

}

# line plot with timeline -------------------------------------------------------------------------
plot.timeline = function(df, by){

  aux =
    df %>%
    summarise(Total = n(), .by = c(month, {{by}})) %>%
    complete(!!!(lapply(c(by), sym)), month, fill = list(Total = 0)) %>%
    ungroup()

  aux %>%
    group_by(across(any_of(by))) %>%
    e_charts(month, timeline = TRUE) %>%
    e_line(
      Total
      ,symbol = 'roundRect'
      ,lineStyle = list(width = 3, opacity = 0.8)
      ,legend = list(show = FALSE)
      ,label = list(show = TRUE)
      ,color = '#3D5467'
    ) %>%
    e_timeline_opts(autoPlay = TRUE) %>%
    e_tooltip(trigger = 'axis') %>%
    e_timeline_serie(
      title =
        map(
          aux %>% pull({{by}}) %>% unique()
          ,~ list(
            subtext = glue('{by}: {.x}')
          )
        )
    )

}

# reactable plot -------------------------------------------------------------------
plot.reactable = function(df) {

  col_list = col_list(df)

  df %>%
    reactable(
      columns = col_list
      ,compact = TRUE
      ,outlined = TRUE
    )

}
