# pick unique elements from a column --------------------------------------------
pick_cols = function(df, col){
  df %>% pull({{col}}) %>% unique()
}

# checkbox widget ---------------------------------------------------------------
wd.checkbox = function(id, label, choices, selected){

  shinyWidgets::awesomeCheckboxGroup(
    inputId = id
    ,label = label
    ,choices = choices
    ,inline = TRUE
    ,status = 'warning'
    ,selected =
  )

}

# datepicker widget ------------------------------------------------------------
wd.datepicker = function(id, label, min, max, value){

  shinyWidgets::airDatepickerInput(
    inputId = id
    ,label = label
    ,value = c(ymd('2013-01-01'), ymd('2013-12-31'))
    ,multiple = TRUE
    ,range = TRUE
    ,minDate = ymd('2013-01-01')
    ,maxDate = ymd('2013-12-31')
    ,clearButton = TRUE
    ,todayButton = TRUE
  )

}

# picker widget ---------------------------------------------------------------
wd.pickerinput = function(id, label, multiple = FALSE, choices = NULL){

  shinyWidgets::pickerInput(
    inputId = id
    ,label = label
    ,choices = choices
    ,selected = NULL
    ,multiple = multiple
    ,options =
      pickerOptions(
        actionsBox = TRUE
        ,deselectAllText = 'Deselect all'
        ,liveSearch = TRUE
        ,liveSearchPlaceholder = 'Search'
        ,liveSearchStyle = 'startsWith'
        ,noneSelectedText = 'Nothing selected'
        ,selectAllText = 'Select all'
      )
  )

}

# format date -----------------------------------------------------------------
format_date = function(df){

  df %>%
    mutate(
      date = glue::glue('{year}-{month}-{day}') %>% lubridate::ymd()
    )

}
