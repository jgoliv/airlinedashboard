ui = function(){

  page_sidebar(
    title = 'Airlines'
    # ,theme = theme

    ,sidebar =
      sidebar(
        position = 'right'
        ,width = 350
        ,md.filter_UI()
      )

    ,md.dashboard_UI()

  )

}
