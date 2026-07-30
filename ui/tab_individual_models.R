nav_panel(
  title = "Individual Models",
  navset_card_underline(
    source("ui/tab_inlaenza.R", local = TRUE)$value,
    source("ui/tab_copycat.R", local = TRUE)$value,
    source("ui/tab_newgbqr.R", local = TRUE)$value
  )
) # end nav_panel Individual Models
