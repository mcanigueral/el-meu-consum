
# Waiter screen -----------------------------------------------------------

waiting_screen <- function(msg) {
  tagList(
    spin_flower(),
    h4(msg)
  )
}

