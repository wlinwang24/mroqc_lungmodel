# Posit Connect Cloud entry point for the existing two-file MROQC Shiny app.
# This wrapper keeps the original ui.R and server.R code unchanged.
# Both files currently return their UI/server objects as their last expression.

ui <- source("ui.R", local = TRUE, encoding = "UTF-8")$value
server <- source("server.R", local = TRUE, encoding = "UTF-8")$value

stopifnot(inherits(ui, "shiny.tag") || inherits(ui, "shiny.tag.list"))
stopifnot(is.function(server))

shiny::shinyApp(ui = ui, server = server)
