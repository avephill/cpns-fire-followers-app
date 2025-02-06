library(rsconnect)

rsconnect::deployApp("/home/ahill/Projects/fire-followers/shiny",
  appFiles = c(
    "app.R", "data/shiny.db"
  )
)
