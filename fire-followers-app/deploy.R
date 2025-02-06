library(rsconnect)

rsconnect::deployApp("/home/ahill/Projects/fire-followers/fire-followers-app",
  appFiles = c(
    "app.R", "data/shiny.db"
  )
)
