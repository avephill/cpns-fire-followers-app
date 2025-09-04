library(rsconnect)

rsconnect::deployApp("/home/ahill/Projects/fire-followers/fire-followers-app",
  appFiles = c(
    "app.R", "about.html", "data/shiny.db", "www/cnps-style.css"
  )
)
