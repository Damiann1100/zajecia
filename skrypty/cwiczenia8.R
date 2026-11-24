library(workflowr)
wflow_git_config(
  user.name = "Damiann1100",
  user.email = "nowaczykdamiann@gmail.com",
  overwrite = TRUE
)

wflow_start(directory = "C:/Users/Damian/Desktop/Zajęcia/R/Nowaczyk/workflowr",
            name = "zajecia",
            existing = TRUE)

wflow_build()

wflow_publish(c("analysis/*Rmd", "docs/*html"))

wflow_git_remote(
  "origin",
  user = "Damiann1100",
  repo = "zajecia",
  action = "set_url"
)
