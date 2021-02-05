# Shiny app

This is a Shiny web application. You can run the application locally by clicking
the 'Run App' button after opening any of the following files in RStudio IDE:
`global.R`, `ui.R`, or `server.R`.

## Known issues

- Error in `evalues.RR`: Lower confidence limit should be less than upper confidence limit:
  probably need to `observe()` `input$lo.RR` and `input$hi.RR` or use `renderUI`.

## Deployment workflow

### Putting the app into a Docker container

Revised the files:

- `startup.R` is now `global.R`,
- deleted `app.R`, no need for that when there is global/ui/server files
- made `server.R` to contain `server <- function() {...}`
- made `ui.R` to contain `ui <- navbarPage(...)`
- moved the app specific files into the `/app` folder, see reasoning below
- added `Dockerfile`

Dockerfile contents explained




# Heroku deployment examples

Workflow (works with public and privare repositories):

- start with this template or modify your app taking this as an example
- run `renv::init()` or `renv::snapshot()` to capture dependencies in the `renv.lock` file
- work on your app
- add secrets to your GitHub repo settings and follow steps below

The `/app` folder contains the shiny app. The demo app was taken from here:
https://github.com/analythium/shinyproxy-demo

Note: Shiny apps [time out after 55 seconds](https://devcenter.heroku.com/articles/limits#http-timeouts):

> After the initial response, each byte sent from the server restarts a rolling 55 second window. A similar 55 second window is restarted for every byte sent from the client.
>
> If no data is received from the dyno within the 55 second window the connection is terminated and an H15 error is logged.
>
> Similarly, if no data is received from the client within the 55 second window, the connection is terminated and an H28 error is logged.

A workaround was posted on [SO](https://stackoverflow.com/questions/54594781/how-to-prevent-a-shiny-app-from-being-grayed-out) to print a dot to the console every passage of 10 seconds. 
A counter set at 50 seconds interval is added to `/app/server.R`:

```R
## prevent timeout
autoInvalidate <- reactiveTimer(intervalMs = 50*1000)
observe({
    autoInvalidate()
    cat(".")
})
```

The following deployment options are explained here:

- [Using GitHub actions](#using-github-actions)
- [Using local Heroku CLI](#using-local-heroku-cli)

## Deployment using GitHub actions

Log into Heroku, in the dashboard, click on 'New' then select 'Create new App'.
Give a name (e.g. `shiny-cicd`, if available, this will create the app at https://shiny-cicd.herokuapp.com/) to the app and create the app.

Got to the Settings tab of the repo, scrolld down to Secrets and add the
following new repository secrets:

- `HEROKU_EMAIL`: your Heroku email
- `HEROKU_APP_NAME`: you application name from above
- `HEROKU_API_KEY`: your Heroku api key, you can find it under your personal settings, click on reveal and copy

See the `.github/workflows/deploy.yml` file for additional options
(`dockerfile_name`, `docker_options`, `dockerfile_directory`)

