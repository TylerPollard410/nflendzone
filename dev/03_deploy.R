# Building a Prod-Ready, Robust Shiny Application.
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev scripts before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
#
#
######################################
#### CURRENT FILE: DEPLOY SCRIPT #####
######################################

# Test your app

## Run checks ----
## Check the package before sending to prod
devtools::check(args = c("--no-tests"))
# rhub::check_for_cran()

# Deploy

## Local, CRAN or Package Manager ----
## This will build a tar.gz that can be installed locally,
## sent to CRAN, or to a package manager
devtools::build()

## Docker ----
## If you want to deploy via a generic Dockerfile
golem::add_dockerfile_with_renv()
## If you want to deploy to ShinyProxy
golem::add_dockerfile_with_renv_shinyproxy()

## Posit ----
## If you want to deploy on Posit related platforms
golem::add_positconnect_file()
golem::add_shinyappsio_file()
golem::add_shinyserver_file()

## Deploy to Posit Connect or ShinyApps.io ----
remotes::install_github("TylerPollard410/nflendzone")

renv::init()
renv::snapshot()
renv::remove("nflendzone")

## Add/update manifest file (optional; for Git backed deployment on Posit )
rsconnect::writeManifest()


## Workflow when you add/update packages:
# 1. Install new package
install.packages("newpackage")

# 2. Add to DESCRIPTION file (if using golem)
# (or it gets added automatically)

# 3. Update renv.lock with current state
renv::snapshot()

# 4. Generate manifest.json from renv.lock
rsconnect::writeManifest()

# 5. Commit both files
# git add renv.lock manifest.json
# git commit -m "Add newpackage dependency"
# git push

## In command line.
rsconnect::deployApp(
  appName = desc::desc_get_field("Package"),
  appTitle = desc::desc_get_field("Package"),
  server = "shinyapps.io",
  account = "tylerpollard410",
  appFiles = c(
    # Add any additional files unique to your app here.
    "R/",
    "inst/",
    #"data/",
    "NAMESPACE",
    "DESCRIPTION",
    "app.R"
  ),
  appId = rsconnect::deployments(".")$appID,
  lint = FALSE,
  forceUpdate = TRUE
)
