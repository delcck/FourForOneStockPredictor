#' @export
#' @import utils shiny tidyverse lubridate alphavantager ggplot2 tidyr

run_app = function() {

  # Name of package
  package_name = utils::packageName()


  # Name of app folder inside of inst/
  # Do not include `inst/`
  app_dir = system.file("myapp", package = package_name)

  if (app_dir == "") {
    stop(paste0("Could not find example directory. Try re-installing `",
                package_name, "`.", call. = FALSE))
  }

  shiny::runApp(app_dir, display.mode = "normal")
}

