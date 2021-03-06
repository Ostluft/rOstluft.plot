check_usage_package <- function() {
  devtools::load_all(".")
  codetools::checkUsagePackage(desc::desc_get("Package"))
}

check_examples <- function() {
  devtools::run_examples()
}

build_documentation <- function() {
  devtools::document()
  devtools::install(quick = TRUE, reload = TRUE, dependencies = FALSE)
  gh_md <- rmarkdown::github_document(html_preview = FALSE, toc = FALSE)
  rmarkdown::render("README.Rmd", gh_md, encoding = "UTF-8")
  #fs::file_delete("README.html")
  pkgdown::clean_site()
  pkgdown::build_site()
}


prepare_release <- function() {
  bump_ <- function(x, ver) {
    d <- desc::desc(text = paste0("Version: ", ver))
    suppressMessages(d$bump_version(x)$get("Version")[[1]])
  }

  bump_version <- function(ver) {
    bumps <- c("major", "minor", "patch", "dev")
    vapply(bumps, bump_, character(1), ver = ver)
  }

  proj <- usethis::proj_get()
  ver <- desc::desc_get_version(proj)
  versions <- bump_version(ver)

  choice <- utils::menu(
    choices = glue::glue(
      "{format(names(versions), justify = 'right')} --> {versions}"
    ),
    title = glue::glue(
      "Current version is {ver}.\n", "Which part to increment? (0 no increment)"
    )
  )

  if (choice != 0) {
    new_ver <- versions[choice]
    desc::desc_set_version(new_ver, proj)
  }

  choice <- utils::menu(
    choices = c(
      "No",
      "Yes"
    ),
    title = "Run devtools::check()"
  )

  if (choice == 2) {
    #
    devtools::check(env_vars = list(
      GGMAP_GOOGLE_API_KEY = Sys.getenv("GGMAP_GOOGLE_API_KEY")
    ))
  }

  choice <- utils::menu(
    choices = c(
      "No",
      "Yes"
    ),
    title = "Update Documentation"
  )

  if (choice == 2) {
    build_documentation()
  }

}

prepare_release()


