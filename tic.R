get_stage("install") %>%
  add_code_step(install.packages(old.packages())) %>%
  add_code_step(devtools::install_github("pat-s/rcmdcheck@build-args")) %>% # FIXME: If this is solved in r-lib/rcmdcheck
  add_code_step(devtools::install_github("mlr-org/phng")) %>%
  add_code_step(devtools::install_github("mllg/backports")) %>%
  add_code_step(devtools::install_github("klutometis/roxygen")) %>%
  add_code_step(devtools::install_deps(upgrade = TRUE, dependencies = TRUE))

get_stage("script") %>%
  add_code_step(devtools::document()) %>%
  add_step(step_rcmdcheck(notes_are_errors = FALSE, build_args = "--no-build-vignettes",
                          check_args = "--ignore-vignettes --no-manual --as-cran"))

if ("TRAVIS_R_VERSION_STRING" == "release") {

  get_stage("after_success") %>%
    add_code_step(covr::codecov(quiet = FALSE))

}