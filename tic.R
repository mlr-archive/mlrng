get_stage("install") %>%
  add_code_step(devtools::install_github("pat-s/rcmdcheck@build-args")) %>% # FIXME: If this is solved in r-lib/rcmdcheck
  #add_code_step(devtools::install_github("mlr-org/phng")) %>%
  #add_code_step(devtools::install_github("mllg/backports")) %>%
  add_step(step_install_cran("roxygen2")) %>%
  add_code_step(devtools::install_dev_deps()) %>%
  add_code_step(devtools::install_deps(upgrade = TRUE, dependencies = TRUE))

get_stage("script") %>%
  add_code_step(devtools::document()) %>%
  add_step(step_rcmdcheck(notes_are_errors = FALSE, warnings_are_errors = FALSE,
                          build_args = "--no-build-vignettes",
                          check_args = "--ignore-vignettes --no-manual --as-cran"))

if (inherits(ci(), "TravisCI") && Sys.getenv("TRAVIS_R_VERSION_STRING") == "release") {

  get_stage("before_deploy") %>%
    add_step(step_setup_ssh())

  get_stage("deploy") %>%
    add_code_step(devtools::document(roclets=c('rd', 'collate', 'namespace'))) %>%
    add_step(step_build_pkgdown()) %>%
    add_step(step_push_deploy(path = "docs", branch = "gh-pages")) %>%
    add_step(step_push_deploy(branch = "master", commit_paths = c("NAMESPACE", "man/*")))

  get_stage("after_success") %>%
    add_code_step(covr::codecov(quiet = FALSE))

}
