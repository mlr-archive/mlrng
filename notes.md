# Package Structure

* Wrappers / Hooks / Pipelines
* Still unclear: tidy version of result objects needs to be discussed.
  And we need to define what operations are supported.
  We definitely want to "merge" BMR and RR easily
* We now always store a representation of the object in the Dictionary,
  instead of the generator.
  This makes iff:
    - these are deterministic, parameter-free objects like tasks
    - we need reflections, e.g. for tasks
  This is odd iff:
    - they have parameters which define their general behavior, e.g. Resamplings (and some measures)
* We need to discuss more packages which could potentially influence the way we structure the package:
    - purrr as a replacement for `BBmisc::v*apply` and more goodies
    - a way to log or debug -> note that debugme currently does not work well with R6
    - Rcpp to speed up some
    - futures (Bengston) / promises (RStudio)
* Support for broom?
