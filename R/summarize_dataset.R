
# @param dt data to pass on
# @param vars variables to look at
# @param var_types variable type list
# @param fn_num numeric calc function
# @param fn_cat category calc function
# @param ... args passed onto fn's
summarize_with_fn_type <- function(dt, vars, var_types, fn_num, fn_cat, verbose = TRUE, ...) {
  if (verbose) {
    pb <- txtProgressBar(style = 3)
  }

  purrr::map(
    vars,
    function(col_name) {
      if (verbose) {
        setTxtProgressBar(pb, which(col_name == vars) / length(vars))
      }
      if (var_types[[col_name]] == "num") {
        fn <- fn_num
      } else {
        fn <- fn_cat
      }

      fn(dt, col_name, ...)
    }
  ) %>%
    set_names(vars) ->
  ret

  if (verbose) {
    close(pb)
  }

  ret
}


#' Summarize Full dataset
#'
#' Create a full object that summarizes both subject level and time varying variables.
#'
#' @param dt dataset to summarizes
#' @param check boolean to determine if \code{check_data()} should be performed
#' @param group_duration string of one of \code{c("week", "month", "quarter", "year")}
#' @param verbose boolean to determine if progress bars should be displayed
#' @export
#' @rdname summarize_dataset
summarize_dataset <- function(dt, check = TRUE, group_duration = "week", verbose = TRUE) {

  if (check) {
    check_data(dt)
  }

  dt <- get_data_attributes(dt)
  data_var_types <- vtype_list(dt)

  tdd <- get_time_data(dt)
  sdd <- get_subject_data(dt)

  # only keep non subject id vars
  attr(dt, "hbgd") %>%
    extract2("var_summ") %>%
    filter(type != "subject id") %>%
    extract2("variable") ->
  subject_names_good
  sdd <- sdd[colnames(sdd) %in% subject_names_good]

  if (verbose) cat("Subject level summaries\n")
  distributions <- summarize_subject_level(sdd, data_var_types, verbose)
  if (verbose) cat("Time varying summaries\n")
  time <- summarize_time_varying(tdd, data_var_types, group_duration, verbose)

  ret <- append(distributions, time[sort(names(time))])

  # order results to be the same as the incoming data
  colnames(dt) %>%
    subset(. %in% names(ret)) ->
  original_order
  ret <- ret[original_order]

  ret
}

#' @param pretty boolean to determine if the json should be pretty printed
#' @param ... all args directly passed to summarize_dataset
#' @rdname summarize_dataset
#' @export
summarize_dataset_json <- function(..., pretty = TRUE) {
  summarize_dataset(...) %>%
    jsonlite::toJSON(pretty = pretty, na = "null")
}