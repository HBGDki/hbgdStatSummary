
# @param dt data to pass on
# @param vars variables to look at
# @param var_types variable type list
# @param fn_num numeric calc function
# @param fn_cat category calc function
# @param ... args passed onto fn's
summarize_with_fn_type <- function(dt, vars, var_types, fn_num, fn_cat, verbose = TRUE, ...) {
  if (verbose) {
    pb <- progress_bar$new(
      total = length(vars),
      format = "  [:bar] :percent eta::eta",
      clear = FALSE
    )
  }

  purrr::map(
    vars,
    function(col_name) {
      if (verbose) {
        pb$tick()
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

  ret
}


#' Summarize full dataset
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

  # get days sep by 1 week for up to 2 years
  time_breaks <- seq(from = 1, by = 7, length.out = 2 * 52 + 1)

  # make sure they are under two years old
  dt <- dt[
    dt[["agedays"]] < max(time_breaks) &  # remove old time
    dt[["agedays"]] >= min(time_breaks),  # remove 'pre-birth' time
  ]

  # find out which week the record was taken
  lapply(dt$agedays, function(day) {
    which(day >= time_breaks[-length(time_breaks)] & day < time_breaks[-1])
  }) %>%
    unlist() ->
  dt$ageweeks


  # remove all NA columns
  is_na_cols <- sapply(dt, function(col) { all(is.na(col)) })
  if (length(is_na_cols) > 0) {
    if (verbose) {
      cat("Removing ", sum(is_na_cols), " NA columns from data\n", sep = "")
      cat("Bad cols: \n")
      cat("  ", paste(names(dt)[is_na_cols], collapse = ", "), "\n", sep = "")
    }
    dt <- dt[!is_na_cols]
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


#' Summarize full dataset and save to file
#'
#' Create a full object that summarizes both subject level and time varying variables, then saves the json results to a file
#'
#' @param ... args passed directly to \code{summarize_dataset_json}
#' @param file file to save to
#' @export
summarize_dataset_file <- function(..., file) {
  ret <- summarize_dataset_json(...)
  cat(ret, file = file)
  cat("\n", file = file, append = TRUE)

  invisible(ret)
}
