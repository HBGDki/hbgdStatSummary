
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
#' @param agedays_min,agedays_max min and max agedays allowed
#' @export
#' @rdname summarize_dataset
summarize_dataset <- function(
  dt,
  check = TRUE,
  group_duration = "week",
  verbose = TRUE,
  agedays_min = -365,
  agedays_max = 265*2
) {

  colnames(dt) <- tolower(colnames(dt))

  if (check) {
    check_data(dt)
  }

  # get days sep by 1 week for up to 2 years
  # time_breaks <- seq(from = 1, by = 7, length.out = 2 * 52 + 1)

  # make sure they are under two years old
  dt <- dt[!is.na(dt$agedays), ]
  dt <- dt[
    dt[["agedays"]] <= agedays_max &  # remove old time
    dt[["agedays"]] >= agedays_min,  # remove 'pre-birth' time
  ]

  # find out which week the record was taken
  dt$ageweeks <- floor(dt$agedays / 7)


  # remove all NA columns
  is_na_cols <- sapply(dt, function(col) {
    all(is.na(col))
  })
  if (sum(is_na_cols) > 0) {
    # always print, as it's changing the data
    cat("Removing ", sum(is_na_cols), " NA columns from data:\n", sep = "")
    cat("  ", paste(names(dt)[is_na_cols], collapse = ", "), "\n", sep = "")
    dt <- dt[!is_na_cols]
  }

  if (is.null(attr(dt, "hbgd"))) {
    dt <- get_data_attributes(dt)
  }
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
  summarize_dataset(...) %>% to_json(pretty = pretty)
}


#' Summarize full dataset and save to file
#'
#' Create a full object that summarizes both subject level and time varying variables, then saves the json results to a file
#'
#' @param pretty boolean to determine if the json should be pretty printed
#' @param ... args passed directly to \code{summarize_dataset_json}
#' @param file file to save to
#' @export
summarize_dataset_file <- function(..., file, pretty) {
  ret <- summarize_dataset(...)
  json <- to_file(ret, file, pretty)
  invisible(json)
}


#' Save methods
#'
#' @param x item in question
#' @param pretty boolean to pretty print the json
#' @param file file to print the json
#' @rdname to_json
#' @export
to_json <- function(x, pretty = FALSE) {
  jsonlite::toJSON(x, pretty = pretty, na = "null", auto_unbox = TRUE)
}
#' @rdname to_json
#' @export
to_file <- function(x, file, pretty = FALSE) {
  x <- to_json(x, pretty)
  readr::write_file(x, path = file)
  readr::write_file("\n", path = file, append = TRUE)
  x
}






























#' Summarise subject level info per cateogry
#' '
#' @param dt dataset to summarizes
#' @param check boolean to determine if \code{check_data()} should be performed
#' @param group_duration string of one of \code{c("week", "month", "quarter", "year")}
#' @param verbose boolean to determine if progress bars should be displayed
#' @param agedays_min,agedays_max min and max agedays allowed
#' @importFrom magrittr equals not
#' @export
summarize_subject_per_category <- function(
  dt,
  check = TRUE,
  group_duration = "week",
  verbose = TRUE,
  agedays_min = -365,
  agedays_max = 365*2
) {

  colnames(dt) <- tolower(colnames(dt))

  if (check) {
    check_data(dt)
  }

  # get days sep by 1 week for up to 2 years
  # time_breaks <- seq(from = 1, by = 7, length.out = 2 * 52 + 1)

  # make sure they are under two years old
  dt <- dt[!is.na(dt$agedays), ]
  dt <- dt[
    dt[["agedays"]] <= agedays_max &  # remove old time
    dt[["agedays"]] >= agedays_min,  # remove 'pre-birth' time
  ]

  # find out which week the record was taken
  dt$ageweeks <- floor(dt$agedays / 7)

  # remove all NA columns
  is_na_cols <- sapply(dt, function(col) {
    all(is.na(col))
  })
  if (sum(is_na_cols) > 0) {
    cat("Removing ", sum(is_na_cols), " NA columns from data:\n", sep = "")
    cat("  ", paste(names(dt)[is_na_cols], collapse = ", "), "\n", sep = "")
    dt <- dt[!is_na_cols]
  }

  dt <- get_data_attributes(dt)
  data_var_types <- vtype_list(dt)

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

  # get numeric data
  sapply(distributions, `[[`, "type") %>%
    equals("subject-level-num") %>%
    which() %>%
    names() ->
  numeric_subject_columns
  num_distro <- sdd[colnames(sdd) %in% numeric_subject_columns]

  # get category columns
  sapply(distributions, `[[`, "type") %>%
    equals("subject-level-num") %>%
    not() %>%
    which() %>%
    names() ->
  cat_subject_columns

  lapply(
    cat_subject_columns,
    function(col) {
      keys <- distributions[[col]]$counts$key
      col_dt <- dt[[col]]
      data_frame(
        col = col,
        key = keys,
        is_key = lapply(keys, function(key) {
          if (key == "..na..") {
            is.na(col_dt)
          } else {
            col_dt == key
          }
        })
      )
    }
  ) %>%
    bind_rows() ->
  col_key_combos

  # for each category and key, summarise all numeric columns
  if (verbose) {
    cat("Subject level summaries per category\n")
    pb <- progress_bar$new(
      total = nrow(col_key_combos),
      format = "  [:bar] :percent eta::eta (:category/:key)",
      clear = FALSE,
      show_after = 0
    )
    pb$tick(0)
  }

  # only keep non subject id vars
  attr(dt, "hbgd") %>%
    extract2("var_summ") %>%
    filter(type != "subject id") %>%
    extract2("variable") ->
  subject_names_good

  ret <- lapply(seq_len(nrow(col_key_combos)), function(combo_pos) {

    row <- col_key_combos[combo_pos, ]
    col <- col_key_combos$col[[combo_pos]]
    key <- col_key_combos$key[[combo_pos]]
    is_key <- col_key_combos$is_key[[combo_pos]]

    if (verbose) pb$tick(tokens = list(category = col, key = key))

    key_dt <- dt[is_key, ]
    # transfer the attr so that the columns are selected according to the whole dataset
    attr(key_dt, "hbgd") <- attr(dt, "hbgd")

    tdd <- get_time_data(key_dt)
    sdd <- get_subject_data(key_dt)

    # only keep non subject id vars
    sdd <- sdd[colnames(sdd) %in% subject_names_good]

    distributions <- summarize_subject_level(sdd, data_var_types, verbose = FALSE)
    time <- summarize_time_varying(tdd, data_var_types, group_duration, verbose = FALSE)

    summary <- append(distributions, time[sort(names(time))])

    # order results to be the same as the incoming data
    summary <- summary[colnames(dt)]


    # summary <- summarize_dataset(
    #   key_dt,
    #   check = check, time_breaks = time_breaks,
    #   verbose = FALSE, group_duration = group_duration
    # )

    list(
      category_column = col,
      category_key = key,
      summary = summary
    )
    #
    # lapply(
    #   numeric_subject_columns,
    #   function(numeric_col) {
    #     list(
    #       category_column = col,
    #       category_key = key,
    #       numeric_column = numeric_col,
    #       dist = summarize_subject_level_num(key_dt, numeric_col)
    #     )
    #   }
    # )
  })

  # ret <- unlist(ret, recursive = FALSE)

  ret
}
