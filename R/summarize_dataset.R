
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
      format = "summarise [:bar] :percent :elapsed:-:eta :current/:total :spin",
      show_after = 0,
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
  agedays_max = 365 * 2
) {

  colnames(dt) <- tolower(colnames(dt))

  if (check) {
    check_data(dt)
  }

  # get days sep by 1 week for up to 2 years
  # time_breaks <- seq(from = 1, by = 7, length.out = 2 * 52 + 1)

  if (identical(dt$agedays, NULL)) {
    data_var_types <- lapply(dt, function(x_col) {
      if (is.numeric(x_col)) {
        "num"
      } else {
        "cat"
      }
    })
    names(data_var_types) <- colnames(dt)
    ret <- summarize_subject_level(dt, data_var_types, verbose)
    attr(ret, "dt") <- dt
    return(ret)
  }

  original_age_days_range <- range(dt$agedays, na.rm = TRUE)
  # make sure they are under two years old
  dt <- dt[!is.na(dt$agedays), ]
  dt <- dt[
    dt[["agedays"]] <= agedays_max &  # remove old time
    dt[["agedays"]] >= agedays_min,  # remove 'pre-birth' time
  ]

  if (nrow(dt) == 0) {
    stop("No data within the agedays of ", agedays_min, ":", agedays_max, ". Supplied range is ", original_age_days_range[1], ":", original_age_days_range[2]) # nolint
  }

  # remove all NA columns (after subsetting dates)
  is_na_cols <- sapply(dt, function(col) {
    all(is.na(col))
  })
  if (sum(is_na_cols) > 0) {
    # always print, as it's changing the data
    cat("Removing ", sum(is_na_cols), " NA columns from data:\n", sep = "")
    cat("  ", paste(names(dt)[is_na_cols], collapse = ", "), "\n", sep = "")
    dt <- dt[!is_na_cols]
  }


  # find out which week the record was taken
  dt$ageweeks <- floor(dt$agedays / 7)

  if (is.null(attr(dt, "hbgd"))) {
    dt <- get_data_attributes(dt)
    var_summ <- attr(dt, "hbgd")$var_summ
    ageweeks_row <- which(var_summ$variable == "ageweeks")
    if (var_summ$type[ageweeks_row] == "subject-level") {
      var_summ$type[ageweeks_row] <- "time-varying"
      attr(dt, "hbgd")$var_summ <- var_summ
      attr(dt, "hbgd")$timevarying_vars <- c(attr(dt, "hbgd")$timevarying_vars, "ageweeks")
      attr(dt, "hbgd")$subjectlevel_vars <- setdiff(attr(dt, "hbgd")$subjectlevel_vars, "ageweeks")
    }
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
  ret <- summarize_subject_level(sdd, data_var_types, verbose)

  # there there are columns more than agedays and ageweeks
  if (ncol(tdd) > 2) {
    if (verbose) cat("Time varying summaries\n")
    time <- summarize_time_varying(tdd, data_var_types, group_duration, verbose)
    ret <- append(ret, time[sort(names(time))])
  }



  # order results to be the same as the incoming data
  dt_cols <- colnames(dt)
  original_order <- dt_cols[dt_cols %in% names(ret)]
  ret <- ret[original_order]

  # var_info <- data_frame(
  #   name = c(colnames(sdd), colnames(tdd)),
  #   kind = rep(c("subject", "time"), c(length(colnames(sdd)), length(colnames(tdd))))
  # ) %>%
  #   mutate(
  #     type = unlist(data_var_types[name])
  #   ) ->
  # attr(ret, "var_info")
  attr(ret, "dt") <- dt
  attr(ret, "hbgd") <- attr(dt, "hbgd")

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
#' @rdname to_json
#' @param data_name name of dataset
#' @param verbose boolean to determine if a progress bar is displayed
#' @export
to_multiple_files <- function(x, data_name, pretty = FALSE, verbose = TRUE, parallel_cores = 1) {

  dir.create(data_name, recursive = TRUE, showWarnings = FALSE)

  if (verbose) {
    cat("Saving outputs to folder: ", data_name, "\n")
    len <- length(names(x))
    format <- "save ':col' [:bar] :percent :elapsed:-:eta :current/:total :spin"
    if (parallel_cores > 1) {
      len <- len / parallel_cores
      format <- paste(format, "\n", sep = "")
    }
    pb <- progress_bar$new(
      total = len,
      format = format,
      show_after = 0,
      clear = FALSE
    )
    pb$tick(0)
  }

  # for (name in names(x)) {
  plyr::llply(
    names(x),
    function(name) {
      if (verbose) pb$tick(tokens = list(col = name))
      to_file(x[[name]], file = file.path(data_name, paste(name, ".json", sep = "")), pretty = pretty)
      NULL
    },
    .parallel = parallel_cores > 1
  )
  # }

  invisible(x)
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
summarize_dataset_with_time_varying_subsets <- function(
  dt,
  check = TRUE,
  group_duration = "week",
  verbose = TRUE,
  agedays_min = -365,
  agedays_max = 365 * 2,
  parallel_cores = 1
) {

  ret <- summarize_dataset(
    dt, check = check, group_duration = group_duration,
    verbose = verbose, agedays_min = agedays_min, agedays_max = agedays_max
  )

  lapply(ret, function(x) {
    data_frame(id = x$id, type = x$type)
  }) %>%
    dplyr::bind_rows() ->
  variable_types

  subj_cat_cols <- variable_types %>% filter(type == "subject-level-cat") %>% extract2("id")
  time_var_num_cols <- variable_types %>% filter(type == "time-varying-num") %>% extract2("id")

  upgraded_dt <- attr(ret, "dt")

  group_by_fn <- get_group_by_fn(group_duration)

  for (col in time_var_num_cols) {
    # init
    ret[[col]]$"subject-level-cat" <- list()
  }

  cols_and_keys <- plyr::ldply(subj_cat_cols, function(subj_cat_col) {
    data_frame(
      subj_cat_col = subj_cat_col,
      column_key = ret[[subj_cat_col]]$counts$key
    )
  })

  cols_and_keys <- plyr::ldply(time_var_num_cols, function(time_var_num_col) {
    ans <- cols_and_keys
    ans$time_var_num_col <- time_var_num_col
    ans
  })

  if (verbose) {

    total_count <- nrow(cols_and_keys)

    if (parallel_cores > 1) {
      total_count <- total_count / parallel_cores
    }

    cat("Per Subject Category, Time Varying summaries\n")
    pb <- progress_bar$new(
      total = total_count,
      format = "summarise/subj cat [:bar] :percent :elapsed:-:eta :current/:total :spin",
      clear = FALSE
    )
    pb$tick(0)

  }

  par_ans <- plyr::llply(
    seq_len(nrow(cols_and_keys)),
    .parallel = parallel_cores > 1,
    function(i) {
      subj_cat_col <- cols_and_keys$subj_cat_col[i]
      column_key <- cols_and_keys$column_key[i]
      time_var_num_col <- cols_and_keys$time_var_num_col[i]

      dt_col <- upgraded_dt[[subj_cat_col]]

      dt_col_is_na <- is.na(dt_col)

      # subset the data once per subj-cat column/key combo
      if (column_key == "..na..") {
        subset_dt <- upgraded_dt[dt_col_is_na, ]
      } else {
        subset_dt <- upgraded_dt[(dt_col == column_key) & !dt_col_is_na, ]
      }

      # get the time summaries with the subsetted data
      time_var_num_sum <- summarize_time_varying_num(subset_dt, time_var_num_col, group_by_fn = group_by_fn, group_duration = group_duration)

      if (verbose) pb$tick()

      list(
        time_bins = time_var_num_sum$time_bins,
        subj_cat_col = subj_cat_col,
        column_key = column_key,
        time_var_num_col = time_var_num_col
      )
    }
  )

  # store the answer in proper place
  if (verbose) {
    pb <- progress_bar$new(
      total = length(par_ans),
      format = "combine results [:bar] :percent :elapsed:-:eta :current/:total :spin",
      clear = FALSE
    )
    pb$tick(0)
  }
  for (ans_i in par_ans) {
    if (verbose) pb$tick()
    time_var_num_col <- ans_i$time_var_num_col
    column_key <- ans_i$column_key
    subj_cat_col <- ans_i$subj_cat_col
    time_bins <- ans_i$time_bins

    # init
    if (is.null(ret[[time_var_num_col]][["subject-level-cat"]][[subj_cat_col]])) {
      ret[[time_var_num_col]][["subject-level-cat"]][[subj_cat_col]] <- list()
    }

    # store time bins in c("time col", "subject-level-cat", "SUBJ CAT COL", "KEY")
    ret[[time_var_num_col]][["subject-level-cat"]][[subj_cat_col]][[column_key]] <- list(time_bins = time_bins)
  }


  # # for each subject category column
  # lapply(subj_cat_cols, function(subj_cat_col) {
  #
  #   column_keys <- ret[[subj_cat_col]]$counts$key
  #   dt_col <- upgraded_dt[[subj_cat_col]]
  #
  #   dt_col_is_na <- is.na(dt_col)
  #
  #   # for each key in subject cat column
  #   lapply(column_keys, function(column_key) {
  #     # subset the data once per subj-cat column/key combo
  #     if (column_key == "..na..") {
  #       subset_dt <- upgraded_dt[dt_col_is_na, ]
  #     } else {
  #       subset_dt <- upgraded_dt[(dt_col == column_key) & !dt_col_is_na, ]
  #     }
  #
  #     # for each time-varying-num column
  #     lapply(time_var_num_cols, function(time_var_num_col) {
  #       # init
  #       if (is.null(ret[[time_var_num_col]][["subject-level-cat"]][[subj_cat_col]])) {
  #         ret[[time_var_num_col]][["subject-level-cat"]][[subj_cat_col]] <<- list()
  #       }
  #
  #       # get the time summaries with the subsetted data
  #       time_var_num_sum <- summarize_time_varying_num(subset_dt, time_var_num_col, group_by_fn = group_by_fn, group_duration = group_duration)
  #
  #       # store time bins in c("time col", "subject-level-cat", "SUBJ CAT COL", "KEY")
  #       ret[[time_var_num_col]][["subject-level-cat"]][[subj_cat_col]][[column_key]] <<- list(time_bins = time_var_num_sum$time_bins)
  #
  #       NULL
  #     })
  #
  #     if (verbose) pb$tick()
  #     NULL
  #   })
  #   NULL
  # })


  ret

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
summarize_dataset_with_time_varying_subsets_two <- function(
  dt,
  check = TRUE,
  group_duration = "week",
  verbose = TRUE,
  agedays_min = -365,
  agedays_max = 365 * 2,
  parallel_cores = 1
) {

  ret <- summarize_dataset(
    dt, check = check, group_duration = group_duration,
    verbose = verbose, agedays_min = agedays_min, agedays_max = agedays_max
  )

  lapply(ret, function(x) {
    data_frame(id = x$id, type = x$type)
  }) %>%
    dplyr::bind_rows() ->
  variable_types

  subj_cat_cols <- variable_types %>% filter(type == "subject-level-cat") %>% extract2("id")
  time_var_num_cols <- variable_types %>% filter(type == "time-varying-num") %>% extract2("id")

  upgraded_dt <- attr(ret, "dt")

  group_by_fn <- get_group_by_fn(group_duration)

  if (length(time_var_num_cols) > 0) {

    if (verbose) {
      cat("Per Subject Category, Time Varying summaries\n")
      time_pb <- progress_bar$new(
        total = length(time_var_num_cols),
        format = "time columns: ':time_col' [:bar] :current/:total :elapsed:-:eta\n",
        show_after = 0,
        clear = FALSE
      )

    }

    for (time_var_num_col in time_var_num_cols) {
      # init
      ret[[time_var_num_col]]$"subject-level-cat" <- list()

      cols_and_keys <- plyr::ldply(subj_cat_cols, function(subj_cat_col) {
        # init
        if (is.null(ret[[time_var_num_col]][["subject-level-cat"]][[subj_cat_col]])) {
          ret[[time_var_num_col]][["subject-level-cat"]][[subj_cat_col]] <<- list()
        }

        data_frame(
          subj_cat_col = subj_cat_col,
          column_key = ret[[subj_cat_col]]$counts$key
        )
      })

      if (verbose) {
        time_pb$tick(tokens = list(time_col = time_var_num_col))
        total_count <- nrow(cols_and_keys)
        if (parallel_cores > 1) {
          total_count <- total_count / parallel_cores
        }
        pb <- progress_bar$new(
          total = total_count,
          format = "':time_col' summarise [:bar] :percent :elapsed:-:eta :current/:total :spin",
          clear = TRUE
        )
        pb$tick(0)
      }

      par_ans <- plyr::llply(
        seq_len(nrow(cols_and_keys)),
        .parallel = parallel_cores > 1,
        function(i) {
          subj_cat_col <- cols_and_keys$subj_cat_col[i]
          column_key <- cols_and_keys$column_key[i]

          dt_col <- upgraded_dt[[subj_cat_col]]

          dt_col_is_na <- is.na(dt_col)

          # subset the data once per subj-cat column/key combo
          if (column_key == "..na..") {
            subset_dt <- upgraded_dt[dt_col_is_na, ]
          } else {
            subset_dt <- upgraded_dt[(dt_col == column_key) & !dt_col_is_na, ]
          }

          # get the time summaries with the subsetted data
          time_var_num_sum <- summarize_time_varying_num(subset_dt, time_var_num_col, group_by_fn = group_by_fn, group_duration = group_duration)

          if (verbose) pb$tick(tokens = list(time_col = time_var_num_col))

          list(
            time_bins = time_var_num_sum$time_bins,
            subj_cat_col = subj_cat_col,
            column_key = column_key
          )
        }
      )

      # store the answer in proper place
      if (verbose) {
        pb <- progress_bar$new(
          total = length(par_ans),
          format = "combine results [:bar] :percent :elapsed:-:eta :current/:total :spin",
          show_after = 0,
          clear = FALSE
        )
      }
      for (ans_i in par_ans) {
        if (verbose) pb$tick()
        column_key <- ans_i$column_key
        subj_cat_col <- ans_i$subj_cat_col
        time_bins <- ans_i$time_bins
        # store time bins in c("time col", "subject-level-cat", "SUBJ CAT COL", "KEY")
        ret[[time_var_num_col]][["subject-level-cat"]][[subj_cat_col]][[column_key]] <- list(time_bins = time_bins)
      }
    }
  }

  ret
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
summarize_dataset_with_time_varying_subsets_three <- function(
  dt,
  check = TRUE,
  group_duration = "week",
  verbose = TRUE,
  agedays_min = -365,
  agedays_max = 365 * 2,
  parallel_cores = 1
) {

  ret <- summarize_dataset(
    dt, check = check, group_duration = group_duration,
    verbose = verbose, agedays_min = agedays_min, agedays_max = agedays_max
  )

  lapply(ret, function(x) {
    data_frame(id = x$id, type = x$type)
  }) %>%
    dplyr::bind_rows() ->
  variable_types

  subj_cat_cols <- variable_types %>% filter(type == "subject-level-cat") %>% extract2("id")
  time_var_num_cols <- variable_types %>% filter(type == "time-varying-num") %>% extract2("id")

  upgraded_dt <- attr(ret, "dt")

  group_by_fn <- get_group_by_fn(group_duration)

  if (length(time_var_num_cols) > 0) {

    # init
    for (time_var_num_col in time_var_num_cols) {
      ret[[time_var_num_col]]$"subject-level-cat" <- list()
      for (subj_cat_col in subj_cat_cols) {
        if (is.null(ret[[time_var_num_col]][["subject-level-cat"]][[subj_cat_col]])) {
          ret[[time_var_num_col]][["subject-level-cat"]][[subj_cat_col]] <- list()
        }
      }
    }

    if (verbose) {
      cat("Per Subject Category, Time Varying summaries\n")

      total_count <- plyr::laply(subj_cat_cols, function(subj_cat_col) {
        length(ret[[subj_cat_col]]$counts$key)
      }) %>%
        sum()

      pb <- progress_bar$new(
        total = total_count,
        format = ":subj_i/:subj_len ':subj_col' :key_current/:key_total [:bar] :percent :elapsed:-:eta :current/:total :spin",
        clear = FALSE,
        show_after = 0
      )
    }

    subj_cat_cols_length <- length(subj_cat_cols)

    for (subj_i in seq_len(subj_cat_cols_length)) {
      subj_cat_col <- subj_cat_cols[subj_i]

      dt_col <- upgraded_dt[[subj_cat_col]]

      dt_col_is_na <- is.na(dt_col)

      column_keys <- ret[[subj_cat_col]]$counts$key
      column_keys_length <- length(column_keys)

      # for (column_key in column_keys) {
      for (i in seq_len(column_keys_length)) {
        column_key <- column_keys[i]

        # subset the data once per subj-cat column/key combo
        if (column_key == "..na..") {
          subset_dt <- upgraded_dt[dt_col_is_na, ]
        } else {
          subset_dt <- upgraded_dt[(dt_col == column_key) & !dt_col_is_na, ]
        }

        if (verbose) {
          pb$tick(tokens = list(
            subj_col = subj_cat_col,
            key_current = i, key_total = column_keys_length,
            subj_i = subj_i, subj_len = subj_cat_cols_length
          ))
        }

        par_ans <- plyr::llply(
          time_var_num_cols,
          .parallel = parallel_cores > 1,
          function(time_var_num_col) {

            # get the time summaries with the subsetted data
            time_var_num_sum <- summarize_time_varying_num(subset_dt, time_var_num_col, group_by_fn = group_by_fn, group_duration = group_duration)

            list(
              time_bins = time_var_num_sum$time_bins,
              time_var_num_col = time_var_num_col
            )
          }
        )

        for (ans_i in par_ans) {
          time_var_num_col <- ans_i$time_var_num_col
          time_bins <- ans_i$time_bins
          # store time bins in c("time col", "subject-level-cat", "SUBJ CAT COL", "KEY")
          ret[[time_var_num_col]][["subject-level-cat"]][[subj_cat_col]][[column_key]] <- list(time_bins = time_bins)
        }
      }
    }
  }

  ret
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
summarize_dataset_with_time_varying_subsets_and_save_four <- function(
  dt,
  data_name,
  check = TRUE,
  group_duration = "week",
  verbose = TRUE,
  agedays_min = -365,
  agedays_max = 365 * 2,
  pretty = FALSE,
  parallel_cores = 1
) {

  dir.create(data_name, showWarnings = FALSE, recursive = TRUE)

  ret <- summarize_dataset(
    dt, check = check, group_duration = group_duration,
    verbose = verbose, agedays_min = agedays_min, agedays_max = agedays_max
  )

  lapply(ret, function(x) {
    data_frame(id = x$id, type = x$type)
  }) %>%
    dplyr::bind_rows() ->
  variable_types

  subj_cat_cols <- variable_types %>% filter(type == "subject-level-cat") %>% extract2("id")
  time_var_num_cols <- variable_types %>% filter(type == "time-varying-num") %>% extract2("id")

  upgraded_dt <- attr(ret, "dt")

  group_by_fn <- get_group_by_fn(group_duration)

  if (length(time_var_num_cols) > 0) {

    if (verbose) {
      cat("Per Subject Category, Time Varying summaries\n")
    }

    time_var_num_cols_length <- length(time_var_num_cols)
    subj_cat_cols_length <- length(subj_cat_cols)

    ignore <- plyr::llply(
      seq_len(time_var_num_cols_length),
      .parallel = parallel_cores > 1,
      function(time_i) {

        time_var_num_col <- time_var_num_cols[time_i]

        save_name <- file.path(data_name, paste(time_var_num_col, ".json", sep = ""))
        if (file.exists(save_name)) {
          if (verbose) {
            cat("returning early. Output already found for ", save_name, "\n")
          }
          return(NULL)
        }

        if (verbose) {
          total_count <- plyr::laply(subj_cat_cols, function(subj_cat_col) {
            length(ret[[subj_cat_col]]$counts$key)
          }) %>%
            sum()

          pb <- progress_bar$new(
            total = total_count,
            format = ":time_i/:time_len :subj_i/:subj_len:':subj_col' :key_current/:key_total [:bar] :percent :elapsed:-:eta :spin",
            clear = FALSE
          )
        }

        lapply(
          seq_len(subj_cat_cols_length),
          function(subj_i) {
            subj_cat_col <- subj_cat_cols[subj_i]

            dt_col <- upgraded_dt[[subj_cat_col]]

            dt_col_is_na <- is.na(dt_col)

            column_keys <- ret[[subj_cat_col]]$counts$key
            column_keys_length <- length(column_keys)

            if (column_keys_length > 10000) {
              cat("returning early for column: ", subj_cat_col, ". It has more keys (", column_keys_length, ") than the alloted 10k\n")
              # increment the number of cols to update the timer
              pb$tick(column_keys_length)
              return(NULL)
            } else if (subj_cat_col == "subjido") {
              cat("returning early for column: 'subjido'. No sub summaries allowed\n")
              # increment the number of cols to update the timer
              pb$tick(column_keys_length)
              return(NULL)
            }

            # for (column_key in column_keys) {
            # for (i in seq_len(column_keys_length)) {
            lapply(
              seq_len(column_keys_length),
              function(i) {
                column_key <- column_keys[i]

                # subset the data once per subj-cat column/key combo
                if (column_key == "..na..") {
                  subset_dt <- upgraded_dt[dt_col_is_na, ]
                } else {
                  subset_dt <- upgraded_dt[(dt_col == column_key) & !dt_col_is_na, ]
                }

                if (verbose) {
                  pb$tick(tokens = list(
                    time_i = time_i, time_len = time_var_num_cols_length,
                    subj_col = subj_cat_col,
                    key_current = i, key_total = column_keys_length,
                    subj_i = subj_i, subj_len = subj_cat_cols_length
                  ))
                }

                # get the time summaries with the subsetted data
                time_var_num_sum <- summarize_time_varying_num(subset_dt, time_var_num_col, group_by_fn = group_by_fn, group_duration = group_duration)

                list(
                  time_bins = time_var_num_sum$time_bins
                )
              }
            ) ->
            column_ans

            names(column_ans) <- column_keys
            column_ans
          }
        ) ->
        subj_ans

        names(subj_ans) <- subj_cat_cols
        subj_ans

        time_info <- ret[[time_var_num_col]]
        time_info[["subject-level-cat"]] <- subj_ans

        if (verbose) {
          cat("saving column: ", time_var_num_col, "\n")
        }
        to_file(
          time_info,
          file = save_name,
          pretty = pretty
        )
        if (verbose) {
          cat("saving column: ", time_var_num_col, " - DONE\n")
        }

        # return nothing
        time_i
      }
    )

  }

  other_cols <- names(ret)
  other_cols <- other_cols[!(other_cols %in% time_var_num_cols)]

  if (verbose) {
    cat("\nsaving smaller files\n")
  }
  lapply(other_cols, function(other_col) {
    save_name <- file.path(data_name, paste(other_col, ".json", sep = ""))
    if (file.exists(save_name)) {
      if (verbose) {
        cat("returning early. Output already found for ", save_name, "\n")
      }
      return(NULL)
    }
    if (verbose) {
      cat("saving column: ", other_col, "\n")
    }
    to_file(
      ret[[other_col]],
      file = save_name,
      pretty = pretty
    )
    if (verbose) {
      cat("saving column: ", other_col, " - DONE\n")
    }
    NULL
  })

  TRUE
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
  agedays_max = 365 * 2
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
  # num_distro <- sdd[colnames(sdd) %in% numeric_subject_columns]

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
      format = "summarise / cat [:bar] :percent :elapsed:-:eta (:category/:key) :spin",
      clear = FALSE
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

    # row <- col_key_combos[combo_pos, ]
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
