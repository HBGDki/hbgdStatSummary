
# @param dt subject level dataset
summarize_subject_level <- function(dt, var_types, verbose) {
  summarize_with_fn_type(
    dt,
    vars = colnames(dt),
    var_types = var_types,
    fn_num = summarize_subject_level_num,
    fn_cat = summarize_subject_level_cat,
    verbose = verbose
  )
}



# @param dt subject level dataset
# @param col_name numeric column in question
summarize_subject_level_num <- function(dt, col_name) {
  # get var data
  x <- dt[[col_name]]

  # get hist info
  is_na <- is.na(x)
  na_count <- sum(is_na)
  x <- x[!is_na]
  ans <- hist(x, plot = FALSE) %>% unclass()

  ans$xname <- NULL
  ans$equidist <- NULL
  ans$col_name <- col_name
  ans$mean <- mean(x, na.rm = TRUE)
  ans$sd <- sd(x, na.rm = TRUE)
  ans$na_count <- na_count
  ans$ci_lower <- ans$sd * qt(0.025, df = length(x) - 1) / sqrt(length(x)) + ans$mean
  ans$ci_upper <- ans$sd * qt(0.975, df = length(x) - 1) / sqrt(length(x)) + ans$mean
  ans$type <- "subject-level-num"
  ans$col_name <- col_name

  ans
}


# @param dt subject level dataset
# @param col_name categorical column in question
summarize_subject_level_cat <- function(dt, col_name) {
  list(
    col_name = col_name,
    type = "subject-level-cat",
    counts = count_info(dt, col_name, sort(unique(dt[[col_name]]), na.last = TRUE))
  )
}
