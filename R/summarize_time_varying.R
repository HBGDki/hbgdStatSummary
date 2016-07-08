
# @param dt time varying dataset
# @param var_types list of column variable types by column name
# @param group_duration string of "week", "four_weeks", "quarter" or "year"
summarize_time_varying <- function(dt, var_types, group_duration, verbose) {

  group_by_fn_ <- function(n) {
    function(ageweeks) {
      (floor( (ageweeks - 1) / n) + 1) * n
    }
  }
  group_by_fn <- switch(group_duration,
    "week" = identity,
    "four_weeks" = group_by_fn_(4),
    "quarter" = group_by_fn_(13),
    "year" = group_by_fn_(52)
  )

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
  ageweeks

  dt$ageweeks <- ageweeks

  # make sure the variables in question are everything but the time indicators
  vars <- colnames(dt)
  vars <- vars[! (vars %in% c("agedays", "ageweeks"))]

  summarize_with_fn_type(
    dt,
    vars = vars,
    var_types = var_types,
    fn_num = summarize_time_varying_num,
    fn_cat = summarize_time_varying_cat,
    verbose = verbose,
    group_by_fn = group_by_fn,
    group_duration = group_duration
  )
}
