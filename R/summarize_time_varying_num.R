
# @param nested_dt subset of a time varying dataset
# @param time_var column name in question
# @param order highest order for datadr's calculate moments
get_moments <- function(nested_dt, col_name, order) {
  x <- nested_dt[[col_name]]
  res <- datadr::calculateMoments(x, order)
  if (all(is.na(res))) {
    res <- list(M1 = NA, M2 = NA, n = 0)
  }

  res$na_count <- sum(is.na(x))
  # res$col_name <- time_var
  do.call(data_frame, res)
}



# @param dt time varying dataset
# @param col_name time varying column in question
# @group_by_fn function to calculate the age group given the ageweeks
# @group_duration title for the group_by_fn
summarize_time_varying_num <- function(
  dt, col_name,
  group_by_fn,
  group_duration
) {

  print(col_name)

  # calc the age group and nest the data accordingly
  dt %>%
    select_("ageweeks", col_name) %>%
    mutate(agegroup = group_by_fn(ageweeks)) %>%
    group_by_("agegroup") %>%
    nest() ->
  by_group

  # for each group, calc the moments and ci
  by_group %>%
    extract2("data") %>%
    purrr::map(
      ~ get_moments(., col_name, 2)
    ) %>%
    purrr::map(
      ~ as.list(transform(.,
        mean = M1,
        sd = sqrt(M2 / (n - 1))
      ))
    ) %>%
    purrr::map(
      ~ as.list(transform(.,
        ci_lower = ifelse(is.na(mean) || n <= 1, NA,
          sd * qt(0.025, df = n - 1) / sqrt(n) + mean),
        ci_upper = ifelse(is.na(mean) || n <= 1, NA,
          sd * qt(0.975, df = n - 1) / sqrt(n) + mean)
      ))
    ) %>%
    set_names(by_group$agegroup) ->
  moment_list

  is_0_count <- sapply(moment_list, `[[`, "n") %>% magrittr::equals(0)
  if (any(is_0_count)) {
    moment_list <- moment_list[!is_0_count]
  }

  # {
  #   col_name: COLUMN_NAME,
  #   type: "time-varying-num",
  #   group_duration: one of "week", "four_weeks", "quarter", "year",
  #   time_bins: {
  #     AGEGROUP1: {
  #       M1: NUM,
  #       M2: NUM,
  #       n: NUM,
  #       mean: NUM,
  #       sd: NUM
  #     },
  #     ...
  #   }
  # }
  list(
    id = col_name,
    type = "time-varying-num",
    group_duration = group_duration,
    time_bins = moment_list
  )
}




# browser()
#
# # make it a tall data_frame
# moment_info %>%
#   bind_rows() %>%
#   mutate(
#     agegroup = by_week$ageweeks
#   ) ->
# moments_by_week
#
# browser()
#
# moment_info <- sd_and_mean(moments_by_week, group_by_fn, col_name)
# if (is.null(moment_info)) {
#   moment_list <- list()
# } else {
#   moment_info %>%
#     group_by_("agegroup") %>%
#     nest() %>%
#     mutate(
#       cmi = purrr::map(data, ~ as.list(.))
#     ) %>%
#     extract2("cmi") %>%
#     set_names(moment_info$agegroup) ->
#   moment_list
# }
#
#
#
# sd_and_mean <- function(moments_by_week, group_fn = group_by_week, col_name) {
#
#   combine_multi_moments <- function(x) {
#     if (length(x) > 1) {
#       x <- list(do.call(datadr::combineMultipleMoments, x))
#     }
#     x
#   }
#
#   moments_by_week %>%
#     filter(!is.na(M1)) ->
#   moments_by_week
#
#   if (nrow(moments_by_week) == 0) {
#     return(NULL)
#   }
#
#   moments_by_week %>%
#     group_by_("agegroup") %>%
#     nest() %>%
#     mutate(
#       agegroup = group_fn(agegroup),
#       moment_info = purrr::map(data, ~ as.list(.))
#     ) %>%
#     group_by_("agegroup") %>%
#     summarize(
#       cmi = combine_multi_moments(moment_info)
#     ) %>%
#     mutate(
#       cmi = purrr::map(cmi, ~ as_data_frame(.))
#     ) -> a
#
#   a %>%
#     unnest_("cmi") %>%
#     mutate(
#       mean = M1,
#       sd = sqrt(M2 / (n - 1))
#     ) ->
#   moment_info
#
#   moment_info
# }
