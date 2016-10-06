

# @param dt time varying dataset
# @param col_name time varying column in question
# @group_by_fn function to calculate the age group given the ageweeks
# @group_duration title for the group_by_fn
summarize_time_varying_cat <- function(
  dt, col_name,
  group_by_fn,
  group_duration
) {

  # calc the age group and nest the data accordingly
  dt %>%
    select_("ageweeks", col_name) %>%
    mutate(agegroup = group_by_fn(ageweeks)) %>%
    group_by_("agegroup") %>%
    nest() ->
  by_group

  # for each group, count the keys for the col_name
  # then get the count list and name it with the age group
  all_keys <- sort(unique(dt[[col_name]]), na.last = TRUE) # nolint
  by_group %>%
    mutate(
      counts = purrr::map(data, ~ count_info(., col_name, all_keys))
    ) %>%
    extract2("counts") %>%
    set_names(by_group$agegroup) ->
  count_list

  list(
    id = col_name,
    type = "time-varying-cat",
    group_duration = group_duration,
    time_bins = count_list
  )
}
