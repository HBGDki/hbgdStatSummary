% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_dataset.R
\name{summarize_dataset_with_time_varying_subsets_and_save_four}
\alias{summarize_dataset_with_time_varying_subsets_and_save_four}
\title{Summarise subject level info per cateogry
'}
\usage{
summarize_dataset_with_time_varying_subsets_and_save_four(dt, data_name,
  check = TRUE, group_duration = "week", verbose = TRUE,
  agedays_min = -365, agedays_max = 365 * 2, pretty = FALSE,
  parallel_cores = 1)
}
\arguments{
\item{dt}{dataset to summarizes}

\item{check}{boolean to determine if \code{check_data()} should be performed}

\item{group_duration}{string of one of \code{c("week", "month", "quarter", "year")}}

\item{verbose}{boolean to determine if progress bars should be displayed}

\item{agedays_min, agedays_max}{min and max agedays allowed}
}
\description{
Summarise subject level info per cateogry
'
}
