% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarize_dataset.R
\name{summarize_dataset_file}
\alias{summarize_dataset_file}
\title{Summarize full dataset and save to file}
\usage{
summarize_dataset_file(..., file, pretty)
}
\arguments{
\item{...}{args passed directly to \code{summarize_dataset_json}}

\item{file}{file to save to}

\item{pretty}{boolean to determine if the json should be pretty printed}
}
\description{
Create a full object that summarizes both subject level and time varying variables, then saves the json results to a file
}
