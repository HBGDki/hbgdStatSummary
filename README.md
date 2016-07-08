# hbgdStatSummary
summary statistics of datasets

# Installation

```{r}
# clone directory and 'cd' into it
devtools::install()
```

# Example Usage

```{r}
dt <- hbgd::cpp # or any other data.frame

# summarize data and produce a json string
json <- summarize_dataset_json(dt)

# skip data checks
json <- summarize_dataset_json(dt, check = FALSE)

# save output to a file
json <- summarize_dataset_file(dt)
```
