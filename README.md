# hbgdStatSummary
summary statistics of datasets

# Installation

```{r}
# clone directory and 'cd' into it
devtools::install("hbgd/hbgdStatSummary")
```

# Example Usage

```{r}
dt <- hbgd::cpp # or any other data.frame

# summarize data and produce a json string
json <- summarize_dataset(dt) %>% to_json()


# skip data checks
json <- summarize_dataset(dt, check = FALSE) %>% to_json()

# save output to a file
json <- summarize_dataset(dt) %>% to_file(file = "cpp_summary.json")

# summarize all numeric subject columns by each each of the categorical subject columns
json <- summarize_subject_per_category(dt) %>% to_json(pretty = TRUE)
```
