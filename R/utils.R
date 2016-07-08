
count_info <- function(dt, col_name, keys) {

  dt[[col_name]] %>%
    # force it to be a character before making it a factor
    as.character() %>%
    # make sure all keys show up
    factor(levels = keys) %>%
    # make sure to keep the NA values if they appear
    table(useNA = "ifany") %>%
    as.data.frame() %>%
    as.tbl() %>%
    set_colnames(c("key", "count")) ->
  ret

  # force it to be a character (also useful when adding the "..na.." value)
  ret$key <- as.character(ret$key)

  if (any(is.na(ret$key))) {
    ret$key[is.na(ret$key)] <- "..na.."
  } else {
    # didn't see any NA values

    if (any(is.na(keys))) {
      # some key is an NA value
      ret %>%
        rbind(
          data.frame(
            key = "..na..",
            count = 0,
            stringsAsFactors = FALSE
          ) %>% as.tbl()
        ) ->
      ret
    }
  }



  return(ret)

  # ret <- count_(dt, col_name)
  #
  # colnames(ret) <- c("key", "count")
  # if (any(is.na(keys))) {
  #   keys[is.na(keys)] <- "..na.."
  #   ret$key[is.na(ret$key)] <- "..na.."
  # }
  # ret$key <- as.character(ret$key)
  #
  # # make sure they are in the same order
  # ret_order <- purrr::map(ret$key, ~ which(. == keys)) %>% unlist()
  # ret <- ret[ret_order, ]
  #
  # rownames(ret) <- ret$key
  # ret <- as.tbl(as.data.frame(ret)[keys, ])
  # ret$key <- keys
  # ret[is.na(ret$count), "count"] <- 0
  # rownames(ret) <- NULL
  #
  # ret
}


vtype_list <- function(dt) {
  var_summ <- attr(dt, "hbgd")$var_summ
  ret <- as.list(var_summ$vtype)
  names(ret) <- var_summ$variable
  ret
}
