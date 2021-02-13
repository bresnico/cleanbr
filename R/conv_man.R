# Reverse scoring items  conv_man(df, "be", c(4,5,8), 8)
# ajout d'un select pour faciliter la gestion du mutate
conv_man <- function (df, x, items, key) {
  for (i in seq_along(items)) {
    temp_item <- paste0(x,"_",items[[i]])
    df <- df %>%
      mutate("{temp_item}" := key - pull(.,temp_item))
  }
  df
}
