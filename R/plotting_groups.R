#' Create plots for each score intrasubject (time) and intersubjects (condition)
#'
#' @param df a dataframe
#' @param date a character object
#' @param group a character object
#' @param var a character vector
#' @importFrom ggplot2 geom_boxplot position_jitterdodge scale_color_brewer
#' @return a plot
#' @export
#'
plotting_groups <- function (df, date, group, var) {
  plot_list = list()

  for (i in seq_along(var)) {
    p <- df %>%
    ggplot() +
    aes_string(x = date, y = var[i], color = group) +
    geom_boxplot(alpha = .5, outlier.colour = NA) +
    geom_jitter(size = 5, alpha = .5, position = position_jitterdodge(dodge.width=.7, jitter.width = .2)) +
    stat_summary(fun = mean, geom = "point", size = 3, shape = 4) +
    stat_summary(fun = mean, aes_string(group = group), geom = "line") +
    labs(title = paste0("Mesure  ", var[i]), y = "Score") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_color_brewer("Groupe", palette = "Set1")
    plot_list[[i]] = p

  }
  plot_list
}
