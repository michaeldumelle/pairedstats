summary.pairedscalar <- function(object, ...){
  paired_data_split <- split(object, list(.pair1 = object$.pair1, .pair2 = object$.pair2), drop = TRUE)
  paired_data_summary <- lapply(paired_data_split, function(x) {
    unq_df <- unique(x[, c(".pair1", ".pair2"), drop = FALSE])
    unq_df$n_pairs <- length(x$metric)
    unq_df$Min <- min(x$metric)
    unq_df$Q1 <- quantile(x$metric, 0.25)
    unq_df$Median <- quantile(x$metric, 0.5)
    unq_df$Mean <- mean(x$metric)
    unq_df$Q3 <- quantile(x$metric, 0.75)
    unq_df$Max <- max(x$metric)
    unq_df
  })
  paired_stats_summary <- do.call("rbind", paired_data_summary)
  paired_stats_summary <- paired_stats_summary[with(paired_stats_summary, order(.pair1, .pair2)), , drop = FALSE]
  row.names(paired_stats_summary) <- NULL
  paired_stats_summary
}


summary.corrclass <- function(object, p.adjust = "none", ...){
  paired_data_split <- split(object, list(.pair1 = object$.pair1, .pair2 = object$.pair2), drop = TRUE)
  paired_data_summary <- lapply(paired_data_split, function(x) {
    unq_df <- unique(x[, c(".pair1", ".pair2", "n_pairs"), drop = FALSE])
    if (is.na(x$metric)) {
      unq_df$Cor_Est <- NA
      unq_df$p_value <- NA
    } else {
      unq_df$Cor_Est <- x$metric[[1]]$estimate
      unq_df$p_value <- x$metric[[1]]$p.value
    }
    unq_df
  })
  paired_stats_summary <- do.call("rbind", paired_data_summary)
  paired_stats_summary <- paired_stats_summary[with(paired_stats_summary, order(.pair1, .pair2)), , drop = FALSE]
  if (p.adjust != "none") {
    paired_stats_summary$p_value_adj <- p.adjust(paired_stats_summary$p_value, method = p.adjust, n = sum(!is.na(paired_stats_summary$p_value)))
  }
  row.names(paired_stats_summary) <- NULL
  paired_stats_summary
}
