pairedstats <- function(.data, .var, .val, .group, .fun = "difference", .pair1 = NULL, .pair2 = NULL, .pair_expand = TRUE, .fun_aggregate = "mean", .slim = TRUE, ...){

  # change factors to characters
  .data[] <- lapply(.data, function(x){
    if (is.factor(x)) {
      x <- as.character(x)
    } else {
      x
    }
  })

  # take .pair1 to be all unique values if no values supplied
  if (is.null(.pair1)) {
    .pair1 <- unique(.data[[.var]])
  }

  # take .pair2 to be .pair1 if no values supplied
  if (is.null(.pair2)) {
    .pair2 <- .pair1
  }

  # create index data with available pair combinations
  index_data <- get_index_data(.pair1, .pair2, .pair_expand)

  # split index data by copy and no copy
  index_split <- split(index_data, index_data[["copy"]])

  # take no copies
  index_nocopy <- index_split$`FALSE`

  # find unique values to use in subsetting
  unique_pairvals <- unique(c(.pair1, .pair2))

  # find unique groups
  unique_groups <- unique(.data[[.group]])

  # subset data to only include relevant values and columns
  .data <- .data[.data[[.var]] %in% unique_pairvals, c(.var, .val, .group), drop = FALSE]

  # aggregate data for a summary statistic by fun_aggregate
  aggregated_data <- aggregate(
    x = list(.val = .data[[.val]]),
    by = list(.var = .data[[.var]], .group = .data[[.group]]),
    FUN = .fun_aggregate,
    drop = TRUE,
    ...
  )


  paired_data <- get_paired_data(index_nocopy, aggregated_data, unique_groups)

  paired_splits <- split(paired_data, paired_data$index)

  stats_list <- lapply(paired_splits, .fun, ...)

  stats_df <- do.call("rbind", stats_list)

  pairedstats_data <- get_pairedstats_data(stats_df, .slim, .fun, index_data, index_nocopy)



  if (.fun %in% c("difference" , "difference_log", "ratio", "ratio_log")) {
    pairedstats_class <- "pairedscalar"
    pairedstats_data <- pairedstats_data[, c(".pair1", ".pair2", ".group", ".val1", ".val2", "metric"), drop = FALSE]
    colnames(pairedstats_data) <- c(".pair1", ".pair2", .group, paste0("agg_", .val, "1"), paste0("agg_", .val, "2"), "metric")
  } else if (.fun == "correlation") {
    pairedstats_class <- "corrclass"
    pairedstats_data <- pairedstats_data[, c(".pair1", ".pair2", "n_pairs", "metric"), drop = FALSE]
    if (any(is.na(pairedstats_data$metric))) message("Warning: At least one set of pairs had fewer than 3 observations -- returning NA's for that pair")
    #colnames(pairedstats_data) <- c(".pair1", ".pair2", "metric")
  }

  pairedstats_data <- pairedstats_data[with(pairedstats_data, order(.pair1, .pair2)), , drop = FALSE]
  pairedstats_data <- structure(pairedstats_data, class = c(pairedstats_class, class(pairedstats_data)))
}



### helpers

difference <- function(paired_split) {
  paired_split$metric <- paired_split[[".val1"]] - paired_split[[".val2"]]
  paired_split[, c("index", ".group", ".val1", ".val2", "metric"), drop = FALSE]
}

difference_log <- function(paired_split) {
  if (any(paired_split[[".val1"]] < 0) || any(paired_split[[".val2"]] < 0)) {
    stop("Negative values cannot be logged")
  }
  paired_split$metric <- log(paired_split[[".val1"]]) - log(paired_split[[".val2"]])
  paired_split[, c("index", ".group", ".val1", ".val2", "metric"), drop = FALSE]
}

ratio <- function(paired_split) {
  paired_split$metric <- paired_split[[".val1"]] / paired_split[[".val2"]]
  paired_split[, c("index", ".group", ".val1", ".val2", "metric"), drop = FALSE]
}

ratio_log <- function(paired_split) {
  if (any(paired_split[[".val1"]] < 0) || any(paired_split[[".val2"]] < 0)) {
    stop("Negative values cannot be logged")
  }
  paired_split$metric <- log(paired_split[[".val1"]]) / log(paired_split[[".val2"]])
  paired_split[, c("index", ".group", ".val1", ".val2", "metric"), drop = FALSE]
}

correlation <- function(paired_split, ...) {
  paired_split_small <- unique(paired_split[, c("index", ".pair1", ".pair2"), drop = FALSE])
  paired_split_small$n_pairs <- nrow(paired_split)
  if (nrow(paired_split) >= 3) {
    paired_split_small$metric <- list(cor.test(paired_split$.val1, paired_split$.val2, ...))
  } else {
    paired_split_small$metric <- NA
  }
  paired_split_small[, c("index", ".pair1", ".pair2", "n_pairs", "metric"), drop = FALSE]
}

get_pairedstats_data <- function(stats_df, .slim, .fun, index_data, index_data_nocopy) {
  nonpaircols <- which(!(colnames(stats_df) %in% c(".pair1", ".pair2")))
  if (.slim) {
    index_data_nocopy_small <- index_data_nocopy[, c("index", "copy", ".pair1", ".pair2"), drop = FALSE]
    pairedstats_data <- merge(index_data_nocopy_small,stats_df[, nonpaircols, drop = FALSE], by = "index")
  } else {
    index_data_small <- index_data[, c("index", "copy" , ".pair1", ".pair2"), drop = FALSE]
    pairedstats_data <- merge(index_data_small, stats_df[, nonpaircols, drop = FALSE], by = "index")
    if (.fun %in% c("difference", "difference_log")) {
      pairedstats_data$metric <- ifelse(pairedstats_data$copy, -1 * pairedstats_data$metric, pairedstats_data$metric)
    }
    if (.fun %in% c("ratio", "ratio_log")) {
      pairedstats_data$metric <- ifelse(pairedstats_data$copy, 1 / pairedstats_data$metric, pairedstats_data$metric)
    }
  }
  pairedstats_data
}

get_index_data <- function(.pair1, .pair2, .pair_expand) {

  if (.pair_expand) {
    index_data <- expand.grid(.pair2 = .pair2, .pair1 = .pair1, stringsAsFactors = FALSE)
    index_data <- index_data[, c(".pair1", ".pair2"), drop = FALSE]
  } else {
    index_data <- data.frame(.pair1 = .pair1, .pair2 = .pair2, stringsAsFactors = FALSE)
  }

  # remove pairs with self
  index_data <-  index_data[index_data$.pair1 != index_data$.pair2, , drop = FALSE]

  # create pair index
  index_data$index <- paste0(
    pmin(index_data$.pair1, index_data$.pair2),
    pmax(index_data$.pair1, index_data$.pair2)
  )

  # create a duplicated indicator (FALSE = not a copy)
  index_data$copy <- duplicated(index_data$index)

  # return
  index_data
}

get_paired_data <- function(index_nocopy, aggregated_data, unique_groups) {
  # get expanded index data
  expand_index <- expand.grid(index = index_nocopy$index, .group = unique_groups, stringsAsFactors = FALSE)

  # create pair data
  expand_index$.pair1 <- substring(expand_index$index, 1, 1)
  expand_index$.pair2 <- substring(expand_index$index, 2, 2)

  paired_data <- merge(
    expand_index,
    aggregated_data,
    by.x = c(".pair1", ".group"),
    by.y = c(".var", ".group")
  )
  paired_data <- merge(
    paired_data,
    aggregated_data,
    by.x = c(".pair2", ".group"),
    by.y = c(".var", ".group"),
    suffix = c("1", "2")
  )

  paired_data
}
