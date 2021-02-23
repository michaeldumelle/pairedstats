pairedstats <- function(.data, .var, .val, .group, .fun = "mean", .p1 = NULL, .p2 = NULL, .pexpand = TRUE, .fun_aggregate = mean, slim = TRUE, ...){

  .data[] <- lapply(.data, function(x){
    if (is.factor(x)) {
      x <- as.character(x)
    } else {
      x
    }
  })

  if (is.null(.p1)) {
    .p1 <- unique(.data[[.var]])
  }

  if (is.null(.p2)) {
    .p2 <- .p1
  }



  .data <- .data[.data[[.var]] %in% unique(c(.p1, .p2)), c(.var, .val, .group), drop = FALSE]

  aggregated_data <- aggregate(x = list(.val = .data[[.val]]), by = list(.var = .data[[.var]], .group = .data[[.group]]), FUN = .fun_aggregate, drop = TRUE, ...)

  index_data <- get_index_data(.p1, .p2, .pexpand)

  split_index <- split(index_data, index_data$copy)

  nocopies <- split_index$`FALSE`

  expand_data <- expand.grid(index = nocopies$index, .group = unique(.data[[.group]]))

  expand_data$.p1 <- substring(expand_data$index, 1, 1)
  expand_data$.p2 <- substring(expand_data$index, 2, 2)

  expand_data <- merge(expand_data, aggregated_data, by.x = c(".p1", ".group"), by.y = c(.var, .group))
  expand_data <- merge(expand_data, aggregated_data, by.x = c(".p2", ".group"), by.y = c(.var, .group), suffix = c("1", "2"))

  expand_splits <- split(expand_data, expand_data$index)

  nocopies$value <- unlist(lapply(expand_splits, function(x) mean(x[[paste0(.val, "1")]] - x[[paste0(.val, "2")]]), ...))
  nocopies$pairs <- unlist(lapply(expand_splits, function(x) nrow(x)))
  if (slim) {
    final <- nocopies
  } else {
    final <- merge(index_data, nocopies[, c("index", "value", "pairs"), drop = FALSE])
    final$value <- ifelse(final$copy, -1 * final$value, final$value)
  }
  final[, c(".p1", ".p2", "value", "pairs"), drop = FALSE]
  # get pairs
  #paired_data <- get_paired_data(.data = .data, .var = .var, .val = .val, .group = .group, .p1 = .p1, .p2 = .p2, .pexpand = .pexpand, .fun_aggregate = .fun_aggregate, ...)

  # compute pair statistics
}

get_index_data <- function(.p1, .p2, .pexpand) {

  if (.pexpand) {
    index_data <- expand.grid(.p2 = .p2, .p1 = .p1, stringsAsFactors = FALSE)
  } else {
    index_data <- data.frame(.p1 = .p1, .p2 = .p2)
  }

  index_data <-  subset(index_data, .p1 != .p2)
  index_data$index <- paste0(pmin(index_data$.p1, index_data$.p2), pmax(index_data$.p1, index_data$.p2))
  index_data$copy <- duplicated(index_data$index)
  index_data
}

get_expand_data <- function(nocopy, group) {
  expand_data <- expand_grid(nocopy)
}

# get_paired_data <- function(.data, .var, .val, .group, .p1, .p2, .pexpand, .fun_aggregate, ...){
#
#
#
# }

# get_aggregated_data <- function(.data, .var, .val, .group, .fun_aggregate, ...) {
#   if (is.null(.group)) {
#     return(aggregate(x = list(.val = .data[[.val]]), by = list(.var = .data[[.var]]), FUN = .fun_aggregate, drop = FALSE, ...))
#   } else {
#     return(aggregate(x = list(.val = .data[[.val]]), by = list(.var = .data[[.var]], .group = .data[[.group]]), FUN = .fun_aggregate, drop = FALSE, ...))
#   }
# }


# 1. subset relevant values
# 2. get aggregated data
# 3. get expanded data
# 4. get merged data
# 4.a. only consider appropriate subset
# 5. split on .p1 and .p2
# 6. do the correct function
# 6.a. only to the appropriate subset
# 7 types could be difference, difference_log, ratio, ratio_log, correlation
