# R/calculate.R

#' Calculate Row Mean
#'
#' This function calculates the mean of all numeric columns for each row.
#'
#' @param data A data frame.
#' @return A data frame with an additional column `row_mean`.
#' @export
calculate_row_mean <- function(data) {
  data %>%
    rowwise() %>%
    mutate(row_mean = mean(c_across(where(is.numeric)), na.rm = TRUE)) %>%
    ungroup()
}

#' Calculate Row Sum
#'
#' This function calculates the sum of all numeric columns for each row.
#'
#' @param data A data frame.
#' @return A data frame with an additional column `row_sum`.
#' @export
calculate_row_sum <- function(data) {
  data %>%
    rowwise() %>%
    mutate(row_sum = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
    ungroup()
}


calculate_row_max <- function (data)
{
  data %>% rowwise() %>% mutate(row_nax = max(c_across(where(is.numeric)),
                                              na.rm = TRUE)) %>% ungroup()
}


