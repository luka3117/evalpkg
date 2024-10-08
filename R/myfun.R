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
  data %>% rowwise() %>% mutate(row_max = max(c_across(where(is.numeric)),
                                              na.rm = TRUE)) %>% ungroup()
}


convert_na_zero <- function(data) {
data %>% mutate(across(where(is.numeric), ~ if_else(is.na(.), 0, 1)))
}



convert_to_binary <- function(data) {
  data %>%
    mutate(across(where(is.numeric), ~ if_else(. == 0, 0, 1)))
}





typing_convert_to_range <- function(x) {
  case_when(
    x == 100 ~ 1,
    x >= 80 & x <= 99 ~ 0.9,
    x >= 60 & x <= 79 ~ 0.8,
    x >= 50 & x <= 59 ~ 0.7,
    x >= 40 & x <= 49 ~ 0.6,
    x >= 30 & x <= 39 ~ 0.5,
    x >= 0 & x <= 29 ~ 0.4,
    TRUE ~ 0
  )
}


convert_na_zero_only <-function (data)
{
  data %>% mutate(across(where(is.numeric), ~if_else(is.na(.),
                                                     0, .)))
}

convert_grade_factor_order <- function(x) {

  factor(x,
         levels = c("秀", "優", "良", "可", "不可", "不可(放棄)"),
         ordered = TRUE)
}



convert_score_to_grade<- function(x) {
  case_when(
    # x == 100 ~ 1,
    x >= 90 & x <= 100 ~ "秀",
    x >= 80 & x < 90 ~ "優",
    x >= 70 & x < 80 ~ "良",
    x >= 60 & x < 70 ~ "可",
    x >= 0 & x < 60 ~ "不可",
    x == 999 ~ "不可(放棄)"
    # ,
    # TRUE ~ 0
  )
}
