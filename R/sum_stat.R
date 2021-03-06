#' Puts the various parts of speech together into a full phrase.
#'
#' @param vec A vector of doubles
#'
#' @return A dataframe with summary statistics of the vector
#'
#' @import dplyr
#'
#' @export

sum_stat = function(vec, confidence, guess = 0){
  means = mean(vec)
  std = sd(vec)
  confid = qnorm((1+confidence)/2)
  area_min = means - std*confid
  area_max = means + std*confid
  region = c(area_min, area_max)
  if (guess >= area_min & guess <= area_max){
    guess_result = TRUE
  }else{
    guess_result = FALSE
  }
  result = data.frame(mean = means, standard_deviation =std, lower_bound = area_min, upper_bound = area_max, includes_value = guess_result, value = guess)
  return(result)
}
