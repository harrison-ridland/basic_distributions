  
#' Puts the various parts of speech together into a full phrase.
#'
#' @param vec A vector of doubles
#'
#' @return A model showing the distribution of the vector
#'
#' @import forecast
#' @import ggvis
#' @import dplyr
#'
#' @export

model_predict = function(vec){
  vec = sort(vec)
  vecdf = data.frame(index = 1:length(vec), vec) 
  model = vecdf %>% ggvis(~index, ~vec) %>% layer_points() %>%   layer_model_predictions(model = "loess", se = TRUE)
  return(model)
}
