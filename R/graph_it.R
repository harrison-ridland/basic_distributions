#' Puts the various parts of speech together into a full phrase.
#'
#' @param vec A vector of doubles
#'
#' @return A histogram with overlayed model prediction
#'
#' @import forecast
#' @import dplyr
#' @import ggplot2
#'
#' @export

graph_it = function(vec){
  type = distribution_guess(vec)
  vecdf = as.data.frame(vec)
  density = (max(vec)-min(vec))
  means = mean(vec)
  if (type == "uniform"){
    graph = ggplot(data = vecdf) +
geom_histogram(mapping = aes(x = vec, y = ..density..), bins = 12)+
  stat_function(fun= ~dunif(.x,min = min(vec), max = max(vec)), col = "cornflowerblue", lwd = 2)
  }
  if (type == "normal" | type == "unknown"){
    sds = sd(vec)
    graph = ggplot(data = vecdf) +
geom_histogram(mapping = aes(x = vec, y = ..density..), bins = 12)+
  stat_function(fun= ~dnorm(.x,mean = means, sd = sds), col = "cornflowerblue", lwd = 2)
  }
  if (type == "exponential"){
    graph = ggplot(data = vecdf) +
geom_histogram(mapping = aes(x = vec, y = ..density..), bins = 12)+
  stat_function(fun= ~dexp(.x,rate = 1/means), col = "cornflowerblue", lwd = 2)
  }
  return(graph)
}
