#' Puts the various parts of speech together into a full phrase.
#'
#' @param vec A vector of doubles
#'
#' @return A string stating the guess for the distribution of the vectors
#'
#' @import forecast
#' @import dplyr
#'
#' @export

distribution_guess= function(vec){
score_exp = 0
score_norm = 0
score_uni = 0
#Test 1 where are the extreme values
res = sum_stat(vec, .95)
low = (vec <= res$lower_bound)
# Number of fails for low
low = length(vec) - as.numeric(summary(low)[[2]])

high = vec >= res$upper_bound 
# Number of fails for low
high = length(vec) - as.numeric(summary(high)[[2]])
if (low == 0 & high == 0){
  score_uni = score_uni + 1
}
if (low == 0 & high > 3){
  score_exp = score_exp + 1
}
if (low > 0 & high > 0){
  score_norm = score_norm + 1
}

  #Test 2 relative slopes
  rand_min = min(vec)
  rand_max = max(vec)
  rand_mean = mean(vec)
  vec_low = as.data.frame(vec) %>%
  filter(vec < rand_mean) %>%
  mutate(
    num = case_when(
      vec > (3*rand_mean+rand_min)/4 ~ 3,
      vec > (rand_mean+rand_min)/2 ~ 2,
      vec >= rand_min ~ 1
    )
  )
  vec_high = as.data.frame(vec)%>%
  filter(vec >= rand_mean) %>%
  mutate(
    num = case_when(
      vec < (3*rand_mean+rand_max)/4 ~ 3,
      vec < (rand_mean+rand_max)/2 ~ 2,
      vec <= rand_max ~ 1
    )
  )
  low_val = mean(vec_low$num)
  high_val = mean(vec_high$num)


if (low_val > 1.75 & high_val > 1.75){
score_norm = score_norm + 1
} 
if (low_val < 2.1 & high_val < 2.1){
  score_uni = score_uni + 1
}
if (low_val < 1.8 & high_val < 2.1){
  score_exp = score_exp + 1
}

  #Test 3
mid1 = round(length(vec)/3)
mid2 = round(2*length(vec)/3)
time = ts(vec, start = c(1), end = c(length(vec)), frequency = 1)
low = window(time, end = c(mid1))
mid = window(time, start = c(mid1), end = c(mid2))
high = window(time, start = c(mid2))
low_slope = tslm(low~ trend)[[1]][[2]]
mid_slope = tslm(mid~ trend)[[1]][[2]]
high_slope = tslm(high~ trend)[[1]][[2]]

if (low_slope *1.5  > mid_slope & low_slope*1.5 > high_slope & mid_slope * 1.5 > low_slope & mid_slope *1.5 >high_slope & high_slope*1.5 > low_slope & high_slope*1.5 > mid_slope ){
  score_uni = score_uni + .1
} 
if (low_slope > 2*mid_slope | high_slope > 2*mid_slope) {
  if(low_slope >mid_slope & high_slope > mid_slope){
    score_norm = score_norm + .1
  }
}
if (4*low_slope < high_slope){
  score_exp = score_exp + .1
}

#Evaluation
if (score_uni > score_norm & score_uni > score_exp){
  return("uniform")
}else if (score_norm > score_uni & score_norm > score_exp){
  return("normal")
}else if(score_exp > score_norm & score_exp > score_uni){
  return("exponential")
}else{
  return("unknown")
}
}
