moe_prop <- function(num, denom, moe_num, moe_denom) {

  prop <- num / denom

  x <- moe_num^2 - (prop^2 * moe_denom^2)

  result <- ifelse(x < 0, moe_ratio(num = num, denom = denom, moe_num = moe_num, moe_denom = moe_denom),
                   sqrt(x) / denom)

  return(result)
}


moe_ratio <- function(num, denom, moe_num, moe_denom) {

  r2 <- (num / denom)^2

  mn2 <- moe_num^2

  md2 <- moe_denom^2

  result <- (sqrt(mn2 + (r2 * md2))) / denom

  return(result)

}