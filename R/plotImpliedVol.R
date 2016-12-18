#' Implied Volatility of Option on Future
#'
#' This function allows you to calculate implied volatility of option on future using Fortran subroutine "zeroin".
#' 
#' At the end, a plot and a dataframe of implied volatility will be returned.
#' @param df A dataframe containing 6 columns with specific column names: strick, type, optionPrice, future Price, time_to_expiry, interest_rate.
#' @export
#' @examples
#' df <- data.frame(strike = c(50, 20),type = c("C", "P"), optionPrice = c(1.62,0.01),futurePrice = c(48.03, 48.03),time_to_expiry = c(0.1423, 0.1423), interest_rate = c(0.01,0.01))
#' plotImpliedVol(df)

plotImpliedVol <- function(df){
  
  #Black76 formula
  Black76 <- function(F, K, T, r, sig, type="C"){
    
    d1 <- (log(F/K) + (sig^2/2)*T) / (sig*sqrt(T))
    d2 <- d1 - sig*sqrt(T)
    value <- ifelse(type == "C", exp(-r*T) * (F*pnorm(d1) - K*pnorm(d2)),
                    exp(-r*T) * (K*pnorm(-d2) - F*pnorm(-d1)))
    return(value)
  }
  
  #check input information 
  if(nrow(df) == 0||!is.data.frame(df)) stop("Input type error!")
  
  #define a vector for implied volatility
  v <- numeric(nrow(df))
  
  #for loop to get option data
  for(i in 1:nrow(df)){
    
    K <- df$strike[i]    #the option strike in $
    Ty <- df$type[i]   #the option type
    P <- df$optionPrice[i]    #the option price in $
    F <- df$futurePrice[i]   #the price of the underlying future in $
    T <- df$time_to_expiry[i]    #the option time to expiry in year
    r <- df$interest_rate[i]    #the interest rate
    
    #Calculate the difference between Black76 price and market price
    if(Ty == "C") implied_vol <- function(sig) Black76(F, K, T, r, sig, "C") - P
    else implied_vol <- function(sig) Black76(F, K, T, r, sig, "P") - P
    
    #Get the implied volatility which makes diffrence calculated above equal to 0, based on Fortran subroutine "zeroin"
    v[i] <- uniroot(implied_vol, c(-10, 10), tol = 0.001)$root
    
  }
  
  #append implied volatility to input dataframe
  implied_vol_data <- data.frame(df, implied_vol = v)
  
  #plot
  if(nrow(df) > 50)
  {
    #smooth 
    lo <- loess(implied_vol ~ strike, data = implied_vol_data)
    
    plot(implied_vol_data[ , c("strike", "implied_vol")], col = "blue", 
         xlab = "Strike", ylab = "Implied Vol", main = "Implied Volatility of Option on future")
    lines(predict(lo), col = "red")
    legend("bottomleft", cex = 0.9, c("Implied Vol", "Vol Smile"), lty = 1, col = c("blue", "red"))
  }
  else
  {
    plot(implied_vol_data[order(implied_vol_data$strike), c("strike", "implied_vol")], col = "blue", type = "l",
         xlab = "Strike", ylab = "Implied Vol", main = "Implied Volatility of Option on future")
    legend("bottomleft", cex = 0.9, "Implied Vol", lty = 1, col = "blue")
  }
  
  #return implied volatility
  return(implied_vol_data)
}