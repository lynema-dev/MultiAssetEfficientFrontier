#In this example,
#Efficient Frontier of 4 securities, where return data is 
#15 years of monthly returns that are randomly generated
#and range between -10% and +10%. 2000 portfolio
#(this model can be scaled to any number of securities
#by changing the securities parameter)

random_weights <- function(n)
{
  x <- runif(n,0,1)
  y <- x / sum(x)
  return(y)
}

months <- 15 * 12
returnlimit <- 0.1
securities <- 4
portfolios <- 2000
initialprice <- 100

colTypesList <- rep("double", 3 + securities)
colNamesList <- c("Return", "Risk", "IR")
colNamesList_ <- rep("p", securities + 1)

#get the mean return for each of the securities
#and the covariance matrix
for (s in 1:securities)
{
  colNamesList <- c(colNamesList, paste("weight_",s))
  colNamesList_[s] <- paste("security_",s)
  returns <- runif(months, -returnlimit, returnlimit)
  mean_return <- mean(returns)
  if (s == 1)
  {
    ret <- returns
    mean_ret <- mean_return
  }
  else
  {
    ret <- cbind(ret,returns)
    mean_ret <- cbind(mean_ret, mean_return)
  }
}

colNamesList_[securities+1] <- "Optimal Portfolio"
covar <- cov(ret)

#build a price history for each of the months
prices <- rep(initialprice, securities+1)
lastprices <- prices
pricehistory <- prices
for (r in 1:months)
{
  for (s in 1:securities)
  {
      prices[s] <- lastprices[s] * (1 + ret[r,s])
  }
  lastprices <- prices
  pricehistory <- rbind(pricehistory, prices)
}

#calculate the return and risk for each of the portfolios
colTypesList <- rep("double", 3 + securities)
dfEfficientFrontier <- read.table(text = "", col.names = colNamesList, 
    colClasses = colTypesList)
maxir <- -1000 
xmaxir <- 0
ymaxir <- 0
optimalweights <- rep(0,securities)

for (m in 1:portfolios)
{
  w <- random_weights(securities)
  expectedreturn <- sum(w*mean_ret) * 12
  sd <- sqrt(sum(covar*w*w)) * sqrt(12)
  ir <- expectedreturn / sd
  if (ir > maxir)
  {
    maxir <- ir
    ymaxir <- expectedreturn
    xmaxir <- sd
    optimalweights = w
  }
  resultrow <- c(expectedreturn, sd, ir, w)
  
  dfRow <- resultrow
  names(dfRow) <- colNamesList
  dfEfficientFrontier <- rbind(dfEfficientFrontier, dfRow)
}

#add the optimal portfolio back into the price history array
for (p in 1:length(pricehistory)/(securities+1))
{
  pricehistory[p,securities + 1] = sum(pricehistory[p,1:securities] * optimalweights)
}

#plot the price history
colourset = rainbow(securities+1)
plot(0:months,pricehistory[,1],col=colourset[1], 
     type = "l", ylim=c(0,round(max(pricehistory[,]))+10),
     xlab = "month", ylab = 'price', 
     main = "Individual Security and Optimal Portfolio Price Paths")
for (s in 2:securities)
{
  lines(0:months,pricehistory[,s],col=colourset[s])
}
lines(0:months,pricehistory[,securities+1],col = colourset[securities+1])
legend("topleft",colNamesList_, fill = colourset)

#plot the efficient frontier
x <- dfEfficientFrontier[,2]
y <- dfEfficientFrontier[,1]
plot(x, y, 
     main = paste("Efficient Frontier (",portfolios," portfolios, ",securities," securities)"),
     xlab = "Annualised Risk", ylab = "Annualised Return",
     pch = 19, frame = TRUE, col = ifelse(y == ymaxir, "green", "grey"),
     cex = ifelse(y == ymaxir, 2, 1),
     xlim = c(min(x)-0.01,max(x)+0.01), ylim = c(min(y)-0.02,max(y)+0.02))
legend("topleft","Optimal Portfolio", fill = "green")

#print of weights of optimal portfolio
print("Weights of Optimal Portfolio :")
print(optimalweights)
print(paste("Information Ratio = ", round(maxir,3)))



