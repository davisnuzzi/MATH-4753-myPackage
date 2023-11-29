myncurve = function(mu, sigma, a)
{
  # define range
  x_range <- c(mu - 3 * sigma, a)

  # create curve
  curve(dnorm(x, mean = mu, sd = sigma), xlim = x_range, main = "Normal Distribution Curve")

  # shade area under the curve from -inf to a
  polygon(c(x_range[1], seq(x_range[1], a, length.out = 100), a),
          c(0, dnorm(seq(x_range[1], a, length.out = 100), mean = mu, sd = sigma), 0),
          col = "firebrick")

  # calculate probability
  probability <- integrate(function(x) dnorm(x, mean = mu, sd = sigma), -Inf, a)$value

  # print probability
  cat("Probability (P(X <= a)):", round(probability, 4), "\n")
}

myclt=function(n,iter){
  y=runif(n*iter,0,5)
  data=matrix(y,nrow=n,ncol=iter,byrow=TRUE)
  sm=apply(data,2,sum)
  hist(sm)
  sm
}

mymaxlik=function(lfun,x,param,...){
  # how many param values are there?
  np=length(param)
  # outer -- notice the order, x then param
  # this produces a matrix -- try outer(1:4,5:10,function(x,y) paste(x,y,sep=" "))   to understand
  z=outer(x,param,lfun)
  # z is a matrix where each x,param is replaced with the function evaluated at those values
  y=apply(z,2,sum)

  # y is a vector made up of the column sums
  # Each y is the log lik for a new parameter value
  plot(param,y,col="Blue",type="l",lwd=2,...)
  # which gives the index for the value of y == max.
  # there could be a max between two values of the parameter, therefore 2 indices
  # the first max will take the larger indice
  i=max(which(y==max(y)))
  abline(v=param[i],lwd=2,col="Red")

  # plots a nice point where the max lik is
  points(param[i],y[i],pch=19,cex=1.5,col="Black")
  axis(3,param[i],round(param[i],2))
  #check slopes. If it is a max the slope shoud change sign from + to
  # We should get three + and two -vs
  ifelse(i-3>=1 & i+2<=np, slope<-(y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]),slope<-"NA")
  return(list(i=i,parami=param[i],yi=y[i],slope=slope))
}


myci <- function(data)
{
  # Calculate sample mean and standard deviation
  sample_mean <- mean(data)
  sample_sd <- sd(data)

  # Number of observations
  n <- length(data)

  # Degrees of freedom
  df <- n - 1

  # T-score for 95% confidence interval
  t_score <- stats::qt(0.975, df)

  # Margin of error
  margin_error <- t_score * (sample_sd / sqrt(n))

  # Confidence interval for the mean
  ci_mean <- c(sample_mean - margin_error, sample_mean + margin_error)

  # Return the result
  return(ci_mean)
}



