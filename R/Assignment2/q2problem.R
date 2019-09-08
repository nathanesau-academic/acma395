# Author: Nathan Esau
# Date: Sept 27, 2016
# Course: Acma 395
# Purpose:
# 
# 1. Demonstrate that spliced distribution in Question 2 doesn't integrate to 1
# 2. Determine a spliced distribution which does integrate to 1
# 3. Compare the two spliced distributions in a plot

# Splice a vector of n functions together using n - 1 breaks
# 
# Input:
# c - a vector of breaks (length n - 1)
# fns - a vector of functions (length n)
#
# Ouput:
# a continuous spliced function

splice <- function(c, fns, range=c(0,Inf)) { 
  cat("\nSolving for weights... \n")
  n = length(fns)
  breaks = c(range[1], c, range[2])
  pcw <- function(x) {
    g<-cut(x, breaks)
    unsplit(Map(function(f,x) f(x), fns, split(x, g)), g)
  }
  
  integrals = as.numeric(lapply(1:n, function(i) {force(i); integrate(pcw, breaks[i], breaks[i+1])$value }))
  ratios = as.numeric(lapply((1:(n-1)), function(i) {force(i); fns[[i+1]](c[i])/fns[[i]](c[i]) }))
  a = numeric(n)
  products = as.numeric(lapply( (1:(n-1)), function(i) {force(i); prod(ratios[i:(n-1)])}))
  a[n] = (sum(products*integrals[1:length(integrals)-1]) + integrals[length(integrals)])^(-1)
  
  for(i in (n-1):1) a[i] = a[i+1]*ratios[i] # backwards recursion  
  cat("solved weights: ", a, "\n\n") # print the solved weights
  fnsWeighted = lapply(1:n, function(i) {force(i); function(x) f[[i]](x)*a[i]})
  function(x)  {
    g<-cut(x, breaks)
    unsplit(Map(function(f,x) f(x), fnsWeighted, split(x, g)), g)
  }
}

# Plot the spliced distribution determined by ``splice`` and the
# original functions in one plot. The breaks will be plotted as vertical
# dotted lines.

splice.plot <- function(fns, g, c, range=c(0, 75), title="", ylim=c(0, 0.1)) {
  n = length(fns)
  breaks = c(range[1], c, range[2])
  plot(g, range[1], range[2], main=title, ylim=ylim, ylab='f(x)')
  lapply(1:n, function(i) {force(i); plot(fns[[i]], breaks[i], breaks[i+1], col=i+1, lty=3, add=TRUE) })
  lapply((1:(n-1)), function(i) {force(i); plot(fns[[i]], breaks[i+1], range[2], col=i+1, add=TRUE)})
  lapply(n:2, function(i) {force(i); plot(fns[[i]], range[1], breaks[i], col=i+1, add=TRUE) })
  abline(v=c, lty=3)
}

f1 <- function(x) { # Gamma (2, 8)
  1 / 64 * exp(-x/8) * x
}

f2 <- function(x) { # Gamma (4, 8)
  1 / 24576 * exp(-x/8) * x^3
}

f4 <- function(x) { # Spliced Gamma (2, 8) and Gamma (4, 8) with a1 = a2 = 0.5
  ifelse(x <  19.5, 1 / 128 * exp(-x/8) * x,
         1 / 49152 * exp(-x/8) * x^3)
}

# doesn't integrate to 1 when weights are 0.5, 0.5
cat("integral of pdf from 0 to Inf with a1 = a2 = 0.5: ", 
    integrate(f4, 0, Inf)$value, "\n")

f <- c(f1, f2)
c <- 19.5 # break
g <- splice(c, f)

# integrates to 1 and continuous when weights are 0.6765508, 0.6832229
cat("integral of pdf from 0 to Inf with a1 = 0.6765508, a2 = 0.6832229: ", 
    integrate(g, 0, Inf)$value, "\n")

# see plot of splicing distributions
splice.plot(f, g, c) # plot spliced distribution with weights a1 = 0.6765508, a2 = 0.6832229
plot(f4, 0, 75, col = 4, lty = 1, add=TRUE)

legend('topright', c("Gamma(2, 8)", "Gamma(4, 8)", "Splice a1 = a2 = 0.5",
                     "Splice a1 = 0.67655, a2 = 0.68233"),
       col = c('red', 'green', 'blue', 'black'), lty = c(3, 3, 1, 1), cex = 1.0)