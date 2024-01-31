review <- rnorm(5)
print(review)
sharpe <- function
    (x, rf = .00003) {
     (mean(x) -
       rf) / sd(x)
 }
lapply(review, FUN=sharpe, .00007)
