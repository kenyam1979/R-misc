library(tidyverse)
library(semTools)
library(semPlot)

## Data prep
#set.seed(1234)
X <- rnorm(50)
M <- 4*X + rnorm(50)  # M = a*X + e
Y <- 7*M + rnorm(50)  # Y = b*M + e
data <- bind_cols(X=X, M=M, Y=Y)

data %>% ggplot(aes(x=X, y=M)) + geom_point()

## Linear model
m1 <- lm(Y~X, data=data)
m2 <- lm(M~X, data=data)
m3 <- lm(Y~M, data=data)

m1$coefficients
m2$coefficients
m3$coefficients

m1$coefficients[2] # c*
m2$coefficients[2] * m3$coefficients[2] # a*b (mediate)


## SEM based indirect effect analysis
m <- '
  Y ~ b*M
  M ~ a*X
  ab := a*b'

fit <- sem(m, data=data)
summary(fit)
semPaths(fit, whatLabels='par')



rm(list=ls(all.names=TRUE))
## Reference
## https://ides.hatenablog.com/entry/2020/08/11/021904


## Supplement: Evaluation of mediate a*b with bootstrap
library(boot)
func <- function(x, i) {
  m2 <- lm(M~X, data=x[i, ])
  m3 <- lm(Y~M, data=x[i, ])
  ab = m2$coefficients[['X']] * m3$coefficients[['M']]
  return(ab)
}
b <- boot(data, func, R=1000)
plot(b)
