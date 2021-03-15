library(tidyverse)
library(semTools)
library(semPlot)

## Data prep
set.seed(1234)
X <- rnorm(100)
M <- 0.5*X + rnorm(100)  # M = a*X + e
Y <- 0.7*M + 0.3*X + rnorm(100)  # Y = c*X + b*M + e
data <- bind_cols(X=X, M=M, Y=Y)

## Linear model
m1 <- lm(Y~X, data=data)
m2 <- lm(M~X, data=data)
m3 <- lm(Y~X+M, data=data)

m1$coefficients
m2$coefficients
m3$coefficients

m1$coefficients[2] # c*
m2$coefficients[2] * m3$coefficients[3] # a*b (mediate)
m3$coefficients[2] + m2$coefficients[2] * m3$coefficients[3]  # total = c + a*b



## SEM based indirect effect analysis
m <- '
  Y ~ c*X + b*M
  M ~ a*X
  ab := a*b
  total := c + (a*b)'

fit <- sem(m, data=data)
summary(fit)
semPaths(fit, whatLabels='par')


## Parameter estimation with Monte Carlo for a*b
med <- 'a*b'
aparam <- coef(fit)[["a"]]
bparam <- coef(fit)[["b"]]
AC <- vcov(fit)[c(2,3),c(2,3)]

monteCarloMed(med, coef1=aparam, coef2=bparam, outputValues=FALSE, plot=TRUE, ACM=AC)


## Reference
## https://ides.hatenablog.com/entry/2020/08/11/021904