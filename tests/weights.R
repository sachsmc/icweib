data(tooth24)   ## load data
## Stratified on dmf, and sex as explanatory variable 
fit <- icweib(L = left, R = right, data = tooth24, strata = dmf, covariates = ~sex)

## weighted

tooth24$wts <- runif(nrow(tooth24))
fit2 <- icweib(L = left, R = right, data = tooth24, strata = dmf, covariates = ~sex, weights = wts)

## compare to survreg

fit3 <- icweib(L = left, R = right, data = tooth24, strata = dmf, covariates = ~ sex, weights = wts)
fit3a <- icweib(L = left, R = right, data = tooth24, covariates = ~ sex, weights = wts)

tooth24$right2 <- ifelse(!is.finite(tooth24$right), NA, tooth24$right)
fit4 <- survreg(Surv(time = left, time2 = right2, type = "interval2") ~ sex, weights = tooth24$wts, data = tooth24)

out1<-fit4
shape1<-1/sum(out1$scale)
scale1<-exp(sum(out1$coefficients)- out1$coefficients[2])
scale2<-exp(sum(out1$coefficients))
H1<-(shape1/scale1)*(168/scale1)^(shape1-1)
H2<-(shape1/scale2)*(168/scale2)^(shape1-1)

H2 / H1 - exp(fit3a$coef[1]) < 1e-4
