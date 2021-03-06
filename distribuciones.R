curve(dnorm(x,1,1), -10,10)
curve(dgamma(x,1,1), 0, 10)
curve(dexp(x, rate = 2), 0, 20)
curve(dcauchy(x,location = 0, scale = 1), -10, 10)
curve(dt(x, df = 30, ncp = 0), -10, 10)
curve(dchisq(x, df = 4,ncp = 0), 0, 20)
curve(df(x, df1 = 2, df2 = 2, ncp = 0), 0,10)
curve(dunif(x, min = 0, max = 1))
curve(dlogis(x, location = 0, scale = 1), -10, 10)
curve(dlnorm(x, meanlog = 0, sdlog = 1), 0, 10)
curve(dbeta(x, shape1 = 1, shape2 = 0.1, ncp = 0.1), 0, 1)
curve(dpois(x, lambda = 2), 0, 100)
curve(dbinom(x, size = 100, prob = 0.2), 0, 100)


p1 <- ggplot(data = data.frame(x = c(0, 1)), aes(x)) +
    stat_function(fun = dbeta, n = 101, args = list(shape1 = 0.0001, shape2 = 0.0001), colour = "blue", lwd = 1) + ylab("") +
    scale_y_continuous(breaks = NULL)
p1


