set.seed(847337465)
Binomial.sample<-rbinom(500,size=20,prob=.7)
# Binomial.sample

Estimate.p <- mean(Binomial.sample)/20
Estimate.p

suppressWarnings(fitdistrplus)
Binomial.fit <- fitdist(Binomial.sample, dist = "binom", fix.arg = list(size = 20), 
                        start = list(prob = 0.5))
c(Binomial.fit$estimate, sd = Binomial.fit$sd)

#Ho: p = 0.7   Ha: p!= 0.7
pLessthan6923 <- pbinom(6923, size = 500*20, prob = 0.7, lower.tail = TRUE)
pLessthan6923

pMorethan7077 <- pbinom(7077, size = 500*20, prob = 0.7, lower.tail = FALSE)
pMorethan7077

#p-value calculation
sumTailprobabilities <- pLessthan6923 + pMorethan7077
sumTailprobabilities

#binom.test
binom.test(sum(Binomial.sample), 500*20, p = 0.7)

#confidence interval 
c(Binomial.fit$estimate - 1.96*Binomial.fit$sd, Binomial.fit$estimate+1.96*
      Binomial.fit$sd)

# Testing with binom.test conducts"exact binomial test". If the number of Bernoulli
# trials is large binomial probabilities become close to normal probabitlies due
# to central limit theorem. Then pro.test() could be used

pbinom(sum(Binomial.sample), size = 500*20, prob = 0.7)

pnorm(sum(Binomial.sample), 500*20*0.7, sqrt(500*20*0.7*0.3))

prop.test(sum(Binomial.sample), 500*20, p = 0.7)



# Poisson Distribution
# Simulation and estimation
set.seed(847337465)
Poisson.sample <- rpois(500, 7)
Estimate.lambda <- mean(Poisson.sample)
Estimate.lambda

Poisson.fit <- fitdistr(Poisson.sample,"Poisson")
c(Poisson.fit$estimate, sd = Poisson.fit$sd)

(t <- table(Poisson.sample))

(Sample.frequencies <- t/sum(t))

(Poisson.probabilities <- dpois(as.numeric(names(t)), 7))

matplot(as.numeric(names(t)),cbind(Sample.frequencies, Poisson.probabilities),
        type = "l", lty = 1, lwd = 2, ylab = "Probabilities", xlab = "Counts")
legend("topright", legend = c("Estimated", "Theoretical"), lty = 1, col = c(
    "black", "red"))

chisq.test(Sample.frequencies, Poisson.probabilities)
# chisq.test() checks hypothesis of equivalence between the sample frequencies
# and theoretical probabilities. 


# 3. Exponential Distribution: 
set.seed(847337465)
Exponential.sample <- rexp(500, 0.3)
Exponential.lambda <- 1/mean(Exponential.sample)
Exponential.lambda

Exponential.fit <- fitdistr(Exponential.sample, "exponential")
c(Exponential.fit$estimate, sd = Exponential.fit$sd)

#Use Kolmogorov-Smirnov test for goodness of fit
ks.test(Exponential.sample, "pexp", 0.3)
c(Exponential.fit$estimate - 1.96*Exponential.fit$sd, Exponential.fit$estimate
  +1.96*Exponential.fit$sd)


# 4. Normal Distribution
set.seed(847337465)
Normal.sample <- rnorm(500,3,1.5)
Normal.mu <- mean(Normal.sample)
Normal.sigma <- sqrt(var(Normal.sample))
c(mu = Normal.mu, sigma = Normal.sigma)

Normal.fit <- fitdistr(Normal.sample, "normal")
rbind(param = Normal.fit$estimate, param.sd = Normal.fit$sd)

ks.test(Normal.sample,"pnorm", 3, 1.5)


# Example 4.5.1
(z <- (4.96 - 5)/(0.05/sqrt(10)))
pnorm(z)
2 * pnorm(z)
2*(1-pnorm(abs(z)))


# 5. Uniform distribution
set.seed(847337465)
Uniform.sample <- runif(500,3,4.5)
Uniform.mu <- mean(Uniform.sample)
Uniform.sigma <- sd(Uniform.sample)
Uniform.B <- Uniform.mu + sqrt(3)*Uniform.sigma
Uniform.A <- Uniform.mu - sqrt(3)*Uniform.sigma
c(Uniform.A, Uniform.B)

Uniform.fit <- fitdist(Uniform.sample, "unif")
Uniform.fit

ks.test(Uniform.sample, "punif", 3, 4.5)




























