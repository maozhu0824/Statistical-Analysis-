# 1.1
datapath <- "C:/Users/AA/Desktop/Statistical Analysis/Class5/"
dat <- read.csv(file = paste(datapath, "ResidualAnalysisProjectData_2.csv", sep = "/"), header = TRUE, sep = ",")
head(dat)

plot(dat$Input, dat$Output, type = "p", pch = 19)

nSample <- length(dat$Input)


#1.2 
m1 <- lm(Output ~ Input, dat) 
m1$coefficients

matplot(dat$Input, cbind(dat$Output, m1$fitted.values), type = "p", pch = 16,
        ylab = "Sample and Fitted Values")

summary(m1)

estimatedResiduals <- m1$residuals
plot(dat$Input, estimatedResiduals)

Probability.Density.Residuals <- density(estimatedResiduals)
plot(Probability.Density.Residuals, ylim = c(0,0.5))
lines(Probability.Density.Residuals$x, dnorm(Probability.Density.Residuals$x,
                                             mean = mean(estimatedResiduals),
                                             sd = sd(estimatedResiduals)))


#1.3 
Train.Sample <- data.frame(trainInput = dat$Input, trainOutput = rep(NA, nSample))
Train.Sample.Steeper <- data.frame(trainSteepInput = dat$Input, 
                                      trainSteepOutput = rep(NA, nSample))
Train.Sample.Flatter <- data.frame(trainFlatInput = dat$Input, 
                                      trainFlatOutput = rep(NA, nSample))

head(cbind(dat, Train.Sample, Train.Sample.Steeper, Train.Sample.Flatter))


# Create Selectors 

Train.Sample.Selector <- dat$Input >= 5
Train.Sample.Steeper.Selector <- Train.Sample.Selector & (dat$Output > m1$fitted.values)
Train.Sample.Flatter.Selector <- Train.Sample.Selector & (dat$Output <= m1$fitted.values)

# Select subsamples 
Train.Sample[Train.Sample.Selector,2] <- dat[Train.Sample.Selector,2]
Train.Sample.Steeper[Train.Sample.Steeper.Selector, 2] <- dat[Train.Sample.Steeper.Selector,2]
Train.Sample.Flatter[Train.Sample.Flatter.Selector,2] <- dat[Train.Sample.Flatter.Selector,2]
head(Train.Sample)


head(cbind(dat,
           Train.Sample,
           Train.Sample.Steeper,
           Train.Sample.Flatter),10)


plot(Train.Sample$trainInput,Train.Sample$trainOutput, pch = 16, ylab = "Training
     Sample Output", xlab = "Training Sample Input")
points(Train.Sample.Steeper$trainSteepInput, Train.Sample.Steeper$trainSteepOutput,
       pch=20, col = "green")
points(Train.Sample.Flatter$trainFlatInput, Train.Sample.Flatter$trainFlatOutput,
       pch = 20, col = "blue")


# 1.4 
Train.Sample.Steep.lm <- lm(Train.Sample.Steeper$trainSteepOutput~Train.Sample.Steeper$trainSteepInput
                            ,data = Train.Sample.Steeper)

summary(Train.Sample.Steep.lm)$coefficients

summary(Train.Sample.Steep.lm)$sigma

summary(Train.Sample.Steep.lm)$df

summary(Train.Sample.Steep.lm)$r.squared

summary(Train.Sample.Steep.lm)$adj.r.squared

summary(Train.Sample.Steep.lm)$fstatistic



Train.Sample.Flat.lm <- lm(Train.Sample.Flatter$trainFlatOutput~Train.Sample.Flatter$trainFlatInput
                            ,data = Train.Sample.Flatter)

summary(Train.Sample.Flat.lm)$coefficients

summary(Train.Sample.Flat.lm)$sigma

summary(Train.Sample.Flat.lm)$df

summary(Train.Sample.Flat.lm)$r.squared

summary(Train.Sample.Flat.lm)$adj.r.squared

summary(Train.Sample.Flat.lm)$fstatistic


rbind(Steeper.Coefficients = Train.Sample.Steep.lm$coefficients,
      Flatter.Coefficients = Train.Sample.Flat.lm$coefficients)

plot(dat$Input, dat$Output, type = "p", pch = 19)
lines(dat$Input,predict(Train.Sample.Steep.lm, data.frame(trainSteepInput = dat$Input),
                        interval = "prediction")[,1], col = "red",lwd = 3)
   
lines(dat$Input,predict(Train.Sample.Flat.lm, data.frame(trainFlatInput = dat$Input),
                        interval = "prediction")[,1], col = "green",lwd = 3)

    
    
# Define distances from each Output point to both estimated training lines
Distances.to.Steeper <- abs(dat$Output - dat$Input*Train.Sample.Steep.lm$coefficients[2]-
                                Train.Sample.Steep.lm$coefficients[1])
Distances.to.Flatter <- abs(dat$Output - dat$Input*Train.Sample.Flat.lm$coefficients[2]-
                                Train.Sample.Flat.lm$coefficients[1])


# Define the unscramble sequence 
Unscrambling.Sequence.Steeper <- Distances.to.Steeper < Distances.to.Flatter

# Define two subsampleswith NAs in the Output columns. 
Subsample.Steeper <- data.frame(steeperInput = dat$Input, steeperOutput = rep(
    NA,nSample))
Subsample.Flatter <- data.frame(flatterInput = dat$Input, flatterOutput = rep(
    NA, nSample))

# Fill in the unscrambled outputs instead of NAs where necessary. 
Subsample.Steeper[Unscrambling.Sequence.Steeper,2]<-dat[Unscrambling.Sequence.Steeper,2]
Subsample.Flatter[!Unscrambling.Sequence.Steeper,2] <-dat[!Unscrambling.Sequence.Steeper,2]

# Check the first rows
head(cbind(dat, Subsample.Steeper, Subsample.Flatter))

# Plot the unscrambled subsamples, include the original entire sample as a check.
matplot(dat$Input, cbind(dat$Output,
                         Subsample.Steeper$steeperOutput,
                         Subsample.Flatter$flatterOutput),
        type = "p", col = c("black", "green", "blue"),
        pch = 16, ylab = "Separated Subsamples")

# Mixing Probability of Steeper Slope. 
(Mixing.Probability.Of.Steeper.Slope <- sum(Unscrambling.Sequence.Steeper)/
    length(Unscrambling.Sequence.Steeper))

binom.test(x = sum(Unscrambling.Sequence.Steeper), n = nSample, p = 0.5,
           alternative = c("two.sided"),
           conf.level = 0.95)


Linear.Model.Steeper.Recovered <- lm(Subsample.Steeper$steeperOutput~Subsample.Steeper$steeperInput,
                                     data = Subsample.Steeper)
Linear.Model.Flatter.Recovered <- lm(Subsample.Flatter$flatterOutput~Subsample.Flatter$flatterInput,
                                     data = Subsample.Flatter)

rbind(Steeper.Coefficients = Linear.Model.Steeper.Recovered$coefficients,
      Flatter.Coefficients = Linear.Model.Flatter.Recovered$coefficients)

summary(Linear.Model.Steeper.Recovered)$r.sq

summary(Linear.Model.Flatter.Recovered)$r.sq


# 1.6
# Plot residuals 
matplot(dat$Input, cbind(c(summary(Linear.Model.Steeper.Recovered)$residuals,
                           summary(Linear.Model.Flatter.Recovered)$residuals),
                         estimatedResiduals), type = "p", pch = c(19,16), 
        ylab = "Residuals before and after unscrambling")
legend("bottomleft", legend = c("Before", "After"), col = c("red", "black"),
          pch = 16)


# Estimate standard deviations
unmixedResiduals <- c(summary(Linear.Model.Steeper.Recovered)$residuals,
                      summary(Linear.Model.Flatter.Recovered)$residuals)
apply(cbind(ResidualsAfter = unmixedResiduals, 
            ResidualsBefore = estimatedResiduals),2,sd)


suppressWarnings(library(fitdistrplus))

hist(unmixedResiduals)

(residualsParam <- fitdistr(unmixedResiduals,"normal"))

ks.test(unmixedResiduals,"pnorm", residualsParam$estimate[1], residualsParam$estimate[2])

qqnorm(unmixedResiduals)

qqline(unmixedResiduals)


# Slopes
c(Steeper.Slope = Linear.Model.Steeper.Recovered$coefficients[2], 
  Flatter.Slope = Linear.Model.Flatter.Recovered$coefficients[2])

#Intercepts 
c(Steeper.Intercept = Linear.Model.Steeper.Recovered$coefficients[1],
  Flatter.Intercept = Linear.Model.Flatter.Recovered$coefficients[1])



# 2.

plot(dat$Input, (dat$Output - mean(dat$Output))^2, type = "p", pch = 19,
                 ylab = "Squared Deviations")


m1 <- lm(Output ~ Input, dat)
m1$coefficients

yMean <- m1$coefficients[2] * mean(dat$Input) + m1$coefficients[1]

yi <- m1$coefficients[2] * (dat$Input) + m1$coefficients[1]

clusteringParabola <- (yi - yMean)^2

plot(dat$Input, (dat$Output - mean(dat$Output))^2, type = "p", pch = 19, 
     ylab = "Squared Deviations")

points(dat$Input, clusteringParabola,pch = 19, col = "red")
    

Unscrambling.Sequence.Steeper.var <- rep(FALSE, 1000)

for (i in 1:length(Unscrambling.Sequence.Steeper)){
    if ((dat$Output[i] - mean(dat$Output))^2 > clusteringParabola[i]) 
        {Unscrambling.Sequence.Steeper.var[i] = TRUE}}

head(Unscrambling.Sequence.Steeper.var,10)

Subsample.Steeper.var <- data.frame(steeperInput.var = dat$Input, 
                                    steeperOutput.var = rep(NA, nSample))
Subsample.Flatter.var <- data.frame(flatterInput.var = dat$Input,
                                    flatterOutput.var = rep(NA, nSample))

Subsample.Steeper.var[Unscrambling.Sequence.Steeper.var,2] <- dat[Unscrambling.Sequence.Steeper.var,2]
Subsample.Flatter.var[!Unscrambling.Sequence.Steeper.var,2] <- dat[!Unscrambling.Sequence.Steeper.var,2]

#Check the first 10 rows 
head(cbind(dat, Subsample.Steeper.var, Subsample.Flatter.var), 10)

plot(dat$Input,(dat$Output - mean(dat$Output))^2, type = "p", pch = 19, 
     ylab = "Squared Deviations")
points(dat$Input, clusteringParabola, pch = 19, col = "red")
points(dat$Input[Unscrambling.Sequence.Steeper.var],
       (dat$Output[Unscrambling.Sequence.Steeper.var] - 
           mean(dat$Output))^2, pch = 19, col = "blue")
points(dat$Input[!Unscrambling.Sequence.Steeper.var], 
       (dat$Output[!Unscrambling.Sequence.Steeper.var] - 
            mean(dat$Output))^2, pch = 19, col = "green")


excludeMiddle <- (dat$Input <= mean(dat$Input) - 0)|
                  (dat$Input >= mean(dat$Input) + 0)
matplot(dat$Input[excludeMiddle],cbind(dat$Output[excludeMiddle],
                                       Subsample.Steeper.var$steeperOutput.var[excludeMiddle],
                                       Subsample.Flatter.var$flatterOutput.var[excludeMiddle]),
         type = 'p', col = c("black","green","blue"),
         pch = 16, ylab = "Separated Subsamples")


excludeMiddle1 <- (dat$Input <= mean(dat$Input) - 0.5)|
    (dat$Input >= mean(dat$Input) + 0.5)
matplot(dat$Input[excludeMiddle1],cbind(dat$Output[excludeMiddle1],
                                       Subsample.Steeper.var$steeperOutput.var[excludeMiddle1],
                                       Subsample.Flatter.var$flatterOutput.var[excludeMiddle1]),
        type = 'p', col = c("black","green","blue"),
        pch = 16, ylab = "Separated Subsamples")


SteeperInput <- dat$Input[excludeMiddle1]
SteeperOutput <- Subsample.Steeper.var$steeperOutput.var[excludeMiddle1]
dat.Steep.var <- lm(SteeperOutput~SteeperInput)

FlatterInput <- dat$Input[excludeMiddle1]
FlatterOutput <- Subsample.Flatter.var$flatterOutput.var[excludeMiddle1]
dat.Flat.var <- lm(FlatterOutput~FlatterInput)

rbind(Steeper.Coefficients.var = dat.Steep.var$coefficients,
      Flatter.Coefficients.var = dat.Flat.var$coefficients)
summary(dat.Steep.var)
summary(dat.Flat.var)



plot(dat$Input, dat$Output, type = 'p', pch = 19)

lines(SteeperInput,
      dat.Steep.var$coefficients[2]*SteeperInput+dat.Steep.var$coefficients[1],
      col = "red", lwd = 3)
lines(FlatterInput,
      dat.Flat.var$coefficients[2]*FlatterInput+dat.Flat.var$coefficients[1],
      col = "green", lwd = 3)



# 3. 
Slope_Estimate1 <- sum(dat$Output*(dat$Input - mean(dat$Input)))/
                  sum((dat$Input - mean(dat$Input))^2)

Slope_Estimate2 <- cov(dat$Output,dat$Input)/var(dat$Input)

rbind(Slope_Estimate1,Slope_Estimate2)
                   






#4. Test
dataPath <- "C:/Users/AA/Desktop/Statistical Analysis/Class5/"

dat1 <- read.table(paste(dataPath,'Week5_Test_Sample.csv',sep = '/'),header =TRUE)


# Plot the dataset to take a look at the distribution
plot(dat1$Input, dat1$Output, type = "p", pch = 19)

nSampleTest <- length(dat1$Input)


# Generate the regression 
GeneralModel <- lm(dat1$Output ~ dat1$Input, data = dat1)


# Take a look at the summary of the model and coefficients
summary(GeneralModel)

GeneralModel$coefficients


# Plot the residuals
Estimated.Residuals1 <- GeneralModel$residuals

plot(dat1$Input,Estimated.Residuals1)

Probability.Density.Residuals1 <- density(Estimated.Residuals1)

plot(Probability.Density.Residuals1, ylim = c(0, 1))

lines(Probability.Density.Residuals1$x, 
      dnorm(Probability.Density.Residuals1$x, 
      mean = mean(Estimated.Residuals), 
      sd = sd(Estimated.Residuals)))



# Plot the squared deviations
yMean1 <- GeneralModel$coefficients[2] * mean(dat1$Input) + GeneralModel$coefficients[1]

yFitted <- GeneralModel$coefficients[2] * (dat1$Input) + GeneralModel$coefficients[1]

clusteringParabola1 <- (yFitted - yMean1)^2

plot(dat1$Input, (dat1$Output - mean(dat1$Output))^2, type = "p", pch = 19, 
     ylab = "Squared Deviations")

points(dat1$Input, clusteringParabola1,pch = 19, col = "red")


# Define the separating sequnce, and make it TRUE for steeper subsample and 
# FALSE for flatter subsample 

Unscrambling.Sequence.Steeper.var1 <- rep(FALSE, 1000)

for (i in 1:1000){
    if ((dat1$Output[i] - mean(dat1$Output))^2 > clusteringParabola1[i]) 
    {Unscrambling.Sequence.Steeper.var1[i] = TRUE}}

head(Unscrambling.Sequence.Steeper.var1,30)



# Separate the sample into steeper and flatter part. 
Subsample.Steeper.var1 <- data.frame(steeperInput.var1 = dat1$Input, 
                                    steeperOutput.var1 = rep(NA, nSample))
Subsample.Flatter.var1<- data.frame(flatterInput.var1 = dat1$Input,
                                    flatterOutput.var1 = rep(NA, nSample))




# Fill in the unscrambled outputs instead of NAs where necessary. 
Subsample.Steeper.var1[Unscrambling.Sequence.Steeper.var1,2] <- 
    dat1[Unscrambling.Sequence.Steeper.var1,1]

Subsample.Flatter.var1[!Unscrambling.Sequence.Steeper.var1,2] <- 
    dat1[!Unscrambling.Sequence.Steeper.var1,1]


#Check the first 10 rows 
head(cbind(dat1, Subsample.Steeper.var1, Subsample.Flatter.var1), 10)


# Plot clusters of the variance data and the separating parabola
plot(dat1$Input,(dat1$Output - mean(dat1$Output))^2, type = "p", pch = 19, 
     ylab = "Squared Deviations")
points(dat1$Input, clusteringParabola1, pch = 19, col = "red")
points(dat1$Input[Unscrambling.Sequence.Steeper.var1],
       (dat1$Output[Unscrambling.Sequence.Steeper.var1] - 
            mean(dat1$Output))^2, pch = 19, col = "blue")
points(dat1$Input[!Unscrambling.Sequence.Steeper.var1], 
       (dat1$Output[!Unscrambling.Sequence.Steeper.var1] - 
            mean(dat1$Output))^2, pch = 19, col = "green")


# Plot the unscrambled subsamples, include the original entire sample as a 
# check. 
matplot(dat1$Input, cbind(dat1$Output,
                         Subsample.Steeper.var1$steeperOutput.var1,
                         Subsample.Flatter.var1$flatterOutput.var1),
        type = "p", col = c("black", "green", "blue"),
        pch = 16, ylab = "Separated Subsamples")



excludeMiddle2 <- (dat1$Input <= mean(dat1$Input) - 0)|
    (dat1$Input >= mean(dat1$Input) + 0)
matplot(dat1$Input[excludeMiddle2],cbind(dat1$Output[excludeMiddle2],
                                       Subsample.Steeper.var1$steeperOutput.var1[excludeMiddle2],
                                       Subsample.Flatter.var1$flatterOutput.var1[excludeMiddle2]),
        type = 'p', col = c("black","green","blue"),
        pch = 16, ylab = "Separated Subsamples")

matplot(dat1$Input[excludeMiddle2],cbind(dat1$Output[excludeMiddle2],
                                        Subsample.Steeper.var1$steeperOutput.var1[excludeMiddle2],
                                        Subsample.Flatter.var1$flatterOutput.var1[excludeMiddle2]),
        type = 'p', col = c("black","green","blue"),
        pch = 16, ylab = "Separated Subsamples")


# Fit linear models to the separated samples. 
SteeperInput1 <- dat1$Input[excludeMiddle2]
SteeperOutput1 <- Subsample.Steeper.var1$steeperOutput.var1[excludeMiddle2]
mSteep <- lm(SteeperOutput1~SteeperInput1)

FlatterInput1 <- dat1$Input[excludeMiddle2]
FlatterOutput1 <- Subsample.Flatter.var1$flatterOutput.var1[excludeMiddle2]
mFlat <- lm(FlatterOutput1~FlatterInput1)


# Print estimated parameters and summaries of both models. 
summary(mSteep)

summary(mFlat)


# Plot residuals from the combined model and the models for separated samples. 
matplot(dat1$Input[excludeMiddle2],
        cbind(c(summary(mSteep)$residuals,
                summary(mFlat)$residuals),
              estimatedResiduals[excludeMiddle2]),
        type = "p", pch = c(19,16), ylab = "Residuals before and after unscrabling")


res <- list(GeneralModel = GeneralModel, mSteep = mSteep, mFlat = mFlat)

saveRDS(res,file = paste(dataPath, 'result.rds', sep = '/'))




