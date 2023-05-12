library(nimble)
library(MCMCvis)

set.seed(69)

# model code
cod <- nimbleCode({
  # loop through origins and destinations
  for (ii in 1:NsiteA) {
    for (ij in 1:NsiteB) {
      # each observation is related to detection probabilities
      # at both origin and destination
      obs.dyad[ii, ij] ~ dbinom(dyad[ii, ij], prob = (detectA[ii]*detectB[ij]))
      
      # prior for number of movements - model is sensitive to this
      dyad[ii, ij] ~ dpois(40)
    }
  }
})

# run model providing observations of dyads and true detections
mod <- nimbleModel(code=cod,
                   data=list(obs.dyad=dyad.obs, detectA=detectA, detectB=detectB),
                   constants = list(NsiteA=Nsite, NsiteB=Nsite)
)

# configure MCMC
myMCMC <- buildMCMC(mod, monitors=c("dyad"))


# compile code
mod.comp <- compileNimble(mod, showCompilerOutput = T)
comp <- compileNimble(myMCMC)

# run model
samps <- runMCMC(mcmc = comp, niter=5000, nburnin = 4000, nchains = 2)

# check posteriors and traceplots
MCMCsummary(samps)     
MCMCtrace(samps)

# correlation between true and estimated values
cor.test(as.vector(trans), MCMCsummary(samps)$mean)

# plot
plot(trans, MCMCsummary(samps)$mean,
     xlab = "True number", ylab="Estimated number")
text(x=47.5, y=35, labels=paste("cor = ", round(cor(as.vector(trans), MCMCsummary(samps)$mean), 2)))
# abline(0, 1, lty=3)
# text(50, 50, labels=expression("y=x"))
