library(nimble)
library(MCMCvis)

cod <- nimbleCode({
  for (ii in 1:NsiteA) {
    for (ij in 1:NsiteB) {
      obs.dyad[ii, ij] ~ dbinom(dyad[ii, ij], prob = (detectA[ii]*detectB[ij]))
      dyad[ii, ij] ~ dpois(40)
    }
  }
})

mod <- nimbleModel(code=cod,
                   data=list(obs.dyad=dyad.obs, detectA=detectA, detectB=detectB),
                   constants = list(NsiteA=Nsite, NsiteB=Nsite)
)

myMCMC <- buildMCMC(mod, monitors=c("dyad"))

mod.comp <- compileNimble(mod, showCompilerOutput = T)
comp <- compileNimble(myMCMC)


samps <- runMCMC(mcmc = comp, niter=2000, nburnin = 1000, nchains = 2)


MCMCsummary(samps)     
MCMCtrace(samps)

matrix(MCMCsummary(samps)$mean, nrow=5)

plot(trans, MCMCsummary(samps)$mean)
cor.test(as.vector(trans), MCMCsummary(samps)$mean)

plot(rank(trans), rank(MCMCsummary(samps)$mean), xlab = "True number", ylab="Estimated number")
