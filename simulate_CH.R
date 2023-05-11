require(magrittr)

### true states

Nind = 1000
Nsite = 5

detectA <- runif(Nsite, 0.5, 0.9)
detectB <- runif(Nsite, 0.5, 0.9)

states <- data.frame(
  individual = 1:Nind,
  siteA = sample(1:Nsite, Nind, T),
  siteB = sample(1:Nsite, Nind, T)
)


trans <- matrix(0, nrow=Nsite, ncol=Nsite)
dyad.obs <- matrix(0, nrow=Nsite, ncol=Nsite)

obsA <- obsB <- rep(0, Nsite)

ii <- 1
for (ii in 1:Nind) {
  states$obsA[ii] <- rbinom(n=1, size=1, prob=detectA[states$siteA[ii]])
  states$obsB[ii] <- rbinom(n=1, size=1, prob=detectB[states$siteB[ii]])
  states$obs.dyad[ii] <- states$obsA[ii] * states$obsB[ii]
  
  trans[states$siteA[ii], states$siteB[ii]] %<>% add(1)
  dyad.obs[states$siteA[ii], states$siteB[ii]] %<>% add(states$obs.dyad[ii])
  
  obsA[states$siteA[ii]] %<>% add(states$obsA[ii])
  obsB[states$siteB[ii]] %<>% add(states$obsB[ii])
}

plot(dyad.obs, trans)
plot(dyad.obs, trans*detectA)
plot(dyad.obs, trans*detectB)

