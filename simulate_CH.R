require(magrittr)

set.seed(69)

### true states

# number of individuals
Nind = 1000
Nsite = 5

## detection probabilties
# A = breeding, B = wintering
detectA <- runif(Nsite, 0.1, 0.9)
detectB <- runif(Nsite, 0.1, 0.9)

# which sites does each individual use
states <- data.frame(
  individual = 1:Nind,
  siteA = sample(1:Nsite, Nind, T),
  siteB = sample(1:Nsite, Nind, T)
)

# empty matrices to store results
trans <- matrix(0, nrow=Nsite, ncol=Nsite)
dyad.obs <- matrix(0, nrow=Nsite, ncol=Nsite)

obsA <- obsB <- rep(0, Nsite)

# loop through matrices filling observation histories
ii <- 1
for (ii in 1:Nind) {
  ## observations
  # sites
  states$obsA[ii] <- rbinom(n=1, size=1, prob=detectA[states$siteA[ii]])
  states$obsB[ii] <- rbinom(n=1, size=1, prob=detectB[states$siteB[ii]])
  # dyadic connections
  states$obs.dyad[ii] <- states$obsA[ii] * states$obsB[ii]
  
  # extract totals
  obsA[states$siteA[ii]] %<>% add(states$obsA[ii])
  obsB[states$siteB[ii]] %<>% add(states$obsB[ii])
  dyad.obs[states$siteA[ii], states$siteB[ii]] %<>% add(states$obs.dyad[ii])
  
  ## true number of movements
  trans[states$siteA[ii], states$siteB[ii]] %<>% add(1)
}

# plot relationships
plot(dyad.obs, trans)
plot(dyad.obs, trans*detectA)
plot(dyad.obs, trans*detectB)

