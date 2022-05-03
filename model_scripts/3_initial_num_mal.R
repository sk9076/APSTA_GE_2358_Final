# Human population
Sh0 <- 1000000 # susceptible
Eh0 <- 100 # Exposed
Ih0 <- 100 # Infectious
Rh0 <- 100 # Recovered/Immuned
Ch0 <- 0  # Cerebral malaria
Dh0 <- 0 # Death


# Mosquito population
Sv0 <- 1000000 # Susceptible mosquito
Ev0 <- 1000000 # Exposed mosquito
Iv0 <- 1000000 # Infectious mosquito


init_mal <- c(
  Sh = Sh0,
  Eh = Eh0,
  Ih = Ih0,
  Rh = Rh0,
  Ch = Ch0,
  Dh = Dh0,
  Sv = Sv0,
  Ev = Ev0,
  Iv = Iv0
)