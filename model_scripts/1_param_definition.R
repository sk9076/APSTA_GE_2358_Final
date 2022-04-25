# Defining parameters

# Vaccine-related parameters

vac_daily <- 800000 # daily vaccine supply
ep <- 0.48 # 1-first dose efficacy(52%)
epv <- 0.05 # 1-second dose efficacy(95%)
tha <- 0.99 # Screening sensitivity (99%)

# Base parameters
c <- 6.58 # effective contact rate
betas <- 0.0225 # effective infectivity of symptomatics
betaa <- 0.5*betas # effective infectivity of asymptomatics

w <-  1/180 # waning effect of natural recovery

a <- 1/3.9591535 # latent period (incubation period) > CHANGE THIS

r <- 1/7.5 # total duration of mild infection  > CHANGE THIS
rs <- 1/19.84 # total duration of severe infection > CHANGE THIS
ra <- 1/5.11 # total duration of asymptomatic infection (infectious period) > CHANGE THIS
rh <- 1/11 # duration of hospitalization
yi <- 1/2.48 # pre-symptomatic period
p <- 0.85 # % of people developing symptomatic infection
q <- 0.2 # percentage developing severe symptoms
u <- 0.115 # COVID-19 mortality among the hospitalized > CHANGE THIS



# Adjustment following Moghadas et al,.
rq <- r*yi/(yi-r) # duration of quarantine for mild cases
delt <- rs*yi*rh/(yi*rh - rs*rh - yi*rs) # duration of quarantine for hospitalized cases

# First dose flow
pv1 <- p*ep
av1 <- a
rav1 <- ra
yiv1 <- yi
qv1 <- ep*q
rqv1 <- rq
deltv1 <- delt
rhv1 <- rh
uv1 <- u

# Second dose flow
pv2 <- epv*p
av2 <- a
rav2 <- ra
yiv2 <- yi
qv2 <- epv*q
deltv2 <- delt
uv2 <- u
rhv2 <- rh
rqv2 <- rq


parm <- c(
  vac_daily =vac_daily,
  ep = ep,
  epv =epv,

  # Base parameters
  c =c,
  betas =betas,
  betaa =betaa,
  w =w,
  a =a,
  r =r,
  rs =rs,
  ra =ra,
  rh =rh,
  yi =yi,
  p =p,
  q =q,
  u =u,

  # Adjustment following Moghadas et al,.
  rq =rq,
  delt =delt,

  # First dose flow
  pv1 =pv1,
  av1 =av1,
  rav1 =rav1,
  yiv1 =yiv1,
  qv1 =qv1,
  rqv1 =rqv1,
  deltv1 =deltv1,
  rhv1 =rhv1,
  uv1 =uv1,

  # Second dose flow
  pv2 =pv2,
  av2 =av2,
  rav2 =rav2,
  yiv2 =yiv2,
  qv2 =qv2,
  deltv2 =deltv2,
  uv2 =uv2,
  rhv2 =rhv2,
  rqv2 =rqv2
)
