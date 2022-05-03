# initial values from 
# https://malariajournal.biomedcentral.com/articles/10.1186/1475-2875-13-268/tables/3
# https://advancesindifferenceequations.springeropen.com/articles/10.1186/s13662-019-2424-6/tables/2

# Infectivity related
cvh <- 0.24 # vector -> human infectivity (cv)
chv <- 0.024 # human -> vector infectivity (ch)
av <- 0.35 # daily biting rate (beta h)

vh <- 1/14 # incubation period ^-1
p <- 0.05 # cerebral malaria %
uh <- 0.2 # mortality rate of cerebral malaria under treatment
rh <- 0.0035 # recovery rate to susceptible (v)
bh <- 1/365 # loss of immunity (delta)
ah <- 1/7 # recovery rate to immuned
dh <- 1/4 # cerebral malarai recovery to susceptible
eh <- 1/4 # cerebral malaria recovery to immuned

uv  <- 0.13 # daily birth rate per capita
ov <- 0.1 # nautral death rate of mosquitos per capita
vv <- 1/15 # mosquito incubation period

parm_mal <- c(
  cvh = cvh,
  chv = chv,
  av = av,
  vh = vh,
  p = p,
  uh = uh,
  rh = rh,
  bh = bh,
  ah = ah,
  dh = dh,
  eh = eh,
  uv = uv,
  ov = ov,
  vv = vv
)