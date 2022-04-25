# Initial conditions

N <-37590000 # total population
E0 <-2173
A0 <- 1 #36752700  1.7311e+03
I0 <- 2473
Q0 <- 10e3
H0 <- 2324
R0 <- 1317524 #13175244076 # already recovered ones
D0 <- 35 #25512
V20 <- 2169274  #2010000, # people who received the 2nd dose
V10 <- 19410408 #18642700, 19445178, 38820816 # people who recieved 1st dose vaccine
S0 <- N-(E0+I0+A0+Q0+H0+R0+D0+V20+V10+1400)
EV10 <- 100
AV10 <- 100
IV10 <- 100
QV10 <- 100
HV10 <- 100
RV10 <- 100
DV10 <- 100
EV20 <- 100
AV20 <- 100
IV20 <- 100
QV20 <- 100
HV20 <- 100
RV20 <- 100
DV20 <- 100

# For model 3
R_s <- R0*p
R_a <- R0*(1-p)
R_v1_s <- RV10*p
R_v1_a <- RV10*(1-p)

# To count vaccine doses and screenings used
PI1 <- 0
PI2 <- 0
SCR <- 0

init_12 <- c(
  S = S0,
  E=E0,
  A=A0,
  I=I0,
  Q=Q0,
  H=H0,
  R=R0,
  D=D0,
  V2=V20,
  V1 =V10,
  EV1 =EV10,
  AV1 = AV10,
  IV1 = IV10,
  QV1 = QV10,
  HV1 = HV10,
  RV1 = RV10,
  DV1 = DV10,
  EV2 = EV20,
  AV2 = AV20,
  IV2 = IV20,
  QV2 = QV20,
  HV2 = HV20,
  RV2 = RV20,
  DV2 = DV20,
  # To count vaccine doses used
  PI1 = PI1,
  PI2 = PI2
)

init_3 <- c(
  S = S0,
  E=E0,
  A=A0,
  I=I0,
  Q=Q0,
  H=H0,
  D=D0,
  # For model 3
  RS = R_s,
  RA = R_a,
  V2=V20,
  V1 =V10,
  EV1 =EV10,
  AV1 = AV10,
  IV1 = IV10,
  QV1 = QV10,
  HV1 = HV10,
  RV1A = R_v1_a,
  RV1S = R_v1_s,
  DV1 = DV10,
  EV2 = EV20,
  AV2 = AV20,
  IV2 = IV20,
  QV2 = QV20,
  HV2 = HV20,
  RV2 = RV20,
  DV2 = DV20,

  # To count vaccine doses used
  PI1 = PI1,
  PI2 = PI2,
  SCR = SCR
)

