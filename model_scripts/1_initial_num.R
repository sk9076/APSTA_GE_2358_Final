# Initial conditions

N <-1000000 # total population
E0 <-1000
A0 <- 1 #36752700  1.7311e+03
I0 <- 1
Q0 <- 1
H0 <- 1
R0 <- 700000  # already recovered ones
D0 <- 1 # 
V20 <- 1  #2010000, # people who received the 2nd dose
V10 <- 1 #18642700, 19445178, 38820816 # people who recieved 1st dose vaccine
S0 <- N-(E0+I0+A0+Q0+H0+R0+D0+V20+V10)
EV10 <- 1
AV10 <- 1
IV10 <- 1
QV10 <- 1
HV10 <- 1
RV10 <- 1
DV10 <- 1
EV20 <- 1
AV20 <- 1
IV20 <- 1
QV20 <- 1
HV20 <- 1
RV20 <- 1
DV20 <- 1

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

