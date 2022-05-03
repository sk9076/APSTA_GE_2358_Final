# M1
M1 <- function(time,state,parm){
  par=as.list(c(state,parm))
  with(par,{

    pi2 <- min(vac_daily, V1+RV1)
    pi1 <- min(S+R, vac_daily - pi2)

    Lambda= c*(betaa*(A+AV1+AV2)+betas*(I+IV1+IV2+Q+H+QV1+HV1+QV2+HV2))/(S+E+A+I+Q+H+R+V1+EV1+AV1+IV1+QV1+HV1+RV1+V2+EV2+AV2+IV2+QV2+HV2)

                                 #omega is really lamda
    dS=-Lambda*S-(1/14)*S/(S+R)*pi1
    dE=Lambda*S-p*a*E-(1-p)*a*E
    dA=(1-p)*a*E-ra*A
    dI=p*a*E-yi*I
    dQ=yi*I-(1-q)*rq*Q-q*delt*Q
    dH=q*delt*Q-u*H-rh*H
    dR=ra*A+(1-q)*rq*Q+rh*H-(1/14)*((R)/(S+R))*pi1
    dD=u*H

    dV1=-Lambda*ep*V1-(1/28)*V1/(V1+RV1)*pi2+(1/14)*pi1
    dEV1=Lambda*ep*V1-pv1*av1*EV1-(1-pv1)*av1*EV1
    dAV1=(1-pv1)*av1*EV1-rav1*AV1
    dIV1=pv1*av1*EV1-yiv1*IV1
    dQV1=yiv1*IV1-(1-qv1)*rqv1*QV1-qv1*deltv1*QV1
    dHV1=qv1*deltv1*QV1-uv1*HV1-rhv1*HV1
    dRV1=rav1*AV1+(1-qv1)*rqv1*QV1+rhv1*HV1-(1/28)*RV1/(V1+RV1)*pi2
    dDV1=uv1*HV1

    dV2=1/28*pi2-Lambda*epv*V2
    dEV2=Lambda*epv*V2-pv2*av2*EV2-(1-pv2)*av2*EV2
    dAV2=(1-pv2)*av2*EV2-rav2*AV2
    dIV2=pv2*av2*EV2-yiv2*IV2
    dQV2=yiv2*IV2-(1-qv2)*rqv2*QV2-qv2*deltv2*QV2
    dHV2=qv2*deltv2*QV2-uv2*HV2-rhv2*HV2
    dRV2=rqv2*AV2+(1-qv2)*rqv2*QV2+rhv2*HV2
    dDV2=uv2*HV2

    dPI1 = pi1
    dPI2 = pi2
    list(c(dS,dE,dA,dI,dQ,dH,dR,dD, dV2, dV1,dEV1,dAV1,dIV1,dQV1,dHV1,dRV1,dDV1,dEV2,dAV2,dIV2,dQV2,dHV2,dRV2,dDV2, dPI1, dPI2))
  })
}

M1_q <- function(time,state,parm){
  par=as.list(c(state,parm))
  with(par,{
    
    pi2 <- min(vac_daily, V1+RV1)
    pi1 <- min(S+R, vac_daily - pi2)
    
    Lambda= c*(betaa*(A+AV1+AV2)+betas*(I+IV1+IV2)+0.25*betas*(Q+H+QV1+HV1+QV2+HV2))/(S+E+A+I+Q+H+R+V1+EV1+AV1+IV1+QV1+HV1+RV1+V2+EV2+AV2+IV2+QV2+HV2)
    
    #omega is really lamda
    dS=-Lambda*S-(1/14)*S/(S+R)*pi1
    dE=Lambda*S-p*a*E-(1-p)*a*E
    dA=(1-p)*a*E-ra*A
    dI=p*a*E-yi*I
    dQ=yi*I-(1-q)*rq*Q-q*delt*Q
    dH=q*delt*Q-u*H-rh*H
    dR=ra*A+(1-q)*rq*Q+rh*H-(1/14)*((R)/(S+R))*pi1
    dD=u*H
    
    dV1=-Lambda*ep*V1-(1/28)*V1/(V1+RV1)*pi2+(1/14)*pi1
    dEV1=Lambda*ep*V1-pv1*av1*EV1-(1-pv1)*av1*EV1
    dAV1=(1-pv1)*av1*EV1-rav1*AV1
    dIV1=pv1*av1*EV1-yiv1*IV1
    dQV1=yiv1*IV1-(1-qv1)*rqv1*QV1-qv1*deltv1*QV1
    dHV1=qv1*deltv1*QV1-uv1*HV1-rhv1*HV1
    dRV1=rav1*AV1+(1-qv1)*rqv1*QV1+rhv1*HV1-(1/28)*RV1/(V1+RV1)*pi2
    dDV1=uv1*HV1
    
    dV2=1/28*pi2-Lambda*epv*V2
    dEV2=Lambda*epv*V2-pv2*av2*EV2-(1-pv2)*av2*EV2
    dAV2=(1-pv2)*av2*EV2-rav2*AV2
    dIV2=pv2*av2*EV2-yiv2*IV2
    dQV2=yiv2*IV2-(1-qv2)*rqv2*QV2-qv2*deltv2*QV2
    dHV2=qv2*deltv2*QV2-uv2*HV2-rhv2*HV2
    dRV2=rqv2*AV2+(1-qv2)*rqv2*QV2+rhv2*HV2
    dDV2=uv2*HV2
    
    dPI1 = pi1
    dPI2 = pi2
    list(c(dS,dE,dA,dI,dQ,dH,dR,dD, dV2, dV1,dEV1,dAV1,dIV1,dQV1,dHV1,dRV1,dDV1,dEV2,dAV2,dIV2,dQV2,dHV2,dRV2,dDV2, dPI1, dPI2))
  })
}
