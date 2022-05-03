M2 <- function(time,state,parm){
  par=as.list(c(state,parm))
  with(par,{
    
    Lamh <- cvh*av*Iv / (Sh+Eh+Ih+Rh)
    dSh <- -Lamh*Sh + bh*Rh + dh*Ch + rh*Ih*(1-p)
    dEh <- Lamh*Sh - vh*Eh
    dIh <- vh*Eh - p*Ih - (1-p)*rh*Ih - (1-p)*ah*Ih
    dRh <- (1-p)*ah*Ih + eh*Ch - bh*Rh
    dCh <- p*Ih - eh*Ch - uh*Ch - dh*Ch
    dDh <- uh*Ch
    
    Lamv <- chv*av*Ih / (Sh+Eh+Ih+Rh)
    dSv <- ov*(Sv+Ev+Iv) - uv*Sv - Lamv*Sv
    dEv <- Lamv*Sv - uv*Ev - vv*Ev
    dIv <- vv*Ev - uv*Iv
    
    list(c(dSh, dEh, dIh, dRh, dCh, dDh,
           dSv, dEv, dIv))
  })
}
