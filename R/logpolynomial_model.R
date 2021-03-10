logpolynomial_model<-function(x,A,c0,n,penalty_factor_below_offset=10)
{
    r=polynomial_model(x,A,c0,n)
    r[r>0]=log(r[r>0])
    r[r<=0]=-penalty_factor_below_offset*log(-r[r<=0])
    return(r)
}