fitPolynomialModelGprimeConcentration<-function(Gprime,polymer_conc_mgmL,n=2.5, do_full_fit=TRUE)
{
    
    
    OK= !is.na(Gprime) & !is.na(polymer_conc_mgmL)
    Gprime=Gprime[OK]
    polymer_conc_mgmL=polymer_conc_mgmL[OK]
    
    c0=min(polymer_conc_mgmL)*0.9
    A=max(Gprime)/(max(polymer_conc_mgmL)-c0)^n
    
    logGprime=log(Gprime)
    
    
    for(ind in 1:100)
    {
    
    coeffs=coefficients(nls(logGprime~logpolynomial_model(polymer_conc_mgmL,A,c0,n),
    start=list(A=A),control=nls.control(warnOnly=TRUE)))
    
    
    
    A=coeffs[["A"]]
    
    coeffs=coefficients(nls(logGprime~logpolynomial_model(polymer_conc_mgmL,A,c0,n),
    start=list(A=A,n=n),control=nls.control(warnOnly=TRUE)))
    
    
    
    A=coeffs[["A"]]
    n=coeffs[["n"]]
    
    
    coeffs=coefficients(nls(logGprime~logpolynomial_model(polymer_conc_mgmL,A,c0,n),
    start=list(c0=c0,A=A),control=nls.control(warnOnly=TRUE)))
    
    
    A=coeffs[["A"]]
    c0=coeffs[["c0"]]
    
    coeffs=coefficients(nls(logGprime~logpolynomial_model(polymer_conc_mgmL,A,c0,n),start=list(n=n,A=A),control=nls.control(warnOnly=TRUE)))
    
    A=coeffs[["A"]]
    n=coeffs[["n"]]
    
    }
    
    if(do_full_fit)
    {
    
    coeffs=coefficients(nls(logGprime~logpolynomial_model(polymer_conc_mgmL,A,c0,n),start=
    list(n=n,A=A,c0=c0),control=nls.control(warnOnly=TRUE)))
    
    
    
    A=coeffs[["A"]]
    n=coeffs[["n"]]
    c0=coeffs[["c0"]]
    
    }
    
    return(list(A=A,n=n,c0=c0))
    
    
    
    
}



