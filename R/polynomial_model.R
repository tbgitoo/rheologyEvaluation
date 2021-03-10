polynomial_model <-function(x,A,c0,n)
{
    ret=A*(x-c0)^n
    ret[]=0
    ret[x-c0>=0]=A*(x-c0)[x-c0>=0]^n
    
    return(ret)
}
