# ==============================================

# Function definition:
# Function to estimate the wall fraction from the polymer concentration. Needs two constants, the
# wall concentration and the osmotic coefficient gamma
# From Pellet, C. and M. Cloitre, The glass and jamming transitions of soft polyelectrolyte microgel suspensions. Soft Matter, 2016. 12(16): p. 3710-20,
# see Supplementary 8, particularly for values of the wall concentration and gamma fraction


wall_fraction<-function(polymer_concentration=10,wall_concentration=70,gamma=0.15)
{
    
    to_zero<-function(exp_factor,phi_0,alpha)
    {
        phi=phi_0*exp(-exp_factor)
        return((phi/phi_0)^(2/3)-(1-alpha*phi/(1-phi)))
    }
    
    phi_0 = polymer_concentration/wall_concentration
    
    # From Pellet, C. and M. Cloitre, The glass and jamming transitions of soft polyelectrolyte microgel suspensions. Soft Matter, 2016. 12(16): p. 3710-20.
    
    final_phi = phi_0
    
    for(ind in 1:length(phi_0))
    {
        
        if(phi_0[ind]>0)
        {
            
            
            
            alpha=gamma/(1-gamma)
            
            if(phi_0[ind]<1)
            {
                final_phi[ind]=exp(-uniroot(to_zero,c(0,20),phi_0=phi_0[ind],alpha=alpha)$root)*phi_0[ind]
            } else {
                offset_log=log(phi_0[ind])+1e-6
                final_phi[ind]=exp(-uniroot(to_zero,c(0,20)+offset_log,phi_0=phi_0[ind],alpha=alpha)$root)*phi_0[ind]
            }
            
        }
        
    }
    
    return(final_phi)
    
    
}


