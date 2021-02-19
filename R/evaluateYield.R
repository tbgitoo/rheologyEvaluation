evaluateYield<-function(sweep,Gprime_column="Gprime_Pa",Gprimeprime_column="Gprimeprime_Pa",abscissa="tau_Pa",additional_columns_to_be_interpolated="Gamma_in_percent")
{
    
    valid_additional_columns = additional_columns_to_be_interpolated[additional_columns_to_be_interpolated %in% colnames(sweep)]
    
    ret=vector(mode="numeric",length=2+length(valid_additional_columns))
    names(ret)<-c(abscissa,Gprime_column,valid_additional_columns)
    
    solid = sweep[sweep[,Gprime_column]>=sweep[,Gprimeprime_column],]
    
    
    liquid =sweep[sweep[,Gprime_column]<sweep[,Gprimeprime_column] ,]
    
    
    
    
    if(dim(liquid)[1]==0 | dim(solid)[1]==0)
    {
        ret[]=NA
        
        return(ret)
    }
    
    
    # Find the intersection for the solid-to-liquid transition
    
    lastSolid=0
    lastLiquid=0
    
    
    # Liquid; consider the minimum point with liquid characteristics; above the transition, slipppage can cause quite random values and so we should limit ourselves to the
    # first transition solid-liquid if there appear to be several
    firstLiquid = liquid[liquid[,abscissa]==min(liquid[,abscissa]),]
    
    lastSolid = solid[solid[,abscissa]==max(solid[,abscissa][solid[,abscissa]<=min(liquid[,abscissa])]),]
    
    
    
    alpha_transition=(lastSolid[,Gprimeprime_column]-lastSolid[,Gprime_column])/((firstLiquid[,Gprime_column]-lastSolid[,Gprime_column])-(firstLiquid[,Gprimeprime_column]-lastSolid[,Gprimeprime_column]))
    
    Gamma_transition=lastSolid[,abscissa]+alpha_transition*(firstLiquid[,abscissa]-lastSolid[,abscissa])
    
    Gprime_transition=lastSolid[,Gprime_column]+alpha_transition*(firstLiquid[,Gprime_column]-lastSolid[,Gprime_column])
    
    for(additional_column in valid_additional_columns)
    {
        ret[additional_column]=lastSolid[,additional_column]+alpha_transition*(firstLiquid[,additional_column]-lastSolid[,additional_column])
    }
    
    
    
    ret[abscissa]=Gamma_transition
    ret[Gprime_column]=Gprime_transition
    
    return(ret)
    
    
    
    
}