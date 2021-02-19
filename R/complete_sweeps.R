# function definition
complete_sweeps<-function(theSweeps,abscissa="tau_Pa",sweep_identifier_column="file",columns_to_complete=c("Gprime_Pa","Gprimeprime_Pa","Gamma_in_percent"),scale_with_abscissa=c("Gamma_in_percent"))
{
    
    
    
    
    all_x_values = sort(unique(theSweeps[,abscissa]))
    
    for(theFile in unique(theSweeps[,sweep_identifier_column]))
    {
        current_data =theSweeps[theSweeps[,sweep_identifier_column]==theFile,]
        
        if(length(unique(current_data[,abscissa]))<length(all_x_values))
        {
            to_complete = all_x_values[!(all_x_values %in% unique(current_data[,abscissa]))]
            
            
            for(complete_value in to_complete)
            {
                complete_vector = matrix(nrow=1,ncol=length(c(sweep_identifier_column,abscissa,columns_to_complete)))
                colnames(complete_vector)=c(sweep_identifier_column,abscissa,columns_to_complete)
                complete_vector=as.data.frame(complete_vector)
                complete_vector[,sweep_identifier_column]=theFile
                complete_vector[,abscissa]=complete_value
                
                if(complete_value < min(current_data[,abscissa]))
                {
                    for(theCol in columns_to_complete)
                    {
                        if(theCol %in% scale_with_abscissa)
                        {
                            complete_vector[theCol]=current_data[,theCol][which.min(current_data[,abscissa])]*complete_value/min(current_data[,abscissa])
                        } else {
                            complete_vector[theCol]=current_data[,theCol][which.min(current_data[,abscissa])]
                        }
                        
                    }
                    
                    
                    
                    
                } else {
                    if(complete_value > max(current_data[,abscissa]))
                    { # extrapolate to higher values than measured. For this, assume constant slope in logarithmic plots
                        highest_values = current_data[which.max(current_data[,abscissa]),]
                        abscissa_values = current_data[,abscissa]
                        abscissa_values[which.max(abscissa_values)]=min(abscissa_values) # remove the highest tau rel value such as
                        # to define the second-highest
                        second_highest_values = current_data[which.max(abscissa_values),]
                        
                        
                        
                        
                        for(theCol in columns_to_complete)
                        {
                            
                            complete_vector[theCol]=exp(log(highest_values[,theCol])+
                            (log(highest_values[,theCol])-log(second_highest_values[,theCol]))/
                            (log(highest_values[,abscissa])-log(second_highest_values[,abscissa]))*
                            (log(complete_value)-log(highest_values[,abscissa]))
                            )
                            
                            
                        }
                        
                        
                        
                    } else {
                        # We can interpolate appropriately as we are inside the domain covered
                        
                        for(theCol in columns_to_complete)
                        {
                            complete_vector[theCol]=exp(approx(x=log(current_data[,abscissa]),y=log(current_data[,theCol]),xout=log(complete_value))$y)
                            
                        }
                        
                        
                        
                        
                        
                    }
                    
                }
                
                newLine=current_data[dim(current_data)[1],]
                for(theCol in colnames(complete_vector))
                {
                    newLine[,theCol]=complete_vector[theCol]
                }
                
                theSweeps=rbind(theSweeps,newLine)
                
            }
        }
    }
    # Sort again along the abscissa
    for(theFile in unique(theSweeps[,sweep_identifier_column]))
    {
        current_data=theSweeps[theSweeps[,sweep_identifier_column]==theFile,]
        current_data=current_data[order(current_data[,abscissa]),]
        theSweeps[theSweeps[,sweep_identifier_column]==theFile,]=current_data
    }


    return(theSweeps)
    
}
