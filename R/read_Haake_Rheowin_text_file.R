read_Haake_Rheowin_text_file<-function(file,use_C_locale=TRUE,colname_info=
	list("tau_Pa"=c("Tau.en.Pa","X..en.Pa"),
	"Gprime_Pa"=c("G..en.Pa"),
	"Gprimeprime_Pa"=c("X.G...en.Pa.","G..en.Pa.1"),
	"Phase_angle_degrees"=c("X..en...1","Da.en.."),
	"Gamma_in_percent"=c("X..en..","Ga.en.."),
	"f_Hz"=c("f.en.Hz"),
	"t_s"=c("t.en.s"),
	"t_seg_s"="t_seg.en.s"),...){



	# Get the old locale.In these Rheowin text files, there are problematic multibyte characters, we prefer reading this
	# in plane C and then do some replacements
	# So, get whatever the actual locale presently is
	
	
	old_locale=strsplit(sessionInfo()[["locale"]],"/")[[1]][1]
	
	# Set the locale to pure C, we will set it back afterwards
	
	if(use_C_locale){Sys.setlocale("LC_ALL","C") # To avoid problems with encoding
	}

	args = list(...)
	# Complete the arguments with some useful settings for read.table (in the case of Rheowin text files)
	if(!("header" %in% names(args)))
	{
		args[["header"]]=TRUE
	}
	if(!("sep" %in% names(args)))
	{
		args[["sep"]]="\t"
	}
	if(!("comment.char" %in% names(args)))
	{
		args[["comment.char"]]=""
	}
	if(!("blank.lines.skip" %in% names(args)))
	{
		args[["blank.lines.skip"]]=FALSE
	}
	if(!("quote" %in% names(args)))
	{
		args[["quote"]]=""
	}
	args[["file"]]=file
	
	current_data=do.call(read.table, args)
	
	
	
	# Sometimes, individual files need different treatment (separation character different, try once again with standard ("\t")
	 if(dim(current_data)[2]==1 & args[["sep"]] != "\t")
    	{
		args[["sep"]] = "\t"
		current_data=do.call(read.table, args)
        
    	}
	
	
	
	if(use_C_locale){Sys.setlocale("LC_ALL",old_locale) # To avoid problems with encoding
	}
	
	# Recover the desired colnames (the ones specified in colname_info)
	
	output_data = vector(mode="list",length=length(colname_info))
	names(output_data)=names(colname_info)
	
	for(theColumn in names(colname_info))
	{
		data_read = rep(NA,dim(current_data)[1])
		done=FALSE
		for(proposed_column in colname_info[[theColumn]])
		{
			if(!done & (proposed_column %in% colnames(current_data)))
			{
				output_data[[theColumn]]=current_data[,proposed_column]
				done=TRUE
			}
		}
		if(!done)
		{
			output_data[[theColumn]]=data_read
		}
		
	}
	
	output_data=as.data.frame(output_data)

	 return(output_data)


}


