# Pooled BIFAP should be in folders: rates1000, proportions, stratified
library(stringr)
library(data.table)
##################################################################
##################################################################
##################### FINAL COUNTS: RATES  #######################
##################################################################
##################################################################
output_dir<-"C:/Users/mgamb/Desktop/DAP_results"
plot_folder<-"C:/Users/mgamb/Desktop/DAP_results/plots"

final_counts_rates_folders<-list.files(path = output_dir, pattern = "rates")

if(length(final_counts_rates_folders)>0){
  # Creates lists
  count_names_all<-list()
  count_files_all<-list()
  # Extracts counts files
  for (folder in 1:length(final_counts_rates_folders)){
    if(length(list.files(paste0(output_dir,"/", final_counts_rates_folders[folder]), pattern="count")) > 0){
      count_names<-list.files(paste0(output_dir, "/",final_counts_rates_folders[folder]), pattern = "count")
      #Read in files
      count_files<-lapply(paste0(output_dir, "/", final_counts_rates_folders[folder],"/", count_names), readRDS)
      count_names_all[[folder]]<-count_names
      count_files_all[[folder]]<-count_files
    }
  }
  
  # Removes empty lists
  count_names_all<-count_names_all[lapply(count_names_all,length)>0]
  count_files_all<-count_files_all[lapply(count_files_all,length)>0]
  
  # Plots counts 
  if (length(count_files_all)>0){
    for (i in 1:length(count_files_all)){
      for (j  in 1: length(count_files_all[[i]])){
        main_name<-substr(count_names_all[[i]][[j]], 1,nchar(count_names_all[[i]][[j]])-11)
        pdf((paste0(plot_folder,"/", main_name, "_counts.pdf")), width=8, height=4)
        my_data<-as.data.frame(count_files_all[[i]][[j]])
        #indicate masked values with stars
        my_pch<-count_files_all[[i]][[j]]$masked
        my_pch[my_pch==0]<-16
        my_pch[my_pch==1]<-8
        my_ymax<-ifelse (max(my_data$N) < 1, 1, max(my_data$N))
        # Makes plots 
        plot(x=1:nrow(my_data), y=my_data$N, ylim=c(0,my_ymax), xaxt="n", xlab="", ylab="counts", main=main_name, pch=my_pch, type="b", lwd=2, cex.main=1.5)
        axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
        dev.off()
      }
    }
    # Plots Rates
    for (i in 1:length(count_files_all)){
      for (j  in 1: length(count_files_all[[i]])){
        
        main_name<-substr(count_names_all[[i]][[j]], 1,nchar(count_names_all[[i]][[j]])-11)
        pdf((paste0(plot_folder,"/", main_name, "_rates.pdf")), width=8, height=4)
        my_data<-count_files_all[[i]][[j]]
        my_data<-as.data.table(my_data)
        #Set NA/inf rate values to 0
        my_data[!is.finite(rates),rates:=0]
        
        # my_data[!is.finite(my_data$rates),]$rates<-0
        
        #indicate masked values with stars
        my_pch<-my_data$masked
        my_pch[my_pch==0]<-16
        my_pch[my_pch==1]<-8
        my_ymax<-ifelse (max(my_data$rates) < 1, 1, max(my_data$rates))
        # Assigns ylab name
        # ylab_props<-""
        # if(str_detect(main_name, "prevalence")){ylab_rates<-"Number current users per 1000 pm"}
        # if(str_detect(main_name, "incidence")){ylab_rates<-"Number new users per 1000 pm"}
        # if(str_detect(main_name, "discontinued")){ylab_rates<-"Number discontinued users per 1000 pm"}
        # if(str_detect(main_name, "switcher")){ylab_rates<-"Number switchers per 1000 pm"}
        # if(str_detect(main_name, "general_concomit")){ylab_rates<-"Number general concomitance/1000 person-months"}
        
        # Makes plots
        plot(x=1:nrow(my_data), y=my_data$rates,ylim=c(0,my_ymax), xaxt="n",type="b", xlab="", ylab= "rates", main=main_name, pch=my_pch, lwd=2, cex.main=1.5)
        
        axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
        dev.off()
        
      }
    }
  } else {
    print("There are no files to plot")
  }
}

##################################################################
##################################################################
##################### FINAL COUNTS: PROPORTIONS  #################
##################################################################
##################################################################
final_counts_props_folders<-list.files(path = output_dir, pattern = "proportions")

if(length(final_counts_props_folders)>0){
  # Creates lists
  count_names_all<-list()
  count_files_all<-list()
  # Extracts counts files
  for (folder in 1:length(final_counts_props_folders)){
    if(length(list.files(paste0(output_dir,"/", final_counts_props_folders[folder], "/"), pattern="count")) > 0){
      count_names<-list.files(paste0(output_dir,"/",final_counts_props_folders[folder]), pattern = "count")
      count_files<-lapply(paste0(output_dir, "/",final_counts_props_folders[folder],"/", count_names), readRDS)
      count_names_all[[folder]]<-count_names
      count_files_all[[folder]]<-count_files
    }
  }
  # Removes empty lists
  count_names_all<-count_names_all[lapply(count_names_all,length)>0]
  count_files_all<-count_files_all[lapply(count_files_all,length)>0]
  # Plots counts 
  if (length(count_files_all)>0){
    for (i in 1:length(count_files_all)){
      for (j  in 1: length(count_files_all[[i]])){
        main_name<-substr(count_names_all[[i]][[j]], 1,nchar(count_names_all[[i]][[j]])-11)
        pdf((paste0(plot_folder,"/", main_name, "_counts.pdf")), width=8, height=4)
        my_data<-as.data.frame(count_files_all[[i]][[j]])
        #indicate masked values with stars
        my_pch<-count_files_all[[i]][[j]]$masked
        my_pch[my_pch==0]<-16
        my_pch[my_pch==1]<-8
        my_ymax<-ifelse (max(my_data$N) < 1, 1, max(my_data$N))
        # Makes plots 
        plot(x=1:nrow(my_data), y=my_data$N,ylim=c(0,max(my_data$N)), xaxt="n", xlab="", ylab="counts", main=main_name, pch=my_pch, type="b", lwd=2, cex.main=1.5)
        axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
        dev.off()
      }
    }
    # Plots Rates
    for (i in 1:length(count_files_all)){
      for (j  in 1: length(count_files_all[[i]])){
        main_name<-substr(count_names_all[[i]][[j]], 1,nchar(count_names_all[[i]][[j]])-11)
        pdf((paste0(plot_folder,"/", main_name, "_rates.pdf")), width=8, height=4)
        my_data<-count_files_all[[i]][[j]]
        
        my_data<-as.data.table(my_data)
        #Set NA/inf rate values to 0
        my_data[!is.finite(rates),rates:=0]
        
        #indicate masked values with stars
        my_pch<-my_data$masked
        my_pch[my_pch==0]<-16
        my_pch[my_pch==1]<-8
        my_ymax<-ifelse (max(my_data$rates) < 1, 1, max(my_data$rates))
        # Assigns ylab name
        # ylab_props<-""
        # if(str_detect(main_name, "contraindicated_records")){ylab_props<-"Contraindicated - record counts (in concomitant users)"}
        # if(str_detect(main_name, "contraindicated_users")){ylab_props<-"Contraindicated - user counts (in concomitant users)"}
        # if(str_detect(main_name, "teratogenic_per_record")){ylab_props<-"Teratogenic RAM - record counts"}
        # if(str_detect(main_name, "teratogenic_per_user")){ylab_props<-"Teratogenic RAM - user counts"}
        
        # Makes plots
        plot(x=1:nrow(my_data), y=my_data$rates, ylim=c(0,my_ymax),xaxt="n",type="b", xlab="", ylab= "rates", main=main_name, pch=my_pch, lwd=2, cex.main=1.5)
        axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
        dev.off()
      }
    }
  } else {
    print("There are no files to plot")
  }
}

##################################################################
##################################################################
##################### FINAL COUNTS: STRATIFIED  #################
##################################################################
##################################################################

final_counts_stratified<-list.files(path = output_dir, pattern = "stratified")

if(length(final_counts_stratified)>0){
  # Creates lists
  count_names_all<-list()
  count_files_all<-list()
  # Extracts counts files
  for (folder in 1:length(final_counts_stratified)){
    if(length(list.files(paste0(output_dir, "/",final_counts_stratified[folder], "/"), pattern="count")) > 0){
      count_names <-list.files(paste0(output_dir, "/",final_counts_stratified[folder]), pattern = "count")
      count_files<-lapply(paste0(output_dir, "/",final_counts_stratified[folder],"/", count_names), readRDS)
      count_names_all[[folder]]<-count_names
      count_files_all[[folder]]<-count_files
    }
  }
  # Removes empty lists
  count_names_all<-count_names_all[lapply(count_names_all,length)>0]
  count_files_all<-count_files_all[lapply(count_files_all,length)>0]
  # Plots counts 
  if (length(count_files_all)>0){
    for (i in 1:length(count_files_all)){
      for (j  in 1: length(count_files_all[[i]])){
        main_name<-gsub("_counts","",count_names_all[[i]][[j]])
        main_name<-substr(main_name, 1,nchar(count_names_all[[i]][[j]])-11)
        
        pdf((paste0(plot_folder,"/", main_name, "_counts.pdf")), width=8, height=4)
        my_data<-as.data.frame(count_files_all[[i]][[j]])
        #indicate masked values with stars
        my_pch<-count_files_all[[i]][[j]]$masked
        my_pch[my_pch==0]<-16
        my_pch[my_pch==1]<-8
        my_ymax<-ifelse (max(my_data$N) < 1, 1, max(my_data$N))
        # Makes plots 
        plot(x=1:nrow(my_data), y=my_data$N,ylim=c(0,max(my_data$N)), xaxt="n", xlab="", ylab="counts", main=main_name, pch=my_pch, type="b", lwd=2, cex.main=1.5)
        axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
        dev.off()
      }
    }
    # Plots Rates
    for (i in 1:length(count_files_all)){
      for (j  in 1: length(count_files_all[[i]])){
        main_name<-gsub("_counts","",count_names_all[[i]][[j]])
        main_name<-substr(main_name, 1,nchar(count_names_all[[i]][[j]])-11)
        pdf((paste0(plot_folder,"/", main_name, "_proportion.pdf")), width=8, height=4)
        my_data<-count_files_all[[i]][[j]]
        
        #Set NA/inf rate values to 0
        my_data[!is.finite(my_data$rates),]$rates<-0
        
        #indicate masked values with stars
        my_pch<-my_data$masked
        my_pch[my_pch==0]<-16
        my_pch[my_pch==1]<-8
        my_ymax<-ifelse (max(my_data$rates) < 1, 1, max(my_data$rates))
        # Assigns ylab name
        # ylab_props<-""
        # if(str_detect(main_name, "contraception_prior")){ylab_props<-"Proportion presc/disp. with contraceptive before"}
        # if(str_detect(main_name, "med_use_during_contra_episodes")){ylab_props<-"Proportion prec/disp. within contraception period"}
        # if(str_detect(main_name, "discontinued")){ylab_props<-"Proportion discontinued"}
        # if(str_detect(main_name, "switched")){ylab_props<-"Proportion switched to alternative"}
        # if(str_detect(main_name, "before")){ylab_props<-"Proportion presc/disp. with pregnancy test before"}
        # if(str_detect(main_name, "after")){ylab_props<-"Proportion presc/disp. with pregnancy test after"}
        # Makes plots
        plot(x=1:nrow(my_data), y=my_data$rates, ylim=c(0,my_ymax),xaxt="n",type="b", xlab="", ylab= "rates", main=main_name, pch=my_pch, lwd=2, cex.main=1.5)
        axis(1, at=1:nrow(my_data), as.character(my_data$YM), las=2)
        dev.off()
      }
    }
  } else {
    print("There are no files to plot")
  }
}
