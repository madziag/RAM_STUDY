
### Creates empty df for expanding counts files (when not all month-year combinations have counts) - uses denominator file min and max year values
denominator<-as.data.table(readRDS(paste0(tmp,list.files(tmp,pattern=paste0(pop_prefix,"_denominator.rds")))))[,studyFUmonths:=NULL]
# Split Y-M variable to year - month columns (for creating empty df & and getting min and max of data available)
denominator[,c("year","month"):= tstrsplit(YM,"-",fixed=TRUE)]
denominator[,year:=as.integer(year)][,month:=as.integer(month)]
# Rearrange columns
denominator<-denominator[,c("YM", "year","month","Freq")]


# Load Retinoid User Denominator 
### Creates empty df for expanding counts files (when not all month-year combinations have counts) - uses denominator file min and max year values
# Loads denominator file
retinoid_denominator<-as.data.table(readRDS(paste0(tmp,list.files(tmp,pattern=paste0(pop_prefix,"_denominator_retinoid.rds")))))[,studyFUmonths:=NULL]

# Split Y-M variable to year - month columns (for creating empty df & and getting min and max of data available)
retinoid_denominator[,c("year","month"):= tstrsplit(YM,"-",fixed=TRUE)]
retinoid_denominator[,year:=as.integer(year)][,month:=as.integer(month)]
# Get min and max data available from retinoid_denominator
min_data_available<-min(retinoid_denominator$year)
max_data_available<-max(retinoid_denominator$year)
# Create empty df using these min and max values
empty_df<-as.data.table(expand.grid(seq(min_data_available,max_data_available),seq(1,12)))
names(empty_df)<-c("year","month")
# Rearrange columns
retinoid_denominator<-retinoid_denominator[,c("YM", "year","month","Freq")]
