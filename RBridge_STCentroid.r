### KMeans Clustering Toolbox
##Author: Fabio Veronesi
tool_exec <- function(in_params, out_params)
{
	if (!requireNamespace("sp", quietly = TRUE))
    install.packages("sp")
	require(sp, quietly = TRUE)
	
	if (!requireNamespace("reshape2", quietly = TRUE))
    install.packages("reshape2")
	require(reshape2, quietly = TRUE)
	
  
	print("Spatio Temporal Centroid")
	print("Author: Fabio Veronesi")
  
	source_shp = in_params[[1]]
	timeCL = in_params[[2]]
	formatTM = in_params[[3]]
	CAT.VAR = in_params[[4]]
	TM = in_params[[5]]
	STAT = in_params[[6]]

	out_shp = out_params[[1]]
   
	### Read Data
	d <- arc.open(source_shp)
	
	getmode <- function(x) {
		temp <- table(as.vector(x))
		mean(as.numeric(names(temp)[temp == max(temp)]))
	}
	
	if(TM=="Day"){
	TM_FUN="%d"
	} else if(TM=="Month"){
	TM_FUN="%m"
	} else {
	TM_FUN="%Y"}
	
	if(STAT=="Mean"){
	STAT="mean"
	} else if(STAT=="Median"){
	STAT="median"
	} else {
	STAT="getmode"}
	
	### Create a Data.Frame with all the variables
	#data <- arc.select(d, variable)
	TIME <- arc.select(d, timeCL)
	
	data_SP <- arc.data2sp(arc.select(d, fields = "*"))
	print("Converting Time Variable...")
	data_SP$DATETIME <- as.POSIXct(TIME[,timeCL], format=formatTM)
	
	if(!is.null(CAT.VAR)){
		CAT = strsplit(CAT.VAR,"\"")[[1]][2]
		subVA = strsplit(CAT.VAR,"'")[[1]][2]
		
		CATEGORY <- arc.select(d, CAT)
		data_SP$CAT <- CATEGORY[,CAT]
		
		data_sub <- data_SP[paste(data_SP$CAT)==subVA,]

		MONTH <- unique(format(data_sub$DATETIME, TM_FUN))
		print("Executing loop. Please wait.")
		results <- matrix(0, length(MONTH), 3, dimnames=list(c(),c("Month","Lon","Lat")))
		for(i in MONTH){
			sub <- data_sub[format(data_sub$DATETIME, TM_FUN)==i,]
			print(eval(parse(text=STAT)[[1]])(sub@coords[,1]))
			results[which(i==MONTH),] <- matrix(c(as.numeric(paste(i)), eval(parse(text=STAT)[[1]])(sub@coords[,1]), eval(parse(text=STAT)[[1]])(sub@coords[,2])),ncol=3)
		}
	
		RES <- as.data.frame(results)
		coordinates(RES)=~Lon+Lat
	
		arc.write(out_shp, RES)
	
	} else {
		MONTH <- unique(format(data_SP$DATETIME, TM_FUN))
		print("Executing loop. Please wait.")
		results <- matrix(0, length(MONTH), 3, dimnames=list(c(),c("Month","Lon","Lat")))
		for(i in MONTH){
			sub <- data_SP[format(data_SP$DATETIME, TM_FUN)==i,]
			results[which(i==MONTH),] <- matrix(c(as.numeric(paste(i)), eval(parse(text=STAT)[[1]])(sub@coords[,1]), eval(parse(text=STAT)[[1]])(sub@coords[,2])),ncol=3)
		}
	
		RES <- as.data.frame(results)
		coordinates(RES)=~Lon+Lat
	
		arc.write(out_shp, RES)
	
	}
	
	
	
	
}
