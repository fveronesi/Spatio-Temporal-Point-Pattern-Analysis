### KMeans Clustering Toolbox
##Author: Fabio Veronesi
tool_exec <- function(in_params, out_params)
{

	if (!requireNamespace("sp", quietly = TRUE))
    install.packages("sp")
	require(sp, quietly = TRUE)
	
	if (!requireNamespace("raster", quietly = TRUE))
    install.packages("raster")
	require(raster, quietly = TRUE)
	
	if (!requireNamespace("maptools", quietly = TRUE))
    install.packages("maptools")
	require(maptools, quietly = TRUE)
	
	if (!requireNamespace("xts", quietly = TRUE))
    install.packages("xts")
	require(xts, quietly = TRUE)
	
	if (!requireNamespace("rgeos", quietly = TRUE))
    install.packages("rgeos")
	require(rgeos, quietly = TRUE)
	
	if (!requireNamespace("ncf", quietly = TRUE))
    install.packages("ncf")
	require(ncf, quietly = TRUE)
	
  
	print("Spatio Temporal Correlogram")
	print("Author: Fabio Veronesi")
  
	source_shp = in_params[[1]]
	variable = in_params[[2]]
	source_owin = in_params[[3]]
	sub_owin = in_params[[4]]
	timeCL = in_params[[5]]
	formatTM = in_params[[6]]
	subTM = in_params[[7]]
	CAT.VAR = in_params[[8]]
	
	out_folder = in_params[[9]]
	INC = in_params[[10]]
	RES = in_params[[11]]
	LATLON = in_params[[12]]
	out_shp = out_params[[1]]
	
	if(!is.null(LATLON)){
	LATLON = TRUE
	} else {
	LATLON = FALSE
	}

   
	### Read Data
	d <- arc.open(source_shp)
	
	### Create a Data.Frame with all the variables
	data <- arc.select(d, variable)
	TIME <- arc.select(d, timeCL)
	
	data_SP <- arc.data2sp(arc.select(d, fields = "*"))
	data_SP$DATETIME <- as.POSIXct(TIME[,timeCL], format=formatTM)
	data_SP$VAR <- data[,variable]
	
	if(!is.null(source_owin)){
	border <- arc.open(source_owin)
	if(!is.null(sub_owin)){
		CAT_owin = strsplit(sub_owin,"\"")[[1]][2]
		subVA_owin = strsplit(sub_owin,"'")[[1]][2]
		
		NAME <- arc.select(border, CAT_owin)
		
		b <- arc.data2sp(arc.select(border))
		b$NAME <- NAME[,CAT_owin]
		
		b_sub <- b[paste(b$NAME)==subVA_owin,]
		shapefile(b_sub, paste0(out_folder,"/Border_subset.shp"),overwrite=T)
	} else {
		b_sub <- arc.data2sp(arc.select(border))
	}
	}
	
	
	if(!is.null(CAT.VAR)){
		CAT = strsplit(CAT.VAR,"\"")[[1]][2]
		subVA = strsplit(CAT.VAR,"'")[[1]][2]
		
		CATEGORY <- arc.select(d, CAT)
		data_SP$CAT <- CATEGORY[,CAT]
		
		data_sub <- data_SP[paste(data_SP$CAT)==subVA,]
		
		time.series <- xts(as.data.frame(data_sub), data_sub$DATETIME)
		sub <- data_sub[which(data_sub$DATETIME %in% index(time.series[subTM])),]
	} else {
		data_sub <- data_SP
		time.series <- xts(as.data.frame(data_sub), data_sub$DATETIME)
		sub <- data_sub[which(data_sub$DATETIME %in% index(time.series[subTM])),]
	}

	if(!is.null(sub_owin)){
		sub.over <- over(sub, b_sub)
		sub$over <- sub.over[,1]
		sub <- sub[paste(sub$over)!="NA",]
		sub$over <- NULL
	}
	
	shapefile(sub, paste0(out_folder,"/Points_subset.shp"),overwrite=T)	
	arc.write(out_shp, sub)
	
	correlogram <- correlog(x=sub@coords[,1], y=sub@coords[,2], z=sub$VAR, increment=as.numeric(INC), resamp=as.numeric(RES), latlon=LATLON, na.rm=T)
	
	jpeg(paste0(out_folder,"/Correlogram.jpeg"),2500,2000,res=300)
	plot(correlogram)
	dev.off()
		
	dev.new()
	plot(correlogram)
	
}
