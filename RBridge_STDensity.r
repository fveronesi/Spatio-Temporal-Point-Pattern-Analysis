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
		
	if (!requireNamespace("spatstat", quietly = TRUE))
    install.packages("spatstat")
	require(spatstat, quietly = TRUE)
	
	if (!requireNamespace("maptools", quietly = TRUE))
    install.packages("maptools")
	require(maptools, quietly = TRUE)
	
	if (!requireNamespace("rgeos", quietly = TRUE))
    install.packages("rgeos")
	require(rgeos, quietly = TRUE)
	
	if (!requireNamespace("xts", quietly = TRUE))
    install.packages("xts")
	require(xts, quietly = TRUE)
	
  
	print("Spatio Temporal Density")
	print("Author: Fabio Veronesi")
  
	source_shp = in_params[[1]]
	source_owin = in_params[[2]]
	sub_owin = in_params[[3]]
	timeCL = in_params[[4]]
	formatTM = in_params[[5]]
	subTM = in_params[[6]]
	CAT.VAR = in_params[[7]]
	
	out_raster = out_params[[1]]

   
	### Read Data
	d <- arc.open(source_shp)
	border <- arc.open(source_owin)
	
	### Create a Data.Frame with all the variables
	#data <- arc.select(d, variable)
	TIME <- arc.select(d, timeCL)
	
	data_SP <- arc.data2sp(arc.select(d, fields = "*"))
	data_SP$DATETIME <- as.POSIXct(TIME[,timeCL], format=formatTM)
	
	if(!is.null(sub_owin)){
		CAT_owin = strsplit(sub_owin,"\"")[[1]][2]
		subVA_owin = strsplit(sub_owin,"'")[[1]][2]
		
		NAME <- arc.select(border, CAT_owin)
		
		b <- arc.data2sp(arc.select(border))
		b$NAME <- NAME[,CAT_owin]
		
		b_sub <- b[paste(b$NAME)==subVA_owin,]
		window <- as.owin(b_sub)
	} else {
		b_sub <- arc.data2sp(arc.select(border))
		window <- as.owin(b_sub)
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
	
	sub_ppp <- ppp(x=sub@coords[,1],y=sub@coords[,2],window=window)
	
	Density <- density.ppp(sub_ppp, sigma = bw.ppl(sub_ppp),edge=T)
	Density.raster <- raster(Density)
	writeRaster(Density.raster, out_raster)
	
	
}
