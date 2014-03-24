require(rgdal)
require(raster)
require(sp)
require(RPostgreSQL)

################################################################################################################
# LOSS COMPUTATION
################################################################################################################

#compute detailed loss for surveyed towns
# in term of expected fatalities, based on a
# shakemap in macroseismic intensity provided 
# as raster.
# saves a shapefile with the settlements exposed to 
# mmi > 5. for each settlements the bounds for the fatalities
# are provided, in absolute and relative terms.
# dbin, dbout: list(db_conn,layer,overwrite) to specify input and output layers in the dbs

compute_loss_geocells<-function(shakemap,nighttime=FALSE,dbin,dbout)
{
	nighttime_coeff <- 0.5
	daytime_coeff <- 0.3
	nclass<-13
	
	coeff<-ifelse(nighttime,nighttime_coeff,daytime_coeff)
	# load geocells from db
	tmpdata<-readOGR(dsn=dbin$dbconn,layer=dbin$layer)  #"urbanstructures")
	
	# project into WGS84
	tmpdata<-spTransform(tmpdata,CRS('+proj=longlat +datum=WGS84 +no_defs'))
	# filter out non-built-up and industrial/commercial areas
	#geocells<-subset(tmpdata,tmpdata@data$structure >0)
	geocells<-subset(tmpdata,tmpdata@data$population >0)
	#compute centroids of geocells
	geocells_centroids<-coordinates(geocells)
	
	# evaluate intensity for each settlement using the shakemap
	intens<-round(extract(shakemap,geocells_centroids,method='bilinear'))
	
	#keep only intensities >5
	ind<-which(intens>5)
	
	# get vulnerability composition for all strata
	#vuln_strata<-vuln_strata()
	
	# select uncertainty level. type=3 refers to most likely fragility values
	type<-3

	loss<-matrix(ncol=6,nrow=length(ind))
	tot_loss_min<-0
	tot_loss_med<-0
	tot_loss_max<-0
	i<-1
	for (place in ind) # loop on geocells
	{
		n_fat_min<-0
		n_fat_med<-0
		n_fat_max<-0
		for (type in 1:nclass) # loop on building types
		{	
			#get dpm for most likely vulnerability
			dpm<-ems98_DPM(vuln_emca[type],3)
			
			#compute prob of collapse
			pd4<-dpm[mmi[intens[place]],'d4']
			pd5<-dpm[mmi[intens[place]],'d5']
			
			# expected min (5%) num  
			b_min<-(places@data)[place,off+type]	
			# expected median 
			b_med<-(places@data)[place,off+nclass+type]	
			# expected max (95%) num  
			b_max<-(places@data)[place,off+2*nclass+type]	
			
			# number of expected collapses
			# uses damage grade 5 plus 25% of damage grade 4 (see Spence 2002)
			pd<-(pd5+0.25*pd4)
			n_coll_min<-pd*b_min
			n_coll_med<-pd*b_med
			n_coll_max<-pd*b_max
			
			# addd on the expected fatalities
			n_fat_min<-n_fat_min+(occupancy[type]*coeff*n_coll_min)
			n_fat_med<-n_fat_med+(occupancy[type]*coeff*n_coll_med)
			n_fat_max<-n_fat_max+(occupancy[type]*coeff*n_coll_max)
		}
		
		# absolute loss
		loss[i,1]<-round(n_fat_min)
		loss[i,2]<-round(n_fat_med)
		loss[i,3]<-round(n_fat_max)
		
		# total loss
		tot_loss_min<-tot_loss_min+loss[i,1]
		tot_loss_med<-tot_loss_med+loss[i,2]
		tot_loss_max<-tot_loss_max+loss[i,3]
		
		# relative loss: fatalities as percentage of population
		pop<-(places@data)$population[place]
		loss[i,4]<-n_fat_min/pop
		loss[i,5]<-n_fat_med/pop
		loss[i,6]<-n_fat_max/pop
		
		i<-i+1
	}
	df2<-data.frame(geocells[ind,]@data,est_fat)
	df3<-SpatialPolygonsDataFrame(geocells[ind,],df2)
	
	# write loss estimates to the db
	#db_conn<-"PG:dbname=test_db host='localhost' user='postgres' password='postgres'"
	
	#dbconn<-"PG:dbname='Bishkek2012' host='localhost' user='postgres' password='postgres'"
	#dbout<-list(dbconn=dbconn,layer='urbanstructures_loss',overwrite=TRUE)
	
	writeOGR(df3,dbout$dbconn,dbout$layer, "PostgreSQL",overwrite_layer=dbout$overwrite)
	
	#uncomment to save them as a shapefile
	#path<-'/home/max/Documents/GFZ_sync/workspace/yurta/Loss'
	#writeOGR(df3,path,"geocells_loss", driver="ESRI Shapefile")
	
	return(tot_fatalities=sum(est_fat))
}


###########################################################################################################
# TESTING
###########################################################################################################

# test
# type=1 : test compute_loss_points
# test=2 : test compute_loss_geocells
#
test_loss<-function(type)
{
	#load shakemap
	#file<-sprintf("%s%s",path,'rasters/buildings_density_2_utm32n.tiff')
	path<-'/home/max/Documents/GFZ_sync/workspace/yurta/'
	file<-sprintf("%s%s",path,'Intensities/intensities.tif')
	GDALinfo(file)
	#read data from file
	shakemap_gdal= readGDAL(file)
	#convert to raster
	shakemap<-raster(shakemap_gdal)
	
	dbconn<-"PG:dbname=test_db host='localhost' user='postgres' password='postgres'"
	dbconn2<-"PG:dbname=centralasia_db host='lhotse.gfz-potsdam.de' user='postgres' password='postgres'"
	
	if (type==1)
	{
		dbin<-list(dbconn=dbconn2,layer='places',overwrite=FALSE)
		dbout<-list(dbconn=dbconn2,layer='places_loss',overwrite=TRUE)
		compute_loss_points(shakemap,nighttime=FALSE,dbin,dbout)
	} 
	else if (type==2)
	{
		# test geocell losses
		dbin<-list(dbconn=dbconn2,layer='urbanstructures',overwrite=FALSE)
		dbout<-list(dbconn=dbconn2,layer='urbanstructures_loss',overwrite=TRUE)
		compute_loss_geocells(shakemap,nighttime=FALSE,dbin,dbout)
	}
	
}

