require(VGAM)
require(MASS)
require(RPostgreSQL)

# estimate the number of builings in a town given:
# total population,
# compositional model of the building types,
# average occupancy.
#
# and compute expected interpercentile ranges
#
# using dirichelet distribution to account for uncertainty
#
estimate_building_num<-function(totpop,bcomp,bcomp_conf,occupancy,percentiles,nsamp)
{
	#generate randomly a number of compositions
	oprob<-rdiric(n=nsamp,shape=bcomp*bcomp_conf)
	
	# stochastical realizations of occupancy per building type
	avgocc_l<-oprob%*%diag(occupancy)
	#average occupancy
	avg_occ<-apply(avgocc_l,1,sum)
	
	#estimated number of buildings
	nbds<-round(totpop/avg_occ)
	
	#estimated number of buldings per type
	nbds_t<-nbds*oprob
	
	#generate resulting percentiles
	ranges<-t(apply(nbds_t,2,function(x){quantile(x,percentiles)}))
	return(ranges)
}

# compute the expected exposure composition 
# of a set of settlements (or geocells), loaded from a DB 
# from a table 'tablename'
# and containing information on the population (field 'population')
# and the percentage of urban area 'urban_ratio'
# updates the table by adding the expected 
# exposure model
compute_exposure<-function(init_exp_table=FALSE, tablename)
{
	#composition model of building types
	#13 classes: (1.1,1.2,1.3,1.4,2,3.1,3.2,3.3,3.4,4,5.1,5.2,6)
	bcomp_urban<-c(0.09,0.05,0.09,0.14,0.22,0.15,0.1,0.01,0.04,.05,.03,.0299,0.0001)
	bcomp_rural<-c(0.11,0.05,0.03,0.07,0.04,0.01,0.01,0.01,0.01,0.49,0.05,0.1,0.02)
	
	#average occupancy
	occupancy <- c(28, 8, 140, 60,200,200,200,200,140,5,28,5,5)
	
	#bcomp_urban<-c(0.37,0.22,0.30,0.05,0.06,0)
	bcomp_urban_conf<-100
	#bcomp_rural<-c(0.26,0.04,0.04,0.49,0.15,0.02)
	bcomp_rural_conf<-100
	
	#confidence in the composition model
	bcomp_conf<-100
	
	# model occupancy (no uncertainty)
	nightime_coeff <- 0.5
	daytime_coeff <- 0.3
	
	#percentiles of the estimation
	percentiles<-c(0.05,0.5,0.95)
	
	# number of occurrences or prior model
	nsamp<-10000
	
	drv <- dbDriver("PostgreSQL")
	con <- dbConnect(drv, host='localhost', user= "postgres", password="postgres", dbname="test_db")
	geocells <- dbGetQuery(con, statement = paste(
					"select gid, population as pop, urban_ratio as ur from ",tablename," where population>0;",sep='')) 
	
	# used only to initialize the table
	if (init_exp_table)
	{
		for (i in 1:39)
		{
			query<-paste("alter table ",tablename," add column bt_",i," numeric;",sep='')
			dbGetQuery(con,statement=query)
		}
	}	
	
	#require(rgdal)
	#shppath<-"/home/max/Documents/GFZ_sync/ACTIVITIES/real-time risk/Scenario_Simulation_DEMO/geodata/osm"
	#places<-readOGR(shppath, "places")
	
	#select only indices where population is > 0
	#ind<-which(places$population>0 & places$population<1e6 )
	
	#length(ind)
		
	urban_set<-geocells$pop*geocells$ur
	rural_set<-geocells$pop*(1-geocells$ur)

	est_bds_urban<-t(sapply(urban_set,function(x) estimate_building_num(x,bcomp_urban,bcomp_urban_conf,occupancy,percentiles,nsamp) ))
	est_bds_rural<-t(sapply(rural_set,function(x) estimate_building_num(x,bcomp_rural,bcomp_rural_conf,occupancy,percentiles,nsamp) ))
	
	est_bds<-est_bds_urban+est_bds_rural
	
	#estimated number of buildings of different types, with uncertainty
	nclass<-13
	#b1=t(est_bds[c(1,nclass+1,2*nclass+1),])
	#b2=t(est_bds[c(2,nclass+2,2*nclass+2),])
	#b3=t(est_bds[c(3,nclass+3,2*nclass+3),])
	#b4=t(est_bds[c(4,nclass+4,2*nclass+4),])
	#b5=t(est_bds[c(5,nclass+5,2*nclass+5),])
	#b6=t(est_bds[c(6,nclass+6,2*nclass+6),])
	#b7=t(est_bds[c(7,nclass+7,2*nclass+7),])
	#b8=t(est_bds[c(8,nclass+8,2*nclass+8),])
	#b9=t(est_bds[c(9,nclass+9,2*nclass+9),])
	#b10=t(est_bds[c(10,nclass+10,2*nclass+10),])
	#b11=t(est_bds[c(11,nclass+11,2*nclass+11),])
	#b12=t(est_bds[c(12,nclass+12,2*nclass+12),])
	#b13=t(est_bds[c(13,nclass+13,2*nclass+13),])
	
	
	# update table in the database
	f<-function(x)
	{
		for (i in 1:39)
		{
			query<-paste("update ",tablename," set bt_",i,'=',round(est_bds[x,i]),' where gid=',geocells$gid[x],sep='')
			dbGetQuery(con,statement=query)
		}
	}
	
	len<-length(geocells$gid)
	sapply(1:len,FUN=f)
	
	
	#create a new spatial data frame with a subset of attributes
	#df1<-as.data.frame(places@coords[ind,])
	#df2<-data.frame(name=as.character(places$name[ind]),population=places$population[ind],est_bds=round(t(est_bds)))
	
	#compose shapefile and save to disk
	#df3<-SpatialPointsDataFrame(df1,df2)
	#df3@proj4string<- places@proj4string
	
	#writeOGR(df3, shppath, "test", driver="ESRI Shapefile")
}