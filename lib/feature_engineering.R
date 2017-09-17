setwd("D:/data_camp/zillow_project")
train <- read.csv("train_property.csv",stringsAsFactors = FALSE)
#dealing with missing data first
train$trans_year <- sapply(strsplit(train$transactiondate, '-'), '[[', 1)
train$trans_month <- sapply(strsplit(train$transactiondate, '-'), '[[', 2)
train$trans_day <- sapply(strsplit(train$transactiondate, '-'), '[[', 3)
train$trans_weekday <- weekdays(as.Date(train$transactiondate))
train$trans_DATE <- as.Date(train$transactiondate)


# train rename


train <- plyr::rename(train,
                      c("parcelid"="id_parcel",
                        "transactiondate" = "trans_date",
                        "yearbuilt" = "build_year",
                        "basementsqft"="area_base_living",
                        "yardbuildingsqft17"="area_patio",
                        "yardbuildingsqft26"="area_shed",
                        "poolsizesum"="area_pool",
                        "lotsizesquarefeet"="area_lot",
                        "garagetotalsqft"="area_garage",
                        "finishedfloor1squarefeet" = "area_firstfloor_finished",
                        "calculatedfinishedsquarefeet" = "area_total_calc",
                        "finishedsquarefeet6" = "area_base",
                        "finishedsquarefeet12" = "area_live_finished",
                        "finishedsquarefeet13" = "area_liveperi_finished",
                        "finishedsquarefeet15" = "area_total_finished",
                        "finishedsquarefeet50" = "area_unknown",
                        "unitcnt" = "num_unit",
                        "numberofstories" = "num_story",
                        "roomcnt" = "num_room",
                        "bathroomcnt" = "num_bathroom",
                        "bedroomcnt" = "num_bedroom",
                        "calculatedbathnbr" = "num_bathroom_calc",
                        "fullbathcnt" = "num_bath",
                        "threequarterbathnbr" = "num_75_bath",
                        "fireplacecnt" = "num_fireplace",
                        "poolcnt" = "num_pool",
                        "garagecarcnt" = "num_garage",
                        "regionidcounty" = "region_county",
                        "regionidcity" = "region_city",
                        "regionidzip" = "region_zip",
                        "regionidneighborhood" = "region_neighbor",
                        "taxvaluedollarcnt" = "tax_total",
                        "structuretaxvaluedollarcnt" = "tax_building",
                        "landtaxvaluedollarcnt" = "tax_land",
                        "taxamount" = "tax_property",
                        "assessmentyear" = "tax_year",
                        "taxdelinquencyflag" = "tax_delinquency",
                        "taxdelinquencyyear" = "tax_delinquency_year",
                        "propertyzoningdesc" = "zoning_property",
                        "propertylandusetypeid" = "zoning_landuse",
                        "propertycountylandusecode" = "zoning_landuse_county",
                        "fireplaceflag" = "flag_fireplace",
                        "hashottuborspa" = "flag_tub",
                        "buildingqualitytypeid" = "quality",
                        "buildingclasstypeid" = "framing",
                        "typeconstructiontypeid" = "material",
                        "decktypeid" = "deck",
                        "storytypeid" = "story",
                        "heatingorsystemtypeid" = "heating",
                        "airconditioningtypeid" = "aircon",
                        "architecturalstyletypeid" = "architectural_style",
                        "pooltypeid10" = "flag_spa",
                        "pooltypeid2" = "flag_pool_spa",
                        "pooltypeid7" = "flag_pool_tub",
                        "fips"="county"))


# correct variable type


# numerical variable
variable_numeric = c("area_firstfloor_finished",
                     "area_base", "area_base_living",
                     "area_garage", 
                     "area_live_finished",
                     "area_liveperi_finished",
                     "area_lot",
                     "area_patio",
                     "area_pool",
                     "area_shed",
                     "area_total_calc",
                     "area_total_finished",
                     "area_unknown",
                     "tax_building",
                     "tax_land",
                     "tax_property",
                     "tax_total",
                     "latitude",
                     "longitude")
# discrete
variable_discrete = c("num_75_bath",
                      "num_bath",
                      "num_bathroom",
                      "num_bathroom_calc",
                      "num_bedroom",
                      "num_fireplace",
                      "num_garage",
                      "num_pool",
                      "num_room",
                      "num_story",
                      "num_unit")

variable_binary = c("flag_fireplace",
                    "flag_tub",
                    "flag_spa",
                    "flag_pool_spa",
                    "flag_pool_tub",
                    "tax_delinquency")

# categorical variable
variable_nominal = c("aircon",
                     "architectural_style",
                     "county",
                     "deck",
                     "framing",
                     "heating",
                     "id_parcel",
                     "material",
                     "region_city",
                     "region_county",
                     "region_neighbor",
                     "region_zip",
                     "story",
                     "zoning_landuse",
                     "zoning_landuse_county")
variable_ordinal = c("quality")

# date
variable_date = c("tax_year",
                  "build_year",
                  "tax_delinquency_year",
                  "trans_year",
                  "trans_month",
                  "trans_day",
                  "trans_date",
                  "trans_weekday")

# others
variable_unstruct = c("zoning_property")

# don't understand
variable_unknown = c('censustractandblock',
                     'rawcensustractandblock')


# Conversion
# - convert some binary to 0, 1
# - convert to date to int
# - convert to numeric to double
# - convert to discrete to int
# - convert to categorical to character


train[train$flag_fireplace == "", "flag_fireplace"] = 0
train[train$flag_fireplace == "true", "flag_fireplace"] = 1
train[train$flag_tub == "", "flag_tub"] = 0
train[train$flag_tub == "true", "flag_tub"] = 1
train[train$tax_delinquency == "", "tax_delinquency"] = 0
train[train$tax_delinquency == "Y", "tax_delinquency"] = 1
# convert to date to int
train[,variable_date] = sapply(train[,variable_date], as.character)
# convert to numeric to double
train[,variable_numeric] = sapply(train[,variable_numeric], as.numeric)
# convert to discrete to int
train[,c(variable_discrete, variable_binary)] = sapply(train[,c(variable_discrete, variable_binary)], as.integer)
# convert to categorical to character
train[,c(variable_nominal, variable_ordinal)] = sapply(train[,c(variable_nominal, variable_ordinal)], as.character)

#drop  the redundant value
train <- train[, !(names(train) %in% c("num_bath","num_bathroom_calc","region_county","tax_year"))]

#deleting missing value
num.NA <- sort(colSums(sapply(train, is.na)))
remain.col <- names(num.NA)[which(num.NA <= 0.8 * dim(train)[1])] # trainT = train
remain.numeric <- remain.col[which(remain.col %in% variable_numeric)]
remain.discrete <- remain.col[which(remain.col %in% variable_discrete)]

#1. feature engineering from density ->zip code density
zip.num <- by(train, train$region_zip, function(x) nrow(x))
train$zip.num <- zip.num[train$region_zip]
zip.area  <- by(train, train$region_zip, function(x) 
                    {abs((max(x$longitude) - min(x$longitude))*(max(x$latitude) - min(x$latitude)))})

zip.density <- ifelse(zip.area  == 0 , 0 ,zip.num/zip.area)
plot(density(zip.density))
plot(zip.density)
train$zip.density <- zip.density[train$region_zip]
with(train,cor(zip.density,logerror,use = "pairwise.complete.obs"))
with(train,cor(zip.density,abs(logerror),use = "pairwise.complete.obs"))
par(mfrow = c(2,2))
plot(train$zip.density, train$logerror)
plot(train$zip.density, abs(train$logerror))
plot(train$zip.num, train$logerror)
plot(train$zip.num, abs(train$logerror))

trunc <- subset(train,train$zip.density < quantile(train$zip.density[!is.na(train$zip.density)],0.999))
plot(trunc$zip.density, trunc$logerror)

#2. whether it is surrounded by  the  same 
library(class)
up.bound <- quantile(train$logerror, 2/3)
low.bound <- quantile(train$logerror, 1/3)
train$pred.level <- ifelse(train$logerror<low.bound,"low",
                           ifelse(train$logerror <= up.bound,"mid","high"))
region_city_knn_accuracy = list()
#unique(train[!is.na(train$region_city),"region_city"])
for (city in  c("17686")){
  print(city)
if (nrow(train[train$region_city == city,])){
  xTrain = train[train$region_city == as.character(city),c("latitude","longitude")]
  print(xTrain)
  
  yTrain = train[train$region_city == as.character(city),"pred.level"]
  print(yTrain)
  KNNpred = knn(xTrain,xTrain,yTrain, k =5)
  region_city_knn_accuracy = c(region_city_knn_accuracy, city =mean(KNNpred  == yTrain) )
}
  }
train[train$region_city == "12447",c("latitude","longitude")]
