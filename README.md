# 2025_WKCSFO
Workshop on the Celtic Sea ecoregion Fisheries Overview






# RDBES

data_00_create_extendedRDBES.r create an "extended" RDBES data base by duplicating lines when coutry provided lines aggregating vessels. Lines duplicated dividing the landings and value by the number of vessels in the previous line separating CLencryptedVesselIds by either ";" or "/"

!!! differnet special characters per countries...
FI EI LT DE GB-ENG GB-WLS GB-NIR EE SE FR PL NL DK PT are using ";" as a separator for CLencryptedVesselIds
ES uses "/" as a separator for CLencryptedVesselIds
LV uses "-" as a separator for CLencryptedVesselIds

And BE use something very strange that is NOT a vessel id!!!


For now only CLscientificWeight and CLlandingsValue are divided 

ONLY use CLscientificWeight_perVessel CLlandingsValue_perVessel as other variables can be duplicated!!






# spatial plots

when extendedRDBES.RData is created you can run the script functionToCreateSpatialLandings.r that creates html and pdf version of the spatial landings


set the variable threshold to "InNumberOfSpecies" if you want to plot the 5 main species and aggregate everything to OTH or 
"InPercentage" if you want to plot the species representing X% (to be defined in the global variables) and the rest summed in OTH

# "who is fishing"
when extendedRDBES.RData is created you can run the script model_00_FOTextFromRDBES.r
The script is not fully cleaned and has some checks inside...
This script extract the vessels having more than 80% of their total landings in the EcoRegion and then makes some summary per country/fishing technics etc...

it will render 2 html files 
- dataExplorationInTables.html allows for exploring the trends in number of vessels 
- TextCountryFormRDBES.html decribes the number of vessels for the last year
