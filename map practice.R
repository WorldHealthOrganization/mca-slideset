library(rgdal)
library(broom)
library(maptools)
library(rgeos)

WHOspat <- readOGR(dsn = "C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/MapTemplate_generalized_2013/Shapefiles")

#WHOspat <- readOGR(dsn = "C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/MapTemplate_detailed_2013/Shapefiles")
disputed_border <- readOGR(dsn = "C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/MapTemplate_detailed_2013/Shapefiles/disputed_borders")
disputed_area <- readOGR(dsn = "C:/Users/kpeve/OneDrive - London School of Hygiene and Tropical Medicine/My OneDrive Documents/WHO/MapTemplate_detailed_2013/Shapefiles/disputed_areas")

 plot(WHOspat)
 plot(disputed_border)
 plot(disputed_area)
 
 # convert spatial object to a ggplot ready data frame
 WHOspat_df <- tidy(WHOspat, region = "ISO_3_CODE")
 # make sure the shapefile attribute table has an id column
 WHOspat$id <- rownames(WHOspat@data)
 # join the attribute table from the spatial object to the new data frame
 WHOspat_df <- left_join(WHOspat_df, WHOspat@data, by = "id")
 
 WHOspat_df$region <- countrycode(WHOspat_df$ISO_3_CODE, origin = "iso3c", destination = 'country.name')

 #repeat for disputed
 disputed_area_df <- tidy(disputed_area, region = "NAME") %>%left_join(disputed_area@data, by=c("id"="NAME"))
 
 disputed_border_df <- tidy(disputed_border, region = NULL) %>%left_join(disputed_border@data %>% mutate(id=rownames(disputed_border@data)), by=c("id"="id"))
 
 # ggplot() +
 #   geom_path(data = disputed_area %>% filter(NAME=="Western Sahara"), aes(x = long, y = lat, group = group))
 
 WHOspat_df$value <- 1
 WHOspat_df$value[WHOspat_df$ISO_2_CODE==USA] <- 2
 
 ggplot()+
    geom_map(data = WHOspat_df, map=WHOspat_df, aes(x = long, y = lat, group = group, map_id=region, fill=value), colour="black", size=0.2)+
    geom_map(data = disputed_area_df %>% filter(id=="Western Sahara" | id== "Abyei" | id=="Jammu and Kashmir" ), map=disputed_area_df, aes(x = long, y = lat, map_id=id), fill="grey", colour="#bdbdbd", size=0.2)+
    geom_path(data = disputed_border_df %>% filter(NAME=="Kosovo"), aes(x = long, y = lat), colour="black")+
    geom_path(data = disputed_border_df %>% filter(NAME=="Bir Tawil (EGY Claim)" ), aes(x = long, y = lat), colour="black")+
    geom_path(data = disputed_border_df %>% filter(NAME=="Halayib Triangle (SDN Claim)"), aes(x = long, y = lat), colour="black")+
    geom_path(data = disputed_border_df %>% filter(NAME=="Halayib Triangle (EGY Claim)"), aes(x = long, y = lat), colour="black")+
    geom_path(data = disputed_border_df %>% filter(NAME=="Ilemi Triangle"), aes(x = long, y = lat), colour="black")+
    geom_path(data = disputed_border_df %>% filter(NAME=="Kosovo"), aes(x = long, y = lat), colour="black")+
    geom_path(data = disputed_border_df %>% filter(NAME=="Gaza Strip"), aes(x = long, y = lat), colour="black", linetype=2)+
    geom_path(data = disputed_border_df %>% filter(NAME=="West Bank"), aes(x = long, y = lat), colour="black", linetype=2)+
    geom_map(data = disputed_area_df %>% filter(id=="Aksai Chin" ), map=disputed_area_df, aes(x = long, y = lat, map_id=id), fill="grey", colour="#bdbdbd", size=0.2)  +
    geom_map(data = disputed_area_df %>% filter(id!="Western Sahara" & id!= "Abyei" & id!="Jammu and Kashmir" & id!="Aksai Chin" ), map=disputed_area_df, aes(x = long, y = lat, map_id=id), fill="blue", colour="#bdbdbd", size=0.2) 

 
 


    
    
# can i subtract out disputed bordres

x <- left_join(WHOspat_df, disputed_border_df, by=c("long", "lat"))
