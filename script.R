# Aparna K
# Setup -------------------------------------------------------------------

library(tidyverse)
library(sf)

# Read boundary description file

desc <- paste(readLines("./cau_boundary_desc.txt"),collapse = "\n") %>% 
      str_replace_all("[”]","") %>% 
  str_replace_all("[\"]","") %>% 
  str_replace_all("[’]","") %>% 
  str_replace_all("[']","") %>% 
  str_replace_all("[\n]"," ")

#expr "n," means at least n times
#expr {n} means exactly n

latitude <- "(N)[ ]([0-9]{1,})[ ]([0-9]{1,2})([' ]{1,2})([0-9]{1,})([.]{0,})([0-9]{0,}([\"]{0,}))"

longitude <-"(E)[ ]([0-9]{1,})[ ]([0-9]{1,2})([' ]{1,2})([0-9]{1,})([.]{0,})([0-9]{0,}([\"]{0,}))" 



# Extract geometry --------------------------------------------------------



#Extract latitudes

lat <- as.data.frame(str_extract_all(desc,latitude), col.names = "N")   # extracts the exact match
lat 
lat <- as.data.frame(str_split_fixed(lat$N," ", n = 4)) %>%  # Split into columns
  
  set_names("V1","deg","min","sec") %>% # Set column names
  
  mutate(deg = as.numeric(deg),
         min= as.numeric(min),
         sec = as.numeric(sec))  

degree <- str_length(as.character(lat$deg))
deg <- as.character(lat$deg)

for(i in 1:nrow(lat)){
  if(degree[i] == 3){
    str_sub(deg[i],3,3) <-""
  }
}

deg <-as.numeric(deg)
lat$deg <- deg


lat <- lat %>% 
    mutate(dec= deg+min/60+sec/3600) %>% # Convert to decimal degrees
  
  select(dec)

lat


# Extract longitudes

long <- as.data.frame(str_extract_all(desc,longitude), col.names = "N")   # extracts the exact match
long
long <- as.data.frame(str_split_fixed(long$N," ", n = 4)) %>%  # Split into columns
  
  set_names("V1","deg","min","sec") %>% # Set column names
  
  mutate(deg = as.numeric(deg),
         min= as.numeric(min),
         sec = as.numeric(sec)) 

degree2 <- str_length(as.character(long$deg))
deg2 <- as.character(long$deg)

for(i in 1:nrow(long)){
  if(degree2[i] == 3){
    str_sub(deg2[i],3,3) <-""
  }
}

deg2 <-as.numeric(deg2)
long$deg <- deg2

  
long <- long %>% 
  mutate(dec= deg+min/60+sec/3600) %>% # Convert to decimal degrees
  
  select(dec)

long



# Create and export the PA polygon ------------------------------------------------------


# Create the point data frame
df <- data.frame(lon = long$dec, lat = lat$dec)


# Convert to sf POINT geometries
polygon <- df %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

# Plot the resulting polygon
plot(polygon)

# Export as shapefile
st_write(polygon, dsn="E:/Task/Boundary_shp/",layer ="PA", driver= "ESRI Shapefile")
