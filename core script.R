library(sf)
library(geojsonio)
data <- read.csv('testdata.csv')
#ct_geojson <- ct2010_geojson[,7]
#geojson_write(ct_geojson, file = 'ct_geojson')
ct2010_geojson <- geojson_read('ct_geojson.geojson', method = 'local', what = 'sp')
ct2010_geojson_pl <- st_transform(st_as_sf(ct2010_geojson), 2163)
for (i in 1:nrow(data)) {
  coords_pl <- st_transform(st_sfc(st_point(c(data$lon[i], data$lat[i])), crs = 4326), 2163)
  data$ct[i] <- as.numeric(ct2010_geojson[which(st_intersects(coords_pl, ct2010_geojson_pl, sparse = FALSE)),]$ct2010)
}

library(DBI)
# Helper for getting new connection to Cloud SQL
getSqlConnection <- function(){
  con <-
    dbConnect(
      RMySQL::MySQL(),
      username = 'shaunkhoo',
      password = 'password1',
      host = '127.0.0.1',
      dbname = 'backend'
    ) # TODO: use a configuration group `group = "my-db")`
  return(con)
}
conn <- getSqlConnection()
DBI::dbWriteTable(conn, value = data, row.names=FALSE, name='tutees', append=TRUE) # writing the data into the MySQL table
DBI::dbGetQuery(conn, 'CREATE TABLE tutees (tutee_id INT NOT NULL AUTO_INCREMENT, fn VARCHAR(35), ln VARCHAR(35), grade VARCHAR(35), subject VARCHAR(35), language VARCHAR(35), lat DECIMAL(10, 7), lon DECIMAL(10, 7), avail VARCHAR(500), ct BIGINT, PRIMARY KEY(tutee_id));')
DBI::dbGetQuery(conn, 'SELECT * FROM tutees;')
DBI::dbGetQuery(conn, 'drop table tutees;')





#%>%
addPolygons(data = final_geojson, weight = 0, color = ~count_palette(final_geojson@data$count),
            group = 'Census Tract', opacity = 0.5, fillOpacity = 0.3,
            popup = final_geojson@data$count,
            options = leafletOptions(pane = 'ct')) 

count_palette <- colorNumeric(palette = 'Reds', domain = final_geojson@data$count)
