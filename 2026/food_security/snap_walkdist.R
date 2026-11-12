library(tidycensus)
library(sf)
library(dplyr)
library(dodgr)
library(osmextract)

########################################################################################
# Settings
########################################################################################

# options: "Grocery", "Deli/Bodega", "Non-Grocery", "Uncategorized", "Support Grocery"  
snap_type = "Grocery"


########################################################################################
# read in data
########################################################################################

nyc_counties <- c("005", "047", "061", "081", "085") 

boro_bounds = st_read("https://data.cityofnewyork.us/api/v3/views/gthc-hcne/query.geojson")

tracts <- get_acs(
  geography = "tract", variables = "B01003_001",  
  state = "NY", county = nyc_counties,
  year = 2023, geometry = TRUE, cb = TRUE) |>
  select(GEOID, pop = estimate) %>% 
  filter(pop > 0) %>% 
  st_transform(4326) %>%
  st_intersection(boro_bounds)

snap = readRDS("data/output/snap_categorized.RDS") %>% 
  st_transform(4326) %>% 
  filter(categories == snap_type)


########################################################################################
# get street network graph
########################################################################################

study_box <- st_bbox(st_buffer(st_transform(tracts, 2263), 5000) |> st_transform(4326))

graph_file <- "data/output/nyc_walk_graph.rds"
if (file.exists(graph_file)) {
  graph <- readRDS(graph_file)
} else {
  # downloaded OSM extract, pulls the Geofabrik New York State .pbf once
  # (a few hundred MB, cached by osmextract), and clips it to the study area while reading.
  streets <- oe_get(
    "New York",
    provider = "geofabrik",
    layer = "lines",
    query = "SELECT osm_id, highway, geometry FROM lines WHERE highway IS NOT NULL",
    #vectortranslate_options = c("-spat", as.character(study_box[c("xmin", "ymin", "xmax", "ymax")])),
    quiet = FALSE
  )
  message("streets: ", nrow(streets), " rows; class = ", paste(class(streets), collapse = "/"))
  stopifnot("No street lines were read - check study_box / the -spat clip" = nrow(streets) > 0)
  
  # dodgr wants a plain sf object of LINESTRINGs with a `highway` column
  streets <- streets |>
    as.data.frame() |>
    st_as_sf() |>
    st_cast("LINESTRING") |>
    select(osm_id, highway)
  
  graph <- weight_streetnet(streets, wt_profile = "foot", id_col = "osm_id") |>
    dodgr_contract_graph()          # speeds up routing a lot
  saveRDS(graph, graph_file)
}


########################################################################################
# prep to calculate distances to stores
########################################################################################

xy <- function(x) st_coordinates(x) |> as.matrix() |> `colnames<-`(c("x", "y"))

# convert tract polygons to centroid points
centroids <- tracts |>
  st_intersection(boro_bounds) |>
  st_transform(2263) |>
  st_centroid() |>
  st_transform(4326)

# Routing points must be graph vertex IDs, not raw coordinates, so snap both sets
# to the nearest vertex. connected = TRUE avoids snapping onto isolated fragments.
verts   <- dodgr_vertices(graph)
iv_from <- match_points_to_verts(verts, xy(centroids), connected = TRUE)  
iv_to   <- match_points_to_verts(verts, xy(snap),      connected = TRUE) 
from_id <- verts$id[iv_from]                                             
to_id   <- verts$id[iv_to]                                       

# how far each point moved to reach the network (metres).    
vpts     <- function(iv) st_as_sf(verts[iv, c("x", "y")], coords = c("x", "y"), crs = 4326)
off_from <- as.numeric(st_distance(centroids, vpts(iv_from), by_element = TRUE))
off_to   <- as.numeric(st_distance(snap,      vpts(iv_to),   by_element = TRUE))

# contract the graph for speed. keep every snapped vertex for exact distances
graph_c <- dodgr_contract_graph(graph, verts = unique(c(from_id, to_id)))

# make sure contraction kept every snapped vertex. if not, use full graph
if (!all(c(from_id, to_id) %in% dodgr_vertices(graph_c)$id)) {
  message("Contraction dropped some snapped vertices; routing on the uncontracted graph instead.")
  graph_c <- graph
}


########################################################################################
# calculate distances to stores
########################################################################################

from_u <- unique(from_id)

# one store per vertex: if several stores snap to the same vertex, keep the one
# that was closest to the network 
to_tbl <- tibble(id = to_id, off = off_to, idx = seq_along(to_id)) |>     
  arrange(off) |>                                                         
  distinct(id, .keep_all = TRUE)                                         
to_u <- to_tbl$id                                                        

chunks <- split(from_u, ceiling(seq_along(from_u) / 500))
nearest <- bind_rows(lapply(chunks, function(f) {
  d <- dodgr_dists(graph_c, from = f, to = to_u, quiet = TRUE)
  stopifnot("dodgr_dists returned no dimnames" = !is.null(rownames(d)) && !is.null(colnames(d)))  # <-- NEW
  d[is.na(d)] <- Inf
  # choose the store that minimises network distance + the store's own offset from the network
  tot <- d + rep(to_tbl$off[match(colnames(d), to_tbl$id)], each = nrow(d)) 
  j   <- max.col(-tot, ties.method = "first")                               
  k   <- cbind(seq_len(nrow(d)), j)                                         
  tibble(from = rownames(d), to = colnames(d)[j],                          
         d = ifelse(is.infinite(d[k]), NA_real_, d[k]))                     
}))


########################################################################################
# format output
########################################################################################

hit       <- match(from_id, nearest$from)                           
store_idx <- to_tbl$idx[match(nearest$to[hit], to_tbl$id)]             

out <- tracts |>
  mutate(
    nearest_snap_idx = store_idx,                                      
    network_dist_m   = nearest$d[hit],                                    
    centroid_snap_m  = off_from,                                          
    store_snap_m     = off_to[store_idx],                                
    walk_dist_m      = network_dist_m + centroid_snap_m + store_snap_m,   
    walk_dist_mi     = walk_dist_m / 1609.344,
    walk_dist_min    = walk_dist_mi*24
  )

# Sanity check: door-to-door walking distance can't be shorter than the straight line
straight <- st_distance(centroids, snap[out$nearest_snap_idx, ], by_element = TRUE) |> as.numeric()
bad <- which(out$walk_dist_m + 5 < straight)                               # (5 m tolerance, no stopifnot)
if (length(bad)) {                                                         # (warning instead of error)
  warning(length(bad), " tracts have a walking distance shorter than the straight line: ",
          paste(head(out$GEOID[bad], 10), collapse = ", "))
}

# Tracts whose centroid is far from any street: inspect these by hand      # <-- NEW
cat(sum(out$centroid_snap_m > 200, na.rm = TRUE), "tracts have a centroid >200 m from the street network\n")  # <-- NEW
summary(out$walk_dist_m)

# check: network distance should never be less than straight-line distance
straight <- st_distance(centroids, snap[out$nearest_snap_idx, ], by_element = TRUE) |> as.numeric()
stopifnot(all(out$walk_dist_m + 1 >= straight, na.rm = TRUE))

summary(out$walk_dist_m)
# st_write(out, "tract_walk_to_snap.gpkg")

ggplot(out[out$walk_dist_min < 30]) + geom_sf(aes(fill=walk_dist_min))

# investigate
i = 1695
ggplot()  + geom_sf(data = snap, col="blue") + geom_sf(data = out[i,], col="red") + geom_sf(data = snap[out$nearest_snap_idx[i], ], col="red")
