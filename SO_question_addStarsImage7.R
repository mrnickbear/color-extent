#was going to be SO question, but now it works

#Revised question: share my approach with st_crop, and the error that happens if I use the other options (addStarsImage, addGeotiff, addgeoraster)
#make it a shiny app that allows the users to enter the center coordinates of the circle

#the approach works OK for me, since my app already includes 2 dropdowns by which the user identifies the location to be shown.
#The limitation of this approach is when the user wants to see beyond the immediate are of the selected location
#In addition, separate question, how to replicate the QGIS color ramp by extents?  Is it possible?

# v3 - max/min is working with stars sample data.  Next, adjust ramp.
# v4 - deeper in the auto color ramp
# v5 - confirming it works with 2023 SF lidar (yes, but very slowly)
# v7 - cleanup more for publishing, and added hillshade.  Published at https://sfpw.shinyapps.io/webmap/

# try addGeotiff again?

library(sf)
library(shiny)
library(leaflet)
library(mapview)
library(leafem)
library(stars)
library(RColorBrewer) # For a nice color palette
library(hillshader)
library(dplyr) #filter, the core R filter() must be different???


# #Example with SF 2023 lidar - one tile
# #For testing, 16MB .tif, no .ovr
# tif <- "K:\\sfLidar\\2023\\usgs_project_archive\\CA_SanFrancisco_1_B23\\bare_earth\\be_rasters\\04200270.tif"
# x <- read_stars(tif)
# streets <-  read_sf("K:\\sfbase\\arcview\\SFCity\\ACTIVESTREETS\\activestreets.shp")
# cliffhouse <- filter(streets, cnn == 10544000) #cliffhouse
# feature <- st_transform(cliffhouse, crs = st_crs(x))


# Full version I want to use later
# #17GB .tif, 5GB .ovr
# demfile <- "K:\\sfLidar\\2023\\dem25cm_2023-04-20\\sf3dxyz_dem25cm_float32.tif"  #cannot allocate vector of size 16.2 Gb
# x <- read_stars(demfile)


#For testing with builtin package data
tif = system.file("tif/olinda_dem_utm25s.tif", package = "stars")
x <-  read_stars(tif)
feature <-  st_sfc(st_buffer(st_point(c(290000, 9120000)), 400), crs = st_crs(x))

# hs <- hillshader(as(x, "Raster"))
hs <- hillshader(as(x, "Raster"),
  c("ray_shade", "ambient_shade"),
  sunangle = 320,
  sunaltitude = 80
  )
hs <- st_as_stars(hs)
st_crs(hs) <- st_crs(x)
# plot(hs, col = grey(70:100/100), legend = FALSE)


ui <- fluidPage(
  div(
    h4("Max/Min colors adjusted per current view:") ,
    textOutput("maxmin")
  ),
  leafletOutput("dynamicRasterMap", width = 400, height = 400)
)
  
server <- function(input, output, session) {  #errors line numbers start from here

  # Reactive expression to store map bounds
  map_bounds <- reactiveVal(NULL)
  
  # Observe Leaflet map 'zoomend' and 'moveend' events
  # When the map stops moving or zooming, update the bounds
  observeEvent({
    input$dynamicRasterMap_zoom
    input$dynamicRasterMap_bounds
  }, {
    if (!is.null(input$dynamicRasterMap_bounds)) {
      bounds <- input$dynamicRasterMap_bounds
      # The input$map_bounds provides a list with north, south, east, west
      # Convert to an sf_bbox object
      bbox_sf <- st_bbox(c(xmin = bounds$west, ymin = bounds$south,
                           xmax = bounds$east, ymax = bounds$north),
                         crs = st_crs(x))
      map_bounds(bbox_sf)
    }
  }, ignoreNULL = FALSE, once = FALSE) # ignoreNULL = FALSE to capture initial state, once = FALSE for continuous updates
  
  
  output$dynamicRasterMap <- renderLeaflet({
    # Initial mapview render with full extent and default color ramp
    # This will be updated reactively below

    m <- mapview(feature, legend = FALSE, project = FALSE) %>%
      addStarsImage(st_as_stars(x), legend = TRUE, project = FALSE) %>% #, at = seq(min_value, max_value, length.out = 10)))
      addStarsImage(hs,
                     colors = grey(60:90/100), # Grayscale for hillshade
                     opacity = 0.5,             # Adjust opacity to see DEM underneath
                     group = "Hillshade")       # Group for layer control)
    m  #Without stars, need m@map
  })
  
  # Reactive expression to generate the mapview object based on map_bounds
  # This triggers when current_map_bounds changes
  observeEvent(map_bounds(), {
    req(map_bounds()) # Require map bounds to proceed
    
    current_bounds <- map_bounds()
    
    s_ext <- st_as_sfc(current_bounds)
    st_crs(s_ext) <- st_crs(4326)
    s_ext <- st_transform(s_ext, st_crs(x))
    
    cropped_raster <- tryCatch({
      st_crop(x, s_ext)
    }, error = function(e) {
      cat("Crop error\n")
      NULL # Handle cases where cropping might fail (e.g., no overlap)
    })
    
    if (length(cropped_raster[[1]]) > 0 && !all(is.na(cropped_raster[[1]]))) {
      # if (!is.null(cropped_raster)) {# && !is.empty(cropped_raster)) {
        max_val <-as.vector(max(cropped_raster[[1]], na.rm = TRUE))
        min_val <-as.vector(min(cropped_raster[[1]], na.rm = TRUE))
      
      # Define color breaks based on visible min/max
      # Ensure min != max to avoid errors with seq()
      if (is.finite(min_val) && is.finite(max_val) && min_val != max_val) {
        color_breaks <- seq(min_val, max_val, length.out = 10)
      } else {
        # Fallback for flat or empty visible areas
        color_breaks <- NULL # mapview will use its default if at is NULL
      }   
        
      palette_colors <- colorRampPalette(brewer.pal(length(color_breaks) - 1, "Spectral"))(length(color_breaks))

      sorted_color_values <- sort(unique(color_breaks))
      sorted_palette_colors <- palette_colors[order(color_breaks)]
      
      color_fun <- colorNumeric(palette = sorted_palette_colors,
                                domain = range(color_breaks),
                                na.color = "transparent")
        
      # Update the mapview proxy with the new raster layer and color ramp
      # Use mapview::mapviewProxy to update an existing map
      # mapviewProxy("dynamicRasterMap") %>%
      leafletProxy("dynamicRasterMap") %>%
        clearImages() %>% # Remove the old raster layer

        addStarsImage(
          st_as_stars(cropped_raster), # x), # Always pass the full raster data to mapview
          alpha = 0.8,
          at = color_breaks,
          colors = color_fun, # Choose a color palette
          na.rm = TRUE,
          layerId = "dynamic_raster_layer" # Assign an ID for easy removal/replacement
        ) %>%
        addStarsImage(hs,
                     colors = grey(60:100/100), # Grayscale for hillshade
                     opacity = 0.5,             # Adjust opacity to see DEM underneath
                     group = "Hillshade")       # Group for layer control)
      
    } else {
    # If no data in the visible extent, clear the raster image
      leafletProxy("dynamicRasterMap") %>%
      clearImages()
    }

  })
  
} #End of server

options(shiny.port = 7771)
options(shiny.host = "10.90.107.197") #IDC49NBEAR2 = 10.90.107.197 #use 0.0.0.0 for posting publicly
shinyApp(ui, server)