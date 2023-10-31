# As a data engineer, you have been tasked to visualize a dataset in a virtual environment (VR). 
# After analyzing the data (datset_aframe.csv), you decide to plot a three-dimensional scatterplot with a-frame. your task is.

# a) Prepare your data to be plotted in the right format in R file. Consider only three variables with a bubble size of 3. 
# Assign a distinct color (e.g., red, green, blue, yellow) to each type of observation (target variable). 
# For instance, rows 2-11 can be represented in red, and rows 12-21 in green.

# b) Using A-Frame, include the data you have organized in the previous step in your HTML file. 
# Use a forest environment with 100 mushrooms in the background. 

library(r2vr)
library(jsonlite)
library(ggplot2)
a_scatterplot <- function(json_data, x, y, z, ...){
  ## js sources for scatterplot
  .scatter_source <- "https://cdn.rawgit.com/zcanter/aframe-scatterplot/master/dist/a-scatterplot.min.js"
  .d3_source <- "https://cdnjs.cloudflare.com/ajax/libs/d3/4.4.1/d3.min.js"
  ## Create in-memory asset for JSON data
  ## A regular a_asset could be used that points to a real file
  ## this is necessary in a vignette to avoid CRAN issues.
  json_file_asset <- a_in_mem_asset(id = "scatterdata",
                                    src = "./scatter_data.json",
                                    .data = json_data)
  a_entity(.tag = "scatterplot",
           src = json_file_asset,
           .js_sources = list(.scatter_source, .d3_source),
           x = x,
           y = y,
           z = z, ...)
} # end of function
# convert the dataset into a JSON file
diamonds_json <- jsonlite::toJSON(diamonds)
# create scene
my_scene <- a_scene(.template = "empty",
                    .children = list(
                      a_scatterplot(diamonds_json, # dataset
                                    x = "depth", y = "carat", z = "table", # choose columns in your dataset
                                    val = "price", # colour scale
                                    xlabel = "depth", ylabel = "carat", zlabel = "table", #axis labels
                                    showFloor = TRUE,
                                    ycage = TRUE,
                                    title = "Price of Diamonds in Dollars $$$",
                                    pointsize = "10", # try different values
                                    position = c(0, 0, -2),
                                    scale = c(3,3,3)),
                      a_pc_control_camera()))
# Serve a scene
my_scene$serve()
# Shift + Click to open it in your web browser
# http://127.0.0.1:8080
# Stop serving a scene
my_scene$stop()
