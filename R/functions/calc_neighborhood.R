library(sf)
library(rio)
library(dplyr)
library(purrr)
library(furrr)
library(reshape2)

# Function to calculate location summary for a single geocoded point
# input is a point spatial object (I recommend projection with meter units!!!)

calc_neighborhood <- function(pnt, points, micro, radius = 100, steps = 10){
  
  # set up neighborhood thresholds
  thresh <- (radius/steps)*(1:steps) 
  
  thresh %>% 
    # map function to calculate variables at each threshold
    map(get_n_value, pnt = pnt) %>% 
    # collapse list into data frame of results
    bind_rows() %>% 
    # make "r" a factor for reshaping
    mutate(r = factor(r),
           id = "id") %>% 
    # reshape results into a single row
    melt() %>% 
    dcast(id ~ variable + r) %>% 
    # drop id var
    select(-id)
}


# function to compute values at each radius
get_n_value <- function(r, pnt){
  
  # grab neighborhood points for this r value
  n <- points[st_buffer(pnt, dist = r), , op = st_intersects]
  # remove pnt from its own neighborhood
  n <- filter(n, serial != pnt$serial)
  
  if (nrow(n) > 0){
    # compute results
    micro %>% 
      # limit to people in selected households
      filter(serial %in% n$serial) %>% 
      # compute summary values
      summarise(
        pct_black = sum(black) / n(),
        pct_wfrn = sum(white_frnbrn) / n(),
        mean_sei = ifelse(is.nan(mean(seius, na.rm = T)), NA, mean(seius, na.rm = T)),
        r = r
      )
  } else {
    tibble(pct_black = NA,
           pct_wfrn = NA,
           mean_sei = NA,
           r = r)
  }
}
