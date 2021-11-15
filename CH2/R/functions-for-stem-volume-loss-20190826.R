# Functions to estimate the stem volume above/below a certain
# height, and the estimation of biomass loss when a tree is
# only partially alive (= it has broken or partially dead trunk)

# Gabriel Arellano
# gabriel.arellano.torres

# The trunk is modelled as a series of truncated cones.
# The radiuses of such truncated cones are estimated based on a tappering function.


# Tappering functions
# These functions (and the default parameters) have been taken from Table 2 of:
# Metcalf et al. 2009. Tree growth inference and prediction when the point of measurement changes: modelling around buttresses in tropical forests. Journal ofTropical Ecology (2009) 25:1–12.
# This model has been found the best among several competing models (in tropical forests)
# Cushman et al. 2014. Improving estimates of biomass change in buttressed trees using tree taper models. Methods in Ecology and Evolution 2014, 5, 573–582

diameter_at_a_given_height <- function(dbh, height, alpha = NA)
{
  # start with a warning about height and dbh units
  warning("PLEASE PROVIDE EVERYTHING IN mm")
  
  # use default alpha parameter, if not provided
  # (averages from Table 2 of Metcalf et al.)
  if(is.na(alpha))
  {
    alpha = median(c(-0.0194, -0.0247, -0.0149, -0.0272, -0.1190))
  }
  
  # ensure numeric input
  dbh <- as.numeric(as.character(dbh))
  height <- as.numeric(as.character(height))
  alpha = as.numeric(as.character(alpha))
  
  # apply the model -- the alpha parameter
  # is taken directly from Metcalf et al., and assumes
  # height in meters and dbh in cm.
  dbh.in.cm <- dbh / 10
  height.in.meters <- height / 1000
  d.in.cm <- dbh.in.cm * exp(alpha * (height.in.meters - 1.3))
  d.in.mm <- d.in.cm * 10
  d.in.mm
}


# Get the volume of a truncated cone
get_volume_of_truncated_cone <- function(R, r = 0, h)
{
  if(R < r)
  {
    Rcopy = R
    R = r
    r = Rcopy
  }
  pi * h * (R^2 + r^2 + R*r)/3
}


# Split a given section into many truncated cones, of a given small height (100 mm = 10 cm by default),
# and calculate the accumulated volume of all of them, given the estimated radiuses
get_volume_of_a_portion_of_a_trunk <- function(dbh, h1, h2, height.of.one = 100)
{
  warning("PLEASE PROVIDE EVERYTHING IN mm")
  
  # the sections
  LO = round((h2 - h1) / height.of.one)
  if(LO < 2) LO = 2
  cuts.at <- seq(from = h1, to = h2, length.out = LO)
  height.of.one = cuts.at[2] - cuts.at[1] # re-calculate the height of each section
  radiuses.at <- sapply(cuts.at, function(h) diameter_at_a_given_height(dbh = dbh, height = h)) / 2
  
  # characterize each of the truncated cones
  df <- data.frame(R = radiuses.at[-length(radiuses.at)], r = radiuses.at[-1], h = height.of.one)
  v <- apply(df, 1, function(Rrh) get_volume_of_truncated_cone(R = Rrh[1], r = Rrh[2], h = Rrh[3]))
  
  # return the accumulated volume in mm^3
  V = sum(v)
  V
}


#round(get_volume_of_a_portion_of_a_trunk(dbh = 1000, h1 = 0, h2 = 1000) / 10^9, 3)
#round(1000 * pi * 500^2  / 10^9, 3) # very similar to a short cilinder


# Estimate the proportion of volume above a given height.
get_p_volume_above_h <- function(dbh, h, maximum.height)
{
  warning("PLEASE PROVIDE EVERYTHING IN mm")
  if(h > maximum.height) h = maximum.height
  V1 = get_volume_of_a_portion_of_a_trunk(dbh = dbh, h1 = 0, h2 = h)
  V2 = get_volume_of_a_portion_of_a_trunk(dbh = dbh, h1 = h, h2 = maximum.height)
  p.above = V2 / (V1 + V2)
  p.above
}






