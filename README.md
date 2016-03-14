# state-county-choropleths
Contains two functions, countymap() and statemap() that return facetted state or county-level maps of specified values in quantiles.

##Dependencies
requires R packages ggplot2, RColorBrewer, maps, rgdal, dplyr, mapproj, grid

##countymap(cnty.dat, map.var, nquant, labels)

*cnty.dat*: a data.frame minimally containing:

- state, a variable with full state names, i.e. ("Alabama", "South Dakota"). If provided state abbreviations, will convert using cleanstate()
- cname, a variable with county names (i.e. "King County", "Orleans Parish"). countymap() will strip out the words "county" and "parish", along with punctuation, no need to pre-process

*map.var*: a vector or list of vectors containing continuous variables (must be equal length of cnty.dat, generally a variable within the data frame)

*nquant*: an integer specifying the number of quantiles to divide map.var into for mapping

*labels*: a vector of character labels for variables in map.var, i.e. ("Income per cap"), length(map.var) must equal length(labels)

### example

##statemap(state.dat, map.var, nquant, labels)
*cnty.dat*: a data.frame minimally containing:

- state, a variable with either two letter state abbreviations in capital letters (i.e. "AL", "AK" ...) or full names

*map.var*: a vector or list of vectors containing continuous variables

*nquant*: an integer specifying the number of quantiles to divide map.var into for mapping

*labels*: a vector of character labels for variables in map.var, i.e. ("Income per cap"), length(map.var) must equal length(labels)

### example
