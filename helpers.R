
load(url('http://gadm.org/data/rda/IRL_adm1.RData'))
# loads an Object "gadm" with shape of Ireland
countiesa <- gadm
countiesa<- (gadm$NAME_1)
percent_spplot <- function(var, color, legend.title, min = 0, max = 100) {

  # generate vector of fill colors for spplot
  shades <- colorRampPalette(c("white", color))(100)
  
  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100, 
    include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]



#
plot.new()


# plot choropleth map

 print(spplot(gadm,"NAME_1",colorkey=TRUE, col.regions = fills, add = TRUE,lty = 0))

# overlay county borders

print(spplot(gadm,"NAME_1", col = "black", col.regions = fills, add = TRUE, lty = 1, lwd = 1))
  
  # add a legend
inc <- (max - min) / 4
legend.text <- c(paste0(min, "%"),
                 paste0(min + inc, "%"),
                 paste0(min + 2 * inc, "%"),
                 paste0(min + 3 * inc, "%"),
                 paste0(max, "%"))

legend("bottomleft", 
       legend = legend.text, 
       fill = shades[c(25,50,75)], 
       title = legend.title)




}
