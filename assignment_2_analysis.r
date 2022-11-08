library(stats)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

all <- read.csv("data_final_raw.csv")


all$priceNumerical <- as.numeric( gsub("[S$]", "", gsub("[,]", "", all$Price)))

all$priceNumerical[is.na(all$priceNumerical)] <- 0

all$subcatLong <-  paste(all$Category , all$Subcategory)
palette_names <- rownames(brewer.pal.info[,"category"=="qual"])
palette_names <- rownames(brewer.pal.info)


gplay_summary <- summarise(group_by(all, Category, subcatLong),
                           meanPrice = mean(priceNumerical),
                           medianPrice = median(priceNumerical)
)

a1a <- function(summary_) {
  
  print("[a1a]")
  # Complete implementation...

  write_csv(summary_,"summary.csv")
  
  cats <- unique(summary_$Category)
  length_cats = length(cats)
  plot_gplay_mean <- ggplot2::ggplot(data = summary_, aes(x=Category, y=medianPrice, group=Category)) +geom_point(aes(shape=summary_$Category, color=summary_$subcatLong,fill=summary_$Category, size=3))
  
  

  colors = list()
  for (i in 1:length_cats){
    
    print("[iterating]")
    cat_ <- cats[i]

    subcats <- summary_[summary_$Category == cat_,]$subcatLong

    myColors <- colorRampPalette(brewer.pal(3, palette_names[i]))(length(subcats))
    names(myColors) <- subcats
    print("mycolors length")
  
    print(length(myColors))
    
    colors <- unlist(list(colors,myColors))
  }
  #colors <- unlist(colors, recursive = FALSE)
  print( "colors")
  print( length(colors))
  
  custom_colors <- scale_colour_manual( values = colors)
  
  
  plot_gplay_mean <- plot_gplay_mean + custom_colors
  
  print(plot_gplay_mean)
}

a1a(gplay_summary)
# write.csv(all, "data.csv", row.names = TRUE)


gplay_summary_without_outliers <- gplay_summary

gplay_summary_without_outliers <- filter(as.data.frame(gplay_summary_without_outliers),gplay_summary_without_outliers$medianPrice < 2500)
a1a(gplay_summary_without_outliers)


