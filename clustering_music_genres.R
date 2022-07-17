library(plotly)
library(dplyr)
library(scales)


# Rescaling with min-max normalization or use scales::rescale()
normalize <- function(x, na.rm = TRUE) {
  minimum = min(x)
  maximum = max(x)
  return((x - minimum) / (maximum - minimum))
}

# Load dataset
df <- read.csv('Spotify-2000.csv')

# Drop index column
df[, 1] <- NULL 

# Correct columns names
names(df) <- tolower(names(df)) 
df <- rename(df, bpm = beats.per.minute..bpm., loudness = loudness..db.,
             length = length..duration.)

# Select columns for comparision
columns <- c('bpm', 
             'loudness', 
             'liveness', 
             'valence', 
             'acousticness', 
             'speechiness')

# Apply normalization
df2 <- sapply(df[columns], rescale)

# Clustering
genre_cluster <- kmeans(df2, centers = 10, nstart = 20)
df <- cbind(df, cluster = genre_cluster$cluster)
df$cluster <- as.factor(df$cluster)

# Plot
fig <- plot_ly(df, x = ~bpm, y = ~energy, z = ~danceability, color = ~cluster, 
               colors = 'Paired')
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'Beats Per Minute'),
                                   yaxis = list(title = 'Energy'),
                                   zaxis = list(title = 'Danceability')))

# At end, enter 'fig' command to show this 3d-plot at Viewer tab on R-studio
