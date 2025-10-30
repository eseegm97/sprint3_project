# Simple MovieLens Data Analysis
# This script provides basic insights and statistics from the movie dataset

# Load required libraries
library(dplyr)
library(ggplot2)

# Set working directory and load data
setwd("c:/Users/eseeg/OneDrive/Documents/School_BYU-I/cse310/sprint3_project")

# Load datasets
print("Loading MovieLens dataset...")
movies <- read.csv("ml-latest-small (1)/ml-latest-small/movies.csv", stringsAsFactors = FALSE)
ratings <- read.csv("ml-latest-small (1)/ml-latest-small/ratings.csv")

print("Data loaded successfully!")
print(paste("Movies:", nrow(movies)))
print(paste("Ratings:", nrow(ratings)))
print(paste("Users:", length(unique(ratings$userId))))

# Function to display results nicely
display_movies <- function(movie_data, title) {
  cat("\n", paste(rep("=", 50), collapse=""), "\n")
  cat(title, "\n")
  cat(paste(rep("=", 50), collapse=""), "\n")
  
  for(i in 1:min(10, nrow(movie_data))) {
    cat(sprintf("%2d. %s\n", i, as.character(movie_data[i, "title"])))
    if("avg_rating" %in% colnames(movie_data)) {
      cat(sprintf("    Average Rating: %.2f (%d reviews)\n", 
                  as.numeric(movie_data[i, "avg_rating"]), 
                  as.integer(movie_data[i, "review_count"])))
    }
    if("review_count" %in% colnames(movie_data) && !"avg_rating" %in% colnames(movie_data)) {
      cat(sprintf("    Total Reviews: %d\n", as.integer(movie_data[i, "review_count"])))
    }
    cat(sprintf("    Genres: %s\n\n", as.character(movie_data[i, "genres"])))
  }
}

# Calculate movie statistics
print("\nCalculating movie statistics...")
movie_stats <- ratings %>%
  group_by(movieId) %>%
  summarise(
    avg_rating = mean(rating),
    review_count = n(),
    .groups = 'drop'
  ) %>%
  inner_join(movies, by = "movieId")

# 1. HIGHEST RATED MOVIES (with at least 50 reviews)
highest_rated <- movie_stats %>%
  filter(review_count >= 50) %>%
  arrange(desc(avg_rating)) %>%
  head(10)

display_movies(highest_rated, "TOP 10 HIGHEST RATED MOVIES (50+ reviews)")

# 2. MOST REVIEWED MOVIES
most_reviewed <- movie_stats %>%
  arrange(desc(review_count)) %>%
  head(10)

display_movies(most_reviewed, "TOP 10 MOST REVIEWED MOVIES")

# 3. LOWEST RATED MOVIES (with at least 20 reviews to be fair)
lowest_rated <- movie_stats %>%
  filter(review_count >= 20) %>%
  arrange(avg_rating) %>%
  head(10)

display_movies(lowest_rated, "TOP 10 LOWEST RATED MOVIES (20+ reviews)")

# 4. LEAST REVIEWED MOVIES (that have at least 1 review)
least_reviewed <- movie_stats %>%
  filter(review_count >= 1) %>%
  arrange(review_count) %>%
  head(10)

display_movies(least_reviewed, "TOP 10 LEAST REVIEWED MOVIES")

# 5. MOST POPULAR MOVIES BY DECADE
cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("MOST POPULAR MOVIES BY DECADE\n")
cat(paste(rep("=", 50), collapse=""), "\n")

# Extract year from movie titles
movies$year <- as.numeric(sub(".*\\((\\d{4})\\).*", "\\1", movies$title))
movies$decade <- floor(movies$year / 10) * 10

movie_stats_with_year <- movie_stats %>%
  left_join(movies[, c("movieId", "year", "decade")], by = "movieId") %>%
  filter(!is.na(decade))

decades <- sort(unique(movie_stats_with_year$decade))
for(decade in decades) {
  if(decade >= 1930) {  # Only show decades with decent data
    decade_movies <- movie_stats_with_year %>%
      filter(decade == !!decade, review_count >= 10) %>%
      arrange(desc(avg_rating)) %>%
      head(3)
    
    if(nrow(decade_movies) > 0) {
      cat(sprintf("\n%ds:\n", decade))
      for(i in 1:nrow(decade_movies)) {
        cat(sprintf("  %d. %s (%.2f rating, %d reviews)\n", 
                    i, as.character(decade_movies[i, "title"]), 
                    as.numeric(decade_movies[i, "avg_rating"]), 
                    as.integer(decade_movies[i, "review_count"])))
      }
    }
  }
}

# 6. GENRE ANALYSIS
cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("GENRE POPULARITY ANALYSIS\n")
cat(paste(rep("=", 50), collapse=""), "\n")

# Split genres and count
all_genres <- unlist(strsplit(movies$genres, "\\|"))
genre_counts <- table(all_genres)
genre_counts <- genre_counts[names(genre_counts) != "(no genres listed)"]
genre_counts <- sort(genre_counts, decreasing = TRUE)

cat("\nMost Common Genres:\n")
for(i in 1:min(10, length(genre_counts))) {
  cat(sprintf("%2d. %s: %d movies\n", i, names(genre_counts)[i], genre_counts[i]))
}

# Calculate average ratings by genre
genre_ratings <- data.frame()
for(genre in names(genre_counts)[1:10]) {
  genre_movies <- movies[grepl(genre, movies$genres), ]
  genre_movie_stats <- movie_stats[movie_stats$movieId %in% genre_movies$movieId, ]
  
  if(nrow(genre_movie_stats) > 0) {
    avg_rating <- weighted.mean(genre_movie_stats$avg_rating, genre_movie_stats$review_count)
    total_reviews <- sum(genre_movie_stats$review_count)
    
    genre_ratings <- rbind(genre_ratings, data.frame(
      genre = genre,
      avg_rating = avg_rating,
      total_reviews = total_reviews,
      movie_count = nrow(genre_movie_stats)
    ))
  }
}

cat("\nGenre Ratings (weighted by number of reviews):\n")
genre_ratings <- genre_ratings[order(genre_ratings$avg_rating, decreasing = TRUE), ]
for(i in 1:nrow(genre_ratings)) {
  cat(sprintf("%2d. %s: %.2f avg rating (%d movies, %d total reviews)\n", 
              i, genre_ratings[i, "genre"], genre_ratings[i, "avg_rating"],
              genre_ratings[i, "movie_count"], genre_ratings[i, "total_reviews"]))
}

# 7. USER ACTIVITY ANALYSIS
cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("USER ACTIVITY ANALYSIS\n")
cat(paste(rep("=", 50), collapse=""), "\n")

user_stats <- ratings %>%
  group_by(userId) %>%
  summarise(
    total_ratings = n(),
    avg_rating_given = mean(rating),
    .groups = 'drop'
  )

cat(sprintf("Most active user rated %d movies\n", max(user_stats$total_ratings)))
cat(sprintf("Least active user rated %d movies\n", min(user_stats$total_ratings)))
cat(sprintf("Average user rated %.1f movies\n", mean(user_stats$total_ratings)))
cat(sprintf("Most generous user averages %.2f stars\n", max(user_stats$avg_rating_given)))
cat(sprintf("Most critical user averages %.2f stars\n", min(user_stats$avg_rating_given)))

# 8. RATING DISTRIBUTION
cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("RATING DISTRIBUTION\n")
cat(paste(rep("=", 50), collapse=""), "\n")

rating_dist <- table(ratings$rating)
total_ratings <- sum(rating_dist)

cat("Rating breakdown:\n")
for(rating in names(rating_dist)) {
  percentage <- (rating_dist[rating] / total_ratings) * 100
  cat(sprintf("%.1f stars: %d ratings (%.1f%%)\n", 
              as.numeric(rating), rating_dist[rating], percentage))
}

# 9. MOVIE RELEASE YEAR ANALYSIS
cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("MOVIES BY RELEASE YEAR\n")
cat(paste(rep("=", 50), collapse=""), "\n")

year_stats <- movies %>%
  filter(!is.na(year), year >= 1930, year <= 2020) %>%
  count(year) %>%
  arrange(desc(n))

cat("Years with most movie releases:\n")
for(i in 1:min(10, nrow(year_stats))) {
  cat(sprintf("%2d. %d: %d movies\n", i, year_stats[i, "year"], year_stats[i, "n"]))
}

# 10. SUMMARY STATISTICS
cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("DATASET SUMMARY\n")
cat(paste(rep("=", 50), collapse=""), "\n")

cat(sprintf("Total movies in dataset: %d\n", nrow(movies)))
cat(sprintf("Total ratings in dataset: %d\n", nrow(ratings)))
cat(sprintf("Total users: %d\n", length(unique(ratings$userId))))
cat(sprintf("Average rating across all movies: %.2f\n", mean(ratings$rating)))
cat(sprintf("Movies with perfect 5.0 rating (10+ reviews): %d\n", 
            nrow(movie_stats[movie_stats$avg_rating == 5.0 & movie_stats$review_count >= 10, ])))
cat(sprintf("Movies with no ratings: %d\n", 
            nrow(movies) - length(unique(ratings$movieId))))

oldest_movie <- movies[which.min(movies$year), ]
newest_movie <- movies[which.max(movies$year), ]
cat(sprintf("Oldest movie: %s (%d)\n", oldest_movie$title, oldest_movie$year))
cat(sprintf("Newest movie: %s (%d)\n", newest_movie$title, newest_movie$year))

cat("\n", paste(rep("=", 50), collapse=""), "\n")
cat("ANALYSIS COMPLETE!\n")
cat(paste(rep("=", 50), collapse=""), "\n")