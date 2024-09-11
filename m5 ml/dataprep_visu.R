# Load necessary libraries
install.packages(c("ggplot2", "ggthemes", "dplyr"))
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)

# Read the data
df <- read.csv("regular_player.csv")
df1 <- read.csv("regular_team.csv")


# Insight data
summary(df)
summary(df1)

colnames(df)
colnames(df1)

df1$Win_Rate



# Clean the data
df <- df[, -c(3)]
df1 <- df1[, -c(1)]

#replace value na to 0
df[is.na(df)] <- 0
df1[is.na(df1)] <- 0


# Create a new column 'kda_ratio'
df$KDA_Ratio <- (df$Kills + df$Assists) / df$Deaths

# Replace instances of division by 0 with NA
df$KDA_Ratio[df$Deaths == 0] <- NA

df$KDA_Ratio
unique(df$KDA_Ratio)

# Create a new column 'win rate'
df1$Win_Rate <- df1$Games.Won / df1$Games.Played

# Define MVP criteria weights
weights <- c(KDA = 0.35, DPM = 0.20, GPM = 0.20, Games.Played = 0.10, KDA_Ratio = 0.15)

# Standardize data based on the index title
data_scaled <- scale(df[, c(6, 7, 8, 17, 18)])

# Combine weighted standardized scores
df$MVP_Score <- rowSums(data_scaled * weights)

# Identify top players based on MVP Score
top_players <- df %>%
    arrange(desc(MVP_Score)) %>%
    head(3) # Adjust the number of top players as needed

# View results
print(top_players)


# Visualization
# KDA distribution histogram
ggplot(df, aes(x = KDA)) +
    geom_histogram(binwidth = 1, fill = "blue", color = "black") +
    labs(title = "KDA Distribution", x = "KDA", y = "Frequency") +
    theme_minimal()

# Damage Dealt vs. Damage Received Scatter Plot
ggplot(df, aes(x = Hero.damage.dealt, y = Damage.Received)) +
    geom_point(color = "red") +
    labs(title = "Damage Dealt vs. Damage Received", x = "Damage Dealt", y = "Damage Received") +
    theme_minimal()

# GPM vs. DPM Scatter Plot
ggplot(df, aes(x = GPM, y = DPM)) +
    geom_point(color = "blue") +
    labs(title = "GPM vs. DPM", x = "GPM", y = "DPM") +
    theme_minimal()

# Team Win Rate Comparison Bar Chart
ggplot(df1, aes(x = Team, y = Win_Rate)) +
    geom_bar(stat = "summary", fill = "blue") +
    labs(title = "Team Win Rate Comparison", x = "Team", y = "Win Rate") +
    theme_minimal()
