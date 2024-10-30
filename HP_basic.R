library(dplyr)
library(ggplot2)

df<-read.csv("HP2.csv")

View(df)
head(df)
str(df)

summary(df)

# Replace NA or empty values with 'Unknown'
df[df == ""] <- 'Unknown'
df[is.na(df)] <- 'Unknown'

df$Gender<-factor(df$Gender)
df$Job<-factor(df$Job)
df$House<-factor(df$House)
df$Species<-factor(df$Species)
df$Blood.status<-factor(df$Blood.status)

# Check the structure and summary of the cleaned data
print("Cleaned Data Summary:")
summary(df)

print("Structure of Cleaned Data:")
str(df)

names(df)

# Frequency counts of key categorical variables
table(df$Gender)
table(df$House)
table(df$Species)
table(df$Blood.status)

par(mar = c(13, 4, 4, 2) + 1)

# Plot 1: Gender distribution
barplot(table(df$Gender), 
        col = "lightblue", 
        main = "Gender Distribution of Harry Potter Characters", 
        xlab = "Gender", 
        ylab = "Count")

# Plot 2: House distribution
barplot(table(df$House), 
        col = "lightgreen", 
        main = "House Distribution of Harry Potter Characters", 
        ylab = "Count", las=2)

# Plot 3: Blood Status distribution
barplot(table(df$Blood.status), 
        col = "pink", 
        main = "Blood Status Distribution", 
        ylab = "Count", las=2)

# Plot 4: Species distribution
barplot(table(df$Species), 
        col = "lightcoral", 
        main = "Species Distribution", 
        ylab = "Count",
        las = 2)


# Cross-tabulation between Gender and House
table(df$Gender, df$House)
# Cross-tabulation between Gender and Blood.status
table(df$Gender, df$Blood.status)
table(df$Gender, df$Loyalty)


# Step 1: Create a frequency table of Gender vs House
gender_house_table <- table(df$Gender, df$House)

# Convert the table to a data frame
gender_house_df <- as.data.frame(gender_house_table)

# Rename columns for clarity
colnames(gender_house_df) <- c("Gender", "House", "Count")

# View the first few rows of the data frame
head(gender_house_df)

# Step 2: Create alternative plots

# Pie chart for Gender distribution by House
ggplot(gender_house_df, aes(x = "", y = Count, fill = House)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +
  labs(title = "Gender Distribution by House", fill = "House") +
  theme_void() +
  facet_wrap(~ Gender)

# Stacked bar chart for Gender vs House
ggplot(gender_house_df, aes(x = House, y = Count, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Gender vs House (Stacked)", 
       x = "House", 
       y = "Count") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 40, hjust = 1, size = 8))

