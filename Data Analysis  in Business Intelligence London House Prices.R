#First import the dataset in the Environment Window

#1 Must run this code to attach the data
attach(London_house_prices)

#2 See data frame
str(London_house_prices)

#3 Show the attached data
print(London_house_prices)

#4 Data Summary
summary(London_house_prices)

#Frequency Analysis
#Bedrooms
#Calculate the frequency
freq_Bedrooms <- table(London_house_prices$Bedrooms)
print(freq_Bedrooms)

#Create the pie chart
pie(freq_Bedrooms,labels = names(freq_Bedrooms),col = rainbow(length(freq_Bedrooms)))

#Add a legend
legend('topright',legend = names(freq_Bedrooms), fill = rainbow(length(freq_Bedrooms)))

#Add a title
title('freq_Bedrooms')

#Bathrooms
#Calculate the frequency
freq_Bathrooms <- table(London_house_prices$Bathrooms)
print(freq_Bathrooms)

#Create the pie chart
pie(freq_Bathrooms, labels = names(freq_Bathrooms), col = rainbow(length(freq_Bathrooms)))

#Add a legend
legend('topright', legend = names(freq_Bathrooms), fill = rainbow(length(freq_Bathrooms)))

#Add a title
title('freq_Bathrooms')

#Has.Garden
#Calculate the frequency
freq_Has.Garden <- table(London_house_prices$Has.Garden)
print(freq_Has.Garden)

#Create the pie chart
pie(freq_Has.Garden,labels = names(freq_Has.Garden), col=rainbow(length(freq_Has.Garden)))

#Add a legend
legend('topright', legend = names(freq_Has.Garden), fill = rainbow(length(freq_Has.Garden)))

#Add a title
title('freq_Has.Garden')

#Has.Garage
#Calculate the frequency
freq_Has.Garage <- table(London_house_prices$Has.Garage)
print(freq_Has.Garage)

#Create the pie chart
pie(freq_Has.Garage,labels = names(freq_Has.Garage), col = rainbow(length(freq_Has.Garage)))

#Add a legend
legend('topright', legend = names(freq_Has.Garage), fill = rainbow(length(freq_Has.Garage)))

#Add a title
title('freq_Has.Garage')

#Has.Pool
#Calculate the frequency
freq_Has.Pool <- table(London_house_prices$Has.Pool)
print(freq_Has.Pool)

#Create the pie chart
pie(freq_Has.Pool, labels = names(freq_Has.Pool), col = rainbow(length(freq_Has.Pool)))

#Add a legend
legend('topright', legend = names(freq_Has.Pool), fill = rainbow(length(freq_Has.Pool)))

#Add a title 
title('freq_Has.Pool')

#Has.Gym
#Calculate the frequency
freq_Has.Gym <- table(London_house_prices$Has.Gym)
print(freq_Has.Gym)

#Create the pie chart
pie(freq_Has.Gym, labels = names(freq_Has.Gym), col = rainbow(length(freq_Has.Gym)))

#Add a legend
legend('topright', legend = names(freq_Has.Gym), fill = rainbow(length(freq_Has.Gym)))

#Add a title
title('freq_Has.Gym')

#Has.Elevator
#Calculate the frequency
freq_Has.Elevator <- table(London_house_prices$Has.Elevator)
print(freq_Has.Elevator)

#Create the pie chart
pie(freq_Has.Elevator, labels = names(freq_Has.Elevator), col = rainbow(length(freq_Has.Elevator)))

#Add a legend
legend('topright', legend = names(freq_Has.Elevator), fill = rainbow(length(freq_Has.Elevator)))

#Add a title
title('freq_Has.Elevator')

#Has.Fireplace
#Calculate the frequency
freq_Has.Fireplace <- table(London_house_prices$Has.Fireplace)
print(freq_Has.Fireplace)

#Create the pie chart
pie(freq_Has.Fireplace, labels = names(freq_Has.Fireplace), col = rainbow(length(freq_Has.Fireplace)))

#Add a legend
legend('topright', legend = names(freq_Has.Fireplace), fill = rainbow(length(freq_Has.Fireplace)))

#Add title
title('freq_Has.Fireplace')

#Is.Waterfront
#Calculate the frequency
freq_Is.Waterfront <- table(London_house_prices$Is.Waterfront)
print(freq_Is.Waterfront)

#Create the pie chart
pie(freq_Is.Waterfront, labels = names(freq_Is.Waterfront), col = rainbow(length(freq_Is.Waterfront)))

#Add a legend
legend('topright', legend = names(freq_Is.Waterfront), fill = rainbow(length(freq_Is.Waterfront)))

#Add a title
title('freq_Is.Waterfront')

#Is.Renovated
#Calculate the frequency
freq_Is.Renovated <- table(London_house_prices$Is.Renovated)
print(freq_Is.Renovated)

#Create the pie chart
pie(freq_Is.Renovated, labels = names(freq_Is.Renovated),col = rainbow(length(freq_Is.Renovated)))

#Add a legend
legend('topright', legend = names(freq_Is.Renovated), fill = rainbow(length(freq_Is.Renovated)))

#Add a title
title('freq_Is.Renovated')

#Has.View
#Calculate the frequency
freq_Has.View <- table(London_house_prices$Has.View)
print(freq_Has.View)

#Create the pie chart
pie(freq_Has.View, labels = names(freq_Has.View), col =  rainbow(length(freq_Has.View)))

#Add a legend
legend('topright', legend = names(freq_Has.View), fill = rainbow(length(freq_Has.View)))

#Add a title
title('freq_Has.View')

#Create a histogram
#Square.Footage
library(ggplot2)
ggplot(London_house_prices, aes(x=Square.Footage))+
  geom_histogram(binwidth = 75, fill = 'blue', color = 'black')+
  labs(title="Square.Footage", x = 'Square Footage', y ='Frequency')

#Price
library(ggplot2)
ggplot(London_house_prices, aes (x=Price))+
  geom_histogram(binwidth = 100, fill = 'black', color = 'blue')+
  labs(title='Price', x='Price', y= 'Frequency')  

#Hypothesis 1 - Multiple Linear Regression Model
#1 Perform multiple regression
model <- lm(Price ~ Has.Garden + Has.Garage + Has.Pool + Has.Gym + Has.Elevator + Has.Fireplace + Is.Waterfront + 
Has.Central.Air + Is.Renovated + Has.View,
    data=London_house_prices)

#2 Get the summary of the regression model
summary(model)

#Hypothesis 2 – Anova

#1 Perform the one-way ANOVA
result <- aov(London_house_prices$Price~London_house_prices$Bedrooms,data = London_house_prices)

#2 Run this code for seeing the anova output:
anova <- anova(result)

#3 Run this code to see anova table:
anova

#4 Print the descriptive statistics table
descriptive_stats <- aggregate(Price ~ Bedrooms,data = London_house_prices,
                               FUN = function(x) c(Mean = mean(x), SD = sd(x), N=length(x)))

#5 Show descriptive statistics of ANOVA
print(descriptive_stats)

# Meansplot
# Combine the data into a data frame
data <- data.frame(Bedrooms=rep(c('1','2','3','4'),each=5),
                   Price=c(Price))

# Load the ggplot2 library
library(ggplot2)

# Create the Annova means plot
means_plot <- ggplot(data, aes(x=Bedrooms, y=Price, fill = Bedrooms)) +
  geom_boxplot() +
  geom_hline(yintercept = mean(London_house_prices$Price), linetype = 'dashed', color = 'white', size = 0.1) +
  labs(x='Bedrooms', y='price', title='ANOVA Means Plot')+
  theme_minimal()

# Display the plot
print(means_plot)

#Hypothesis 3 - Correlation
# Calculate the correlation coefficient and p-value
correlation <- cor(London_house_prices$Square.Footage,London_house_prices$Price)
p_value <- cor.test(London_house_prices$Square.Footage,London_house_prices$Price)$p.value

# Print the correlation coefficient and p-value
cat('Correlation coefficient:', correlation, '\n')

# Visualize the relationship between Square Footage and Price
ggplot(London_house_prices, aes(x=Square.Footage, y=Price))+
  geom_point()+
  geom_smooth(method='lm', se = FALSE) +
  labs(x = 'Square Footage', y='price')+
  theme_minimal()
