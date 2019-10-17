# GRAPH 2B: Bar graph that shows three sample groups: varsity players who only only played in community college, 
# varsity players who played in the NCAA, Maximum number of Varsity spots for their club program.

# School name is stored once in every three bars, so each set of three categories only has one school per set.
blanks_three_bars <- rep("", length = 3 * length(school_labels))
for (i in 1:length(school_labels))
  { blanks_three_bars[3 * i - 2] <- school_labels[i] }
# Makes a vector that shows each school's sample groups adjacent to each other
graph_2b_vec <- rep(0, length = 3 * length(school_labels))
# Loop that makes pie graph for each school's Varsity player demographics
for (i in 1:length(school_labels)) {
  # stores maximum number of Varsity spots per club program
  max_var_spots_vec[i] <- 2 * all_responses[i,1]
  # If there are less Varsity players than the max number of Varsity spots, the max
  # value for their school = the number of Varsity players
  if (all_responses[i,2+fem_data] < max_var_spots_vec[i]) {
    max_var_spots_vec[i] <- all_responses[i,2]
  }
  # stores each category for the school according to: CC only, NCAA, Max Var. spots
  graph_2b_vec[3 * i - 2] <- cc_only_vector[i]
  graph_2b_vec[3 * i - 1] <- ncaa_vector[i]
  graph_2b_vec[3 * i] <- max_var_spots_vec[i]
}
# Default color scheme if not school themed
color_scheme_2b <- c("#FDFF00", "#147DF7", "#FF0000")
if (graph2B_school_colors) {
  color_scheme_2b <- threeColors
}
graph2b <- barplot(graph_2b_vec, space = c(1, 0, 0), col = color_scheme_2b, 
                   ylab = "Number of players", ylim = c(0, max(all_responses[,2+fem_data] + 2)), 
                   names.arg = blanks_three_bars, xlab = "School Name", density = c(1000, 50, 20),
                   main = paste0("Number of Varsity Level Players Based on Collegiate Experience",
                                 "\n(Region = ", region, ", Gender = ", gender_name, ")"))
nums2b <- as.numeric(as.character(graph_2b_vec))
text(x = graph2b, y = nums2b, label = nums2b, pos = 3) # Numbers are shown above the bars
legend("topleft", legend = c("Community College Only", "Played in NCAA", "Max Number of Varsity Spots"), 
       density = c(1000, 50, 20), x.intersp = 0.5, y.intersp = 0.5)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GRAPH 3A: Pie graph measuring the demand for an open division that allowed for
# an unlimited amount of Varsity players per team.
# First, the "Yes" and "No" answers for the open division are split into subsets.
yes_open_div <- subset(all_responses, all_responses[,19] == "Yes")
no_open_div <- subset(all_responses, all_responses[,19] == "No")
lbls3b <- c("Yes", "No") # Labels
nums3b <- c(length(yes_open_div[,19]), length(no_open_div[,19])) # Numeric values
# Percentage for how many schools answered "Yes"
rounded_yes_3b <- round(length(yes_open_div[,19]) / length(all_responses[,19]), 2) * 100
# "Yes" and "No" percentages stored in a single vector for the pie graph
perc3b <- c(rounded_yes_3b, 100 - rounded_yes_3b)
# Labels, numeric values, and percentages combined into a single label
info3b <- paste(paste0(lbls3b, ": ", nums3b), paste0(perc3b, "%"), sep = "\n")
if (length(yes_open_div[,19]) > 0 || length(no_open_div[,19]) > 0)
{
  graph3a <- pie(nums3b, labels = info3b, col = c("green", "red"),
                 main = "If your regional league created an Open division (where teams could
              have an unlimited number of Varsity players), would your club
              consider forming a team(s) for this division?")
} # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GRAPH 3B: Histograms displaying the distribution between the number of teams
# each school would send to represent their university in each gender's Open division.
# All schools are represented in both histograms (teams who replied "No" = 0 teams)
num_open_men <- rep(0, length(all_responses[,19]))
num_open_women <- rep(0, length(all_responses[,19]))
for (i in 1:length(yes_open_div[,19])) {
  num_open_men[i] <- as.numeric(yes_open_div[i,20])
  num_open_women[i] <- as.numeric(yes_open_div[i,21])
}
# Men's Open Division Histogram
table_men3b <- table(num_open_men)
hist(num_open_men, breaks = seq(-0.5, max(num_open_men) + 0.5, by = 1), col = "BLUE",
     ylim = c(0, max(table_men3b) + 1),
     main = paste0("Number of Teams Each School Would Represent in the Open Division",
                   "\n(Region = ", region, ", Gender = Men's)"))
text(x = as.numeric(names(table_men3b)), y = as.numeric(table_men3b), 
     label = as.numeric(table_men3b), pos = 3)
curve(dnorm(x, mean = mean(num_open_men), sd = sd(num_open_men)), 
      from = -0.5, to = max(num_open_men) + 0.5, add = T, col = "PINK")
# Women's Open Division Histogram
table_women3b <- table(num_open_women)
hist(num_open_women, breaks = seq(-0.5, max(num_open_women) + 0.5, by = 1), col = "PINK",
     ylim = c(0, max(table_women3b) + 1),
     main = paste0("Number of Teams Each School Would Represent in the Open Division",
                   "\n(Region = ", region, ", Gender = Men's)"))
text(x = as.numeric(names(table_women3b)), y = as.numeric(table_women3b), 
     label = as.numeric(table_women3b), pos = 3)
curve(dnorm(x, mean = mean(num_open_women), sd = sd(num_open_women)), 
      from = -0.5, to = max(num_open_women) + 0.5, add = T, col = "BLUE")
# GRAPH 3C: Pie graph displaying the reasons why a school would not represent teams in an Open level division.
if (length(no_open_div[,19]) > 0)
{ # Creates a table for each of the reasons and calculates their frequency
  no_open_table <- table(na.omit(all_responses[,22]))
  no_open_nums <- as.numeric(no_open_table)
  # Percentages calculated for each reason's frequency
  perc3c <- round(no_open_nums / sum(no_open_nums), 2) * 100
  # Pie chart that displays all the reasons with duplicates represented in numbers
  pie(no_open_nums, label = paste0(names(no_open_table), "\n", perc3c, "%"), 
      col = rainbow(length(no_open_nums)),
      main = paste0("Reasons Why a School Would Not Represent Teams in an Open Division",
                    "\n(Region = ", region, ")"))
}