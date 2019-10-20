# GRAPH 2B: Bar graph that shows three sample groups: varsity players who only only played in community college, varsity players who played in the NCAA, Maximum number of Varsity spots for their club program.
blanks_three_bars <- list("") # Spacing to show one school for every three bars
for (i in 1:length(school_labels)) {
  # school name goes in middle of the three bars
  blanks_three_bars[3 * i - 2] <- school_labels[i]
  blanks_three_bars[3 * i - 1] <- ""
  blanks_three_bars[3 * i] <- ""
}
# Makes a vector that shows each school's sample groups adjacent to each other
graph_2b_vec <- rep(0, length = 3 * length(school_labels))
# Loop that makes pie graph for each school's Varsity player demographics
for (i in 1:length(school_labels)) {
  # stores maximum number of Varsity spots per club program
  max_var_spots_vec[i] <- 2 * all_responses[i,1]
  # If there are less Varsity players than the max number of Varsity spots, the max
  # value for their school = the number of Varsity players
  if (all_responses[i,2] < max_var_spots_vec[i]) {
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
                   names.arg = blanks_three_bars,
                   cex.names = 0.9^(1/6 * length(school_labels)),
                   ylab = "Number of players",
                   ylim = c(0, max(all_responses[,2] + 2)),
                   xlab = "School Name", density = c(1000, 50, 20),
                   main = paste0("Number of Varsity Level Players Based on Collegiate Experience",
                                 "\n(Region = ", region, ", Gender = ", gender, ")"))
nums2b <- as.numeric(as.character(graph_2b_vec))
text(x = graph2b, y = nums2b, label = nums2b, pos = 3)
legend("topleft", legend = c("Community College Only", "Played in NCAA", 
                             "Max Number of Varsity Spots"), 
       density = c(1000, 50, 20),
       x.intersp = 0.5, y.intersp = 0.5)
# Makes and exports a .csv file that will make a data frame using the GRAPH 2B data
varsity_make_stats <- cbind(school_labels, nonvar_make_vector, var_make_vector, cc_only_vector, ncaa_vector, max_var_spots_vec)
colnames(varsity_make_stats) <- c("School Name", "Non-Varsity Player Make %", "Varsity Player Make %", 
  "Only Played in CC", "Played in NCAA", "Max. Number of Varsity Spots")
write.csv(varsity_make_stats, paste0("~/Documents/NCVF Varsity Project/Varsity Make Stats ", gender, ".csv"))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GRAPH 3A: Pie graph measuring the demand for an open division that allowed for
# an unlimited amount of Varsity players per team.
# First, the "Yes" and "No" answers for the open division are split into subsets.
yes_open_div <- subset(all_responses, all_responses[,10] == "Yes")
no_open_div <- subset(all_responses, all_responses[,10] == "No")
lbls3b <- c("Yes", "No") # Labels
nums3b <- c(length(yes_open_div[,10]), length(no_open_div[,10])) # Numeric values
# Percentage for how many schools answered "Yes"
rounded_yes_3b <- round(length(yes_open_div[,10]) / length(all_responses[,10]), 2) * 100
# "Yes" and "No" percentages stored in a single vector for the pie graph
perc3b <- c(rounded_yes_3b, 100 - rounded_yes_3b)
# Labels, numeric values, and percentages combined into a single label
info3b <- paste(paste0(lbls3b, ": ", nums3b), paste0(perc3b, "%"), sep = "\n")
if (length(yes_open_div[,10]) > 0 || length(no_open_div[,10]) > 0) {
  graph3a <- pie(nums3b, labels = info3b, col = c("green", "red"),
                 main = paste0("If your regional league created an Open division (where teams could have an unlimited
              number of Varsity players), would your club consider forming a team(s) for this division?",
                               "(Gender = ", gender, ")"))
} # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GRAPH 3B: Histograms displaying the distribution between the number of teams each school would send to represent their university in each gender's Open division.
# All schools are represented in both histograms (teams who replied "No" = 0 teams)
num_open_teams <- rep(0, length(all_responses[,10]))
for (i in 1:length(yes_open_div[,10])) {
  num_open_teams[i] <- as.numeric(yes_open_div[i,11])
}
color3b <- "BLUE"
curve3b <- "PINK"
if (gender == "Women's") { 
  color3b <- "PINK" 
  curve3b <- "BLUE"
}
# Yes for an Open Division Histogram
table_3b <- table(num_open_teams)
hist(num_open_teams, breaks = seq(-0.5, max(num_open_teams) + 0.5, by = 1),
     col = color3b, ylim = c(0, max(table_3b) + 1), xlab = "Number of Open Teams",
     main = paste0("Number of Teams Each School Would Represent in the Open Division",
                   "\n(Region = ", region, ", Gender = ", gender, ")"))
text(x = as.numeric(names(table_3b)), y = as.numeric(table_3b), label = as.numeric(table_3b), pos = 3)
curve(dnorm(x, mean = mean(num_open_teams), sd = sd(num_open_teams)), from = -0.5, to = max(num_open_teams) + 0.5, add = T, col = curve3b)
# GRAPH 3C: Pie graph displaying the reasons why a school would not represent teams in an Open level division.
if (length(no_open_div[,10]) > 0) {
  # Creates a table for each of the reasons and calculates their frequency
  no_open_table <- table(na.omit(all_responses[,12]))
  no_open_nums <- as.numeric(no_open_table)
  # Percentages calculated for each reason's frequency
  perc3c <- round(no_open_nums / sum(no_open_nums), 2) * 100
  # Pie chart that displays all the reasons with duplicates represented in numbers
  pie(no_open_nums, label = paste0(names(no_open_table), "\n", perc3c, "%"), col = rainbow(length(no_open_nums)),
      main = paste0("Reasons Why a School Would Not Represent Teams in an Open Division",
                    "\n(Region = ", region, ", Gender = ", gender, ")"))
}
