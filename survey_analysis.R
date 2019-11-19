################################################################################
## Author: Michael Spencer, Andrew (Foster) Docherty, Jorge Nam Song
## Project: MS&E 231 A4
## Script Purpose: Clean and analyze the 311 survey results on both adjusted and
##                 unadjusted data
## Notes: There are a lot of plots...
################################################################################

################################### Setup ######################################

# Libraries
if (!require(tidyverse)) install.packages("tidyverse")
library(tidyverse)

if (!require(nnet)) install.packages("nnet")
library(nnet)

# Parameters
path_data <- "Batch_3835086_batch_results.csv"

path_pop_dist <- "https://5harad.com/mse231/assets/pop_dist.tsv"

# Load Data
data_raw <-
	path_data %>%
	read_csv()

data_pop_distr <-
	path_pop_dist %>% 
	read_tsv()

# Prep data by replacing column names
old_names <- data_raw %>%
	select(starts_with("Answer.")) %>%
	colnames()

new_names <-
	str_replace_all(
		old_names,
		pattern = "Answer.|.TRUE",
		replacement = ""
	)

# Prep and clean data by aggregating columns and determing a single respondent's results
data <-
	# Gathers all response columns and determines what the given respondent answered 
	# by assuming only one answer column from a given question could be `TRUE`
	data_raw %>%
	select(
		id = WorkerId,
		start = AcceptTime,
		submit = SubmitTime,
		total_time = WorkTimeInSeconds,
		starts_with("Answer.")
	) %>%
	rename_at(vars(old_names), ~new_names) %>%
	group_by(id) %>%
	gather(
		key = "variable",
		value = "var_log",
		starts_with("age_"),
		starts_with("education_"),
		starts_with("familiarity_"),
		starts_with("race_"),
		starts_with("sex_")
	) %>%
	arrange(id) %>%
	filter(var_log) %>%
	select(-var_log) %>%
	separate(variable, into = c("demographic", "response"), sep = "_") %>%
	spread(key = demographic, value = response) %>%
	ungroup() %>%
	# Renames and recodes certain variables to comply with the census data pulled
	mutate(
		education =
			recode(
				education,
				"no" = "no high school diploma",
				"hs" = "high school graduate",
				"coll" = "college degree",
				"pg" = "postgraduate degree"
			),
		race =
			recode(
				race,
				"his" = "hispanic"
			),
		education =
			factor(
				education,
				levels = c(
					"no high school diploma",
					"high school graduate",
					"college degree",
					"postgraduate degree"
				)
			),
		familiarity = 
			recode(
				familiarity,
				"none" = 0,
				"some" = 2,
				"very" = 4
			)
	)

########################### Demographics Distribution ##########################

# Sex
data %>%
	filter(attention == 4, !is.na(sex)) %>%
	count(sex) %>%
	mutate(
		sex_prop = n / sum(n)
	) %>%
	ggplot(aes(x = sex, y = sex_prop)) +
	geom_col() +
	scale_y_continuous(
		labels = scales::percent_format(accuracy = 1)
	) +
	theme_minimal() +
	labs(
		title = "Distribution of Respondents' Sex",
		y = "Proportion of Survey",
		x = "Sex",
		caption = "\nResponses only include those that passed our attention test."
	)

# Race
data %>%
	filter(attention == 4, !is.na(race)) %>%
	count(race) %>%
	mutate(
		race_prop = n / sum(n),
		race = str_to_title(race) %>% fct_reorder(race_prop)
	) %>%
	ggplot(aes(x = race, y = race_prop)) +
	geom_col() +
	scale_y_continuous(
		labels = scales::percent_format(accuracy = 1)
	) +
	theme_minimal() +
	labs(
		title = "Distribution of Respondents' Race",
		y = "Proportion of Survey",
		x = "Race/Ethnicity",
		caption = "\nResponses only include those that passed our attention test."
	)

# Education
data %>%
	filter(attention == 4, !is.na(education)) %>%
	count(education) %>%
	mutate(
		ed_prop = n / sum(n)
	) %>%
	ggplot(aes(x = education, y = ed_prop)) +
	geom_col() +
	scale_y_continuous(
		labels = scales::percent_format(accuracy = 1)
	) +
	theme_minimal() +
	labs(
		title = "Distribution of Respondents' Education Level",
		y = "Proportion of Survey",
		x = "Education",
		caption = "\nResponses only include those that passed our attention test."
	)

# Age
data %>%
	filter(attention == 4, !is.na(age)) %>%
	count(age) %>%
	mutate(
		age_prop = n / sum(n)
	) %>%
	ggplot(aes(x = age, y = age_prop)) +
	geom_col() +
	scale_y_continuous(
		labels = scales::percent_format(accuracy = 1)
	) +
	theme_minimal() +
	labs(
		title = "Distribution of Respondents' Age",
		y = "Proportion of Survey",
		x = "Age",
		caption = "\nResponses only include those that passed our attention test."
	)

########################## Unadjusted Survey Analysis ##########################

                              ### Familiarity ###

# Overall average familiarity with 311
avg_familiarity <- 
	data %>% 
	filter(attention == 4) %>% 
	pull(familiarity) %>% 
	mean(na.rm = TRUE)

# Familiarity with 311 by sex
data %>% 
	filter(attention == 4, !is.na(familiarity), !is.na(sex)) %>% 
	group_by(sex) %>% 
	summarize(avg_fam = mean(familiarity)) %>% 
	ggplot(aes(x = sex, y = avg_fam)) +
	geom_hline(yintercept = avg_familiarity, linetype = 2) +
	geom_col() +
	coord_cartesian(ylim = c(0, 4)) +
	annotate(
		"text", 
		x = "female", 
		y = avg_familiarity, 
		label = "Average Familiarity\nfor All",
		vjust = 0
	) +
	theme_minimal() +
	labs(
		title = "Average 311 Familiarity by Sex",
		y = "Average Familiarity\n(from 0-4)",
		x = "Sex",
		caption = "\nResponses only include those that passed our attention test."
	)

# Familiarity with 311 by race
data %>% 
	filter(attention == 4, !is.na(familiarity), !is.na(race)) %>% 
	group_by(race) %>% 
	summarize(avg_fam = mean(familiarity)) %>% 
	mutate(race = str_to_title(race) %>%  fct_reorder(avg_fam)) %>% 
	ggplot(aes(x = race, y = avg_fam)) +
	geom_hline(yintercept = avg_familiarity, linetype = 2) +
	geom_col() +
	coord_cartesian(ylim = c(0, 4)) +
	annotate(
		"text", 
		x = "Black", 
		y = avg_familiarity, 
		label = "Average Familiarity\nfor All"
	) +
	theme_minimal() +
	labs(
		title = "Average 311 Familiarity by Race",
		y = "Average Familiarity\n(from 0-4)",
		x = "Race",
		caption = "\nResponses only include those that passed our attention test."
	)

# Familiarity with 311 by education
data %>% 
	filter(attention == 4, !is.na(familiarity), !is.na(education)) %>% 
	group_by(education) %>% 
	summarize(avg_fam = mean(familiarity)) %>% 
	ggplot(aes(x = education, y = avg_fam)) +
	geom_hline(yintercept = avg_familiarity, linetype = 2) +
	geom_col() +
	coord_cartesian(ylim = c(0, 4)) +
	annotate(
		"text", 
		x = "high school graduate", 
		y = avg_familiarity, 
		label = "Average Familiarity\nfor All", 
	) +
	theme_minimal() +
	labs(
		title = "Average 311 Familiarity by Education",
		y = "Average Familiarity\n(from 0-4)",
		x = "Education",
		caption = "\nResponses only include those that passed our attention test."
	)

# Familiarity with 311 by age
data %>% 
	filter(attention == 4, !is.na(familiarity), !is.na(age)) %>% 
	group_by(age) %>% 
	summarize(avg_fam = mean(familiarity)) %>% 
	ggplot(aes(x = age, y = avg_fam)) +
	geom_hline(yintercept = avg_familiarity, linetype = 2) +
	geom_col() +
	coord_cartesian(ylim = c(0, 4)) +
	annotate(
		"text", 
		x = "45-49", 
		y = avg_familiarity, 
		label = "Average\nFamiliarity for All",
		hjust = 0,
		vjust = 0
	) +
	theme_minimal() +
	labs(
		title = "Average 311 Familiarity by Age",
		y = "Average Familiarity\n(from 0-4)",
		x = "Age",
		caption = "\nResponses only include those that passed our attention test."
	)

                              ### Complaints ###

# Determines overall unadjusted willingness to use 311 (from 1-5)
total_pre_avg <-
	data %>% 
	summarize(
		`Act. Noise Complaint` = mean(noise_complaint, na.rm = TRUE),
		`Act. Graffiti Complaint` = mean(graffiti_complaint, na.rm = TRUE),
		`Act. Roadway Complaint` = mean(roadway_complaint, na.rm = TRUE),
		total_pre_avg = 
			mean(
				`Act. Noise Complaint`,
				`Act. Graffiti Complaint`,
				`Act. Roadway Complaint`
			)
	) %>% 
	pull(total_pre_avg)

# Willingness by sex
data %>% 
	group_by(sex) %>% 
	summarize(
		`Act. Noise Complaint` = mean(noise_complaint, na.rm = TRUE),
		`Act. Graffiti Complaint` = mean(graffiti_complaint, na.rm = TRUE),
		`Act. Roadway Complaint` = mean(roadway_complaint, na.rm = TRUE),
	) %>% 
	gather(
		key = "variable",
		value = "avg_response",
		starts_with("Act.")
	) %>% 
	ggplot(aes(x = sex, y = avg_response)) +
	geom_hline(aes(yintercept = total_pre_avg), linetype = 2) +
	geom_col() +
	coord_cartesian(ylim = c(0,5)) +
	facet_wrap(~variable, nrow = 1, ncol = 4) +
	theme_minimal() +
	labs(
		title = "Average Willingness to Make a Given Complaint via 311 by Sex",
		y = "Average Willingess to Make a Given Complaint\n(from 0-5)",
		x = "Sex",
		caption = "\nResponses are based on statistical adjustments."
	)

# Willingness by Race
data %>% 
	group_by(race) %>% 
	summarize(
		`Act. Noise Complaint` = mean(noise_complaint, na.rm = TRUE),
		`Act. Graffiti Complaint` = mean(graffiti_complaint, na.rm = TRUE),
		`Act. Roadway Complaint` = mean(roadway_complaint, na.rm = TRUE),
	) %>% 
	gather(
		key = "variable",
		value = "avg_response",
		starts_with("Act.")
	) %>% 
	ggplot(aes(x = fct_reorder(race, avg_response), y = avg_response)) +
	geom_hline(aes(yintercept = total_pre_avg), linetype = 2) +
	geom_col() +
	coord_cartesian(ylim = c(0,5)) +
	facet_wrap(~variable, nrow = 1, ncol = 4) +
	theme_minimal() +
	labs(
		title = "Average Willingness to Make a Given Complaint via 311 by Race",
		y = "Average Willingess to Make a Given Complaint\n(from 0-5)",
		x = "Race",
		caption = "\nResponses are based on statistical adjustments."
	)

# Willingness by Education
data %>% 
	filter(!is.na(education)) %>% 
	group_by(education) %>% 
	summarize(
		`Act. Noise Complaint` = mean(noise_complaint, na.rm = TRUE),
		`Act. Graffiti Complaint` = mean(graffiti_complaint, na.rm = TRUE),
		`Act. Roadway Complaint` = mean(roadway_complaint, na.rm = TRUE),
	) %>% 
	gather(
		key = "variable",
		value = "avg_response",
		starts_with("Act.")
	) %>% 
	ggplot(aes(x = education, y = avg_response)) +
	geom_hline(aes(yintercept = total_pre_avg), linetype = 2) +
	geom_col() +
	coord_cartesian(ylim = c(0,5)) +
	facet_wrap(~variable, nrow = 1, ncol = 4) +
	theme_minimal() +
	theme(
		axis.text.x.bottom = element_text(angle = 45, hjust = 1)
	) +
	labs(
		title = "Average Willingness to Make a Given Complaint via 311 by Education",
		y = "Average Willingess to Make a Given Complaint\n(from 0-5)",
		x = "Education",
		caption = "\nResponses are based on statistical adjustments."
	)

# Willingness by Age
data %>% 
	filter(!is.na(age)) %>% 
	group_by(age) %>% 
	summarize(
		`Act. Noise Complaint` = mean(noise_complaint, na.rm = TRUE),
		`Act. Graffiti Complaint` = mean(graffiti_complaint, na.rm = TRUE),
		`Act. Roadway Complaint` = mean(roadway_complaint, na.rm = TRUE),
	) %>% 
	gather(
		key = "variable",
		value = "avg_response",
		starts_with("Act.")
	) %>% 
	ggplot(aes(x = age, y = avg_response)) +
	geom_hline(aes(yintercept = total_pre_avg), linetype = 2) +
	geom_col() +
	coord_cartesian(ylim = c(0,5)) +
	facet_wrap(~variable, nrow = 1, ncol = 4) +
	theme_minimal() +
	theme(
		axis.text.x.bottom = element_text(angle = 45, hjust = 1)
	) +
	labs(
		title = "Average Willingness to Make a Given Complaint via 311 by Age",
		y = "Average Willingess to Make a Given Complaint\n(from 0-5)",
		x = "Age",
		caption = "\nResponses are based on statistical adjustments."
	)

############################ Population Adjustments ############################

# Train the models for each qubstantive questions
familiarity_model <- multinom(familiarity ~ sex + age + race + education, data = data)
noise_model <- multinom(noise_complaint ~ sex + age + race + education, data = data)
graffiti_model <- multinom(graffiti_complaint ~ sex + age + race + education, data = data)
roadway_model <- multinom(roadway_complaint ~ sex + age + race + education, data = data)

# Determine predicted responses based on models and true populations
pop_est_all <-
	data_pop_distr %>% 
	mutate(
		familiarity_preds = predict(familiarity_model, data_pop_distr) %>% as.integer(), 
		familiarity_preds = ifelse(familiarity_preds == 1, 0, familiarity_preds),
		noise_preds = predict(noise_model, data_pop_distr) %>% as.integer(),
		graffiti_preds = predict(graffiti_model, data_pop_distr) %>% as.integer(),
		roadway_preds = predict(roadway_model, data_pop_distr) %>% as.integer()
	)

pop_est_table <-
	pop_est_all %>% 
	summarize(
		`adj_fam` = weighted.mean(familiarity_preds, N),
		`adj_noise` = weighted.mean(noise_preds, N),
		`adj_graffiti` = weighted.mean(graffiti_preds, N),
		`adj_roadway` = weighted.mean(roadway_preds, N)
	)

# Calculate adjusted and unadjusted survey averages to compare in table below
adj_fam <- pop_est_table %>% pull(adj_fam)
adj_noise <- pop_est_table %>% pull(adj_noise)
adj_graffiti <- pop_est_table %>% pull(adj_graffiti)
adj_roadway <- pop_est_table %>% pull(adj_roadway)

avg_noise_complaint <- 
	data %>% 
	filter(attention == 4) %>% 
	pull(noise_complaint) %>% 
	mean(na.rm = TRUE)

avg_graffiti_complaint <- 
	data %>% 
	filter(attention == 4) %>% 
	pull(graffiti_complaint) %>% 
	mean(na.rm = TRUE)

avg_roadway_complaint <- 
	data %>% 
	filter(attention == 4) %>% 
	pull(roadway_complaint) %>% 
	mean(na.rm = TRUE)

# Comparison table of before and after adjustments
tibble(
	Metric = c("311 Familiarity", "Willingness to Make a Noise Complaint", "Willingness to Make a Graffiti Complaint", "Willingness to Make a Roadway Complaint"),
	`Survey Value` = c(avg_familiarity, avg_noise_complaint, avg_graffiti_complaint, avg_roadway_complaint),
	`Population Adj. Value` = c(adj_fam, adj_noise, adj_graffiti, adj_roadway),
	Scale = c("0-4", "1-5", "1-5", "1-5")
)

########################## Unadjusted Survey Analysis ##########################

                               ### Familiarity ###

# Adjusted familiarity with 311 by sex
pop_est_all %>% 
	group_by(sex) %>% 
	summarize(
		`Adj. Familiarity` = weighted.mean(familiarity_preds, N)
	) %>% 
	ggplot(aes(x = sex, y = `Adj. Familiarity`)) +
	geom_hline(aes(yintercept = c(2, 4)), linetype = 2) +
	geom_col() +
	annotate(
		"text", 
		x = "female", 
		y = c(2.2, 3.8),
		label = c("Somewhat Familiar", "Very Familiar"),
		hjust = 1
	) +
	coord_cartesian(ylim = c(0,4)) +
	theme_minimal() +
	labs(
		title = "Average 311 Familiarity by Sex",
		y = "Average Familiarity\n(from 0-4)",
		x = "Sex",
		caption = "\nResponses are based on statistical adjustments."
	)

# Adjusted familiarity with 311 by race
pop_est_all %>% 
	group_by(race) %>% 
	summarize(
		`Adj. Familiarity` = weighted.mean(familiarity_preds, N)
	) %>% 
	ggplot(aes(x = race, y = `Adj. Familiarity`)) +
	geom_hline(aes(yintercept = c(2, 2, 4, 4)), linetype = 2) +
	geom_col() +
	annotate(
		"text", 
		x = "black", 
		y = c(2.2, 3.8),
		label = c("Somewhat Familiar", "Very Familiar"),
		hjust = 0
	) +
	coord_cartesian(ylim = c(0,4)) +
	theme_minimal() +
	labs(
		title = "Average 311 Familiarity by Race",
		y = "Average Familiarity\n(from 0-4)",
		x = "Race",
		caption = "\nResponses are based on statistical adjustments."
	)

# Adjusted familiarity with 311 by by education
pop_est_all %>% 
	group_by(education) %>% 
	summarize(
		`Adj. Familiarity` = weighted.mean(familiarity_preds, N)
	) %>% 
	mutate(
		education =
			factor(
				education,
				levels = c(
					"no high school diploma",
					"high school graduate",
					"college degree",
					"postgraduate degree"
				)
			)
	) %>% 
	ggplot(aes(x = education, y = `Adj. Familiarity`)) +
	geom_hline(aes(yintercept = c(2, 2, 4, 4)), linetype = 2) +
	geom_col() +
	annotate(
		"text",
		x = "no high school diploma",
		y = c(2.2, 3.8),
		label = c("Somewhat Familiar", "Very Familiar"),
		hjust = 0
	) +
	coord_cartesian(ylim = c(0,4)) +
	theme_minimal() +
	labs(
		title = "Average 311 Familiarity by Education",
		y = "Average Familiarity\n(from 0-4)",
		x = "Education",
		caption = "\nResponses are based on statistical adjustments."
	)

# Adjusted familiarity with 311 by age
pop_est_all %>% 
	group_by(age) %>% 
	summarize(
		`Adj. Familiarity` = weighted.mean(familiarity_preds, N)
	) %>% 
	ggplot(aes(x = age, y = `Adj. Familiarity`)) +
	geom_hline(aes(yintercept = c(2, 2, 4, 4, 2, 2, 4, 4)), linetype = 2) +
	geom_col() +
	annotate(
		"text",
		x = "18-24",
		y = c(2.2, 3.8),
		label = c("Somewhat Familiar", "Very Familiar"),
		hjust = 0
	) +
	coord_cartesian(ylim = c(0,4)) +
	theme_minimal() +
	labs(
		title = "Average 311 Familiarity by Age",
		y = "Average Familiarity\n(from 0-4)",
		x = "Age",
		caption = "\nResponses are based on statistical adjustments."
	)

                               ### Complaints ###

# Adjusted overall average willingness to make a complaint using 311
total_avg <-
	pop_est_all %>% 
	summarize(
		`Adj. Noise Complaint` = weighted.mean(noise_preds, N),
		`Adj. Graffiti Complaint` = weighted.mean(graffiti_preds, N),
		`Adj. Roadway Complaint` = weighted.mean(roadway_preds, N),
		total_avg = 
			mean(
				`Adj. Noise Complaint`,
				`Adj. Graffiti Complaint`,
				`Adj. Roadway Complaint`
			)
	) %>% 
	pull(total_avg)

# Adjusted willingness by Sex
pop_est_all %>% 
	group_by(sex) %>% 
	summarize(
		`Adj. Noise Complaint` = weighted.mean(noise_preds, N),
		`Adj. Graffiti Complaint` = weighted.mean(graffiti_preds, N),
		`Adj. Roadway Complaint` = weighted.mean(roadway_preds, N)
	) %>% 
	gather(
		key = "variable",
		value = "avg_response",
		starts_with("Adj.")
	) %>% 
	ggplot(aes(x = sex, y = avg_response)) +
	geom_hline(aes(yintercept = total_avg), linetype = 2) +
	geom_col() +
	coord_cartesian(ylim = c(0,5)) +
	facet_wrap(~variable, nrow = 1, ncol = 4) +
	theme_minimal() +
	labs(
		title = "Average Willingness to Make a Given Complaint via 311 by Sex",
		y = "Average Willingess to Make a Given Complaint\n(from 0-5)",
		x = "Sex",
		caption = "\nResponses are based on statistical adjustments."
	)

# Adjusted willingness by Race
pop_est_all %>% 
	group_by(race) %>% 
	summarize(
		`Adj. Noise Complaint` = weighted.mean(noise_preds, N),
		`Adj. Graffiti Complaint` = weighted.mean(graffiti_preds, N),
		`Adj. Roadway Complaint` = weighted.mean(roadway_preds, N)
	) %>% 
	gather(
		key = "variable",
		value = "avg_response",
		starts_with("Adj.")
	) %>% 
	ggplot(aes(x = fct_reorder(race, avg_response), y = avg_response)) +
	geom_hline(aes(yintercept = total_avg), linetype = 2) +
	geom_col() +
	coord_cartesian(ylim = c(0,5)) +
	facet_wrap(~variable, nrow = 1, ncol = 4) +
	theme_minimal() +
	labs(
		title = "Average Willingness to Make a Given Complaint via 311 by Race",
		y = "Average Willingess to Make a Given Complaint\n(from 0-5)",
		x = "Race",
		caption = "\nResponses are based on statistical adjustments."
	)

# Adjusted willingness by Education
pop_est_all %>% 
	group_by(education) %>% 
	summarize(
		`Adj. Noise Complaint` = weighted.mean(noise_preds, N),
		`Adj. Graffiti Complaint` = weighted.mean(graffiti_preds, N),
		`Adj. Roadway Complaint` = weighted.mean(roadway_preds, N)
	) %>% 
	gather(
		key = "variable",
		value = "avg_response",
		starts_with("Adj.")
	) %>% 
	ggplot(aes(x = education, y = avg_response)) +
	geom_hline(aes(yintercept = total_avg), linetype = 2) +
	geom_col() +
	coord_cartesian(ylim = c(0,5)) +
	facet_wrap(~variable, nrow = 1, ncol = 4) +
	theme_minimal() +
	theme(
		axis.text.x.bottom = element_text(angle = 45, hjust = 1)
	) +
	labs(
		title = "Average Willingness to Make a Given Complaint via 311 by Education",
		y = "Average Willingess to Make a Given Complaint\n(from 0-5)",
		x = "Education",
		caption = "\nResponses are based on statistical adjustments."
	)

# Adjusted willingness by Age
pop_est_all %>% 
	group_by(age) %>% 
	summarize(
		`Adj. Noise Complaint` = weighted.mean(noise_preds, N),
		`Adj. Graffiti Complaint` = weighted.mean(graffiti_preds, N),
		`Adj. Roadway Complaint` = weighted.mean(roadway_preds, N)
	) %>% 
	gather(
		key = "variable",
		value = "avg_response",
		starts_with("Adj.")
	) %>% 
	ggplot(aes(x = age, y = avg_response)) +
	geom_hline(aes(yintercept = total_avg), linetype = 2) +
	geom_col() +
	coord_cartesian(ylim = c(0,5)) +
	facet_wrap(~variable, nrow = 1, ncol = 4) +
	theme_minimal() +
	theme(
		axis.text.x.bottom = element_text(angle = 45, hjust = 1)
	) +
	labs(
		title = "Average Willingness to Make a Given Complaint via 311 by Age",
		y = "Average Willingess to Make a Given Complaint\n(from 0-5)",
		x = "Age",
		caption = "\nResponses are based on statistical adjustments."
	)