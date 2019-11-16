# Surveys, Tests, and Sampling Bias on Amazon Mechanical Turk

### Overview
Following along with the [MS&E 231 class website](https://5harad.com/mse231/#hw4), we'll be conducting a survey on Amazon Mechanical Turk and correcting sampling biases before analyzing the survey results. Our survey test knowledge of and opinions towards the 311 non-emergency system.

### Getting Set Up
1. Clone the GitHub repo into a directory of your choosing. You can name the directory whatever you'd like.
```
mkdir <your new folder>
cd <your new folder>
git init
git remote add origin git@github.com:miguelito34/mse231_a4.git
git pull origin master
```

The first time you go to push a file, you may receive this note:
```
fatal: The current branch master has no upstream branch.
To push the current branch and set the remote as upstream, use

    git push --set-upstream origin master
```

If you see that, push using the instructions as above:
```
git push --set-upstream origin master
```

From now on, anytime you need to make changes, you should be able to push using:
```
git push
```

### Survey
The survey that was distributed on Amazon Mechanical Turk can be found below:

1. Prior to taking this survey, how familar were you with the 311 non-emergency system?
* Not at all
* I have heard about it before but I am unfamiliar with how the system works 
* I have a strong knowledge of what the system is and how it works

__What is 311?__

311 is a non-emergency phone number that people can call in many cities to find information about services, make complaints, or report problems like graffiti, excessive noise, or road damage. Once a report is made it is typically sent to the relevant city entity, such as the police department, sanitation department, or transportation department, who address it or fix it within a reasonable time period depending on the issue. The service is free to use and meant to give residents a way of interfacing with their local government. With that in mind:

2. On a scale of 1-5, how likely are you to use the 311 system to make a noise complaint? Assume 1 is "not at all" and 5 is "always".
* slider from 1-5

3. On a scale of 1-5, how likely are you to use the 311 system to report graffiti? Assume 1 is "not at all" and 5 is "always".
* slider from 1-5

4. On a scale of 1-5, how likely are you to use the 311 system to report damage to a roadway such as a pothole? Assume 1 is "not at all" and 5 is "always".
* slider from 1-5

5. On a scale of 1-5, no matter your opinion on 311, move the slider below to four. Assume 1 is "not at all" and 5 is "always".
* slider from 1-5

__Some Info About You__

The following questions are meant to give us a little more information about those taking our survey. What is your:

6. Sex
* Male
* Female

7. Age
* 18-24
* 25-29
* 30-39
* 40-44
* 45-49
* 50-59
* 60-64
* 65+

8. Race
* White
* Black
* Hispanic
* Other

9. Education
* No high school diploma
* high school graduate
* College degree
* Postgraduate degree

### Data
Data was contained by distributing the above survey to 200 users of the Amazon Mechanical Turk platform.

### Replication
To replicate the analysis and statistical adjustments, run the following command:
```
Rscript -e "rmarkdown::render('survey_analysis.Rmd')"
```