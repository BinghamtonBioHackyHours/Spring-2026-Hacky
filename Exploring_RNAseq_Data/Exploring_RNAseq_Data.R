
# Exploring RNAseq Data ---------------------------------------------------

#Today's work has been modified from the following linK: https://tavareshugo.github.io/data-carpentry-rnaseq/

#Setting the working directory


#load in the necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(tidyverse)

#We are going to pretend we are yeast biologists today!
#we've got data with two types of yeast strains: wildtype (WT) and a mutant (mut)
#each strain has 6 timepoints of osmotic stress
#three replicates for each strain at each timepoint

##Getting comfortable with our data -------
#sample_info.csv: information about each sample
#counts_raw.csv: "raw" read counts for all of the genes (these will give us measures of gene expression)
#counts_transformed.csv: normalized read counts for each gene. This is on a log scale and is transformed to correct for dependency between the mean and the variance (this is normal for count data)

##Importing the data ---------

#import the data and store as the following objects: raw_cts, trans_cts, and sample_info,


#look at the structure of the data. How many samples do we have? Are there the same number of replicates for the data?


#how many genes do we have expression data for


# Exploratory Analysis of Count Data --------------------------------------

#Let's try to understand how our expression data is distributed for each sample. To do this, we'll use frequency polygons (similar to histograms) for our expression data in raw_cts

#To do this, we need to convert our trans_cts to "long format" with columns "gene", "sample" and counts. We will then need to join this table with sample_info

#let's do this using pivot_wider() and save it as an object named trans_cts_long
# "gather" the counts data
trans_cts_long <- trans_cts %>% 
  pivot_longer(cols = wt_0_r1:mut_180_r3, #the columns we want to gather
               names_to = "sample", #name for the new column that contains the old column names
               values_to = "cts") #name for the new column that contains our expression counts

#let's see what this looks like
trans_cts_long

#What if we want to reverse this? We can use pivot_wider()!
trans_cts_long %>% 
  pivot_wider(names_from = "sample", values_from = "cts")

##CHALLENGE: Convert raw_cts into long format 


##Now we want to add information about our samples to our long format data.
#let's take a look at our trans_cts_long data and our sample_info. Do they have anything in common?


#let’s use full_join(), to ensure we retain all data from both tables:
trans_cts_long <- full_join(trans_cts_long, sample_info, by = "sample")

##CHALLENGE: join sample info and raw_cts_long


#Now let's visualize  
trans_cts_long %>%
  ggplot(aes(cts, colour = replicate)) + 
  geom_freqpoly(binwidth = 1) + 
  facet_grid(strain ~ minute)


##CHALLENGE: Make the same plot with raw_cts_long

#CHALLENGE: try visualizing the data using a boxplot


# Scatterplots ------------------------------------------------------------

#Now let's visualize correlations between variables

#Let's start by looking at correlations between two replicates of WT at 0min
trans_cts %>% 
  ggplot(aes(wt_0_r1, wt_0_r2)) + geom_point() +
  geom_abline(colour = "brown")

#CHALLENGE: Compare expression between a WT cell at T0 and T30


#We have tons of samples, and it would be great to have an idea of correlations across all samples. The corrr package will help us with that!
install.packages("corrr")
library(corrr)

#Let's start by calculating gene expression correlations between every pair of samples. We can use the cor function to do this

# Calculate all correlations 
trans_cts_corr <- trans_cts %>% 
  # remove the column "gene" (we don't need correlations for this!)
  select(-gene) %>% 
  # Spearman's correlation for our calculations
  cor(method = "spearman")

# Visualize the correlations between the first 5 samples


#now let's plot it! The corrr package has a function "rplot" that makes this easy
rplot(trans_cts_corr) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

