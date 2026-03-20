
# Exploring Transcriptome-Wide Changes with PCA ---------------------------------

#Today's work has been modified from the following linK: https://tavareshugo.github.io/data-carpentry-rnaseq/03_rnaseq_pca.html#Lesson_Objectives

#Setting the working directory
setwd("Desktop/Hacky Hours/Spring 2026/")

#load in the correct libraries 
library(tidyverse)
library(ggplot2)

#read in counts_transformed and sample_info from last week and store as trans_cts and sample_info respectively
trans_cts <- read.csv("counts_transformed.csv")
sample_info <- read.csv("sample_info.csv")

#We are going to pretend we are yeast biologists today!
#we've got data with two types of yeast strains: wildtype (WT) and a mutant (mut)
#each strain has 6 timepoints of osmotic stress
#three replicates for each strain at each timepoint

##Getting comfortable with our data -------
#sample_info.csv: information about each sample
#counts_transformed.csv: normalized read counts for each gene. This is on a log scale and is transformed to correct for dependency between the mean and the variance (this is normal for count data)

# PCA Basics --------------------------------------------------------------

#Last week, we played around with expression data for tons of genes. This can get super overwhelming, but is a great example of a multidimensional dataset. We've got lots of variables (our genes) and want to understand patterns of similarity between samples (yeast cells)

#Principal component analysis (PCA) is a great way to summarize multi-dimensional data. It allows us to visualize strong patterns in our dataset by linearly transforming the data onto a new coordinate system so the directions (principal components) capturing the largest variation are more easily identifiable.

#We can get three types of information from PCAs:
#1) Principal Component (PC) Scores: these are the coordinates of our sample on the PC axis
#2) Eigenvalues: Represent variance explained by our PCs. These can be used to calculate the proportion of variance in the original data that each axis explains.
#3) Eigenvectors: aka. "variable loadings" - these tell us the "weight" each variable has on a particular PC. Think of them as the correlation between the PC and the original variable

# Running a PCA with prcomp() ---------------------------------------------

#an intro to prcomp()
#prcomp(): takes matrix of data where columns are the variables we want to use to transform our samples. In this case, the samples should be the row of the matrix.

#Today, we are going to look for similarities across yeast cells based on gene expression. To do this, we'll need to generate a matrix where we'ved transposed trans_cts.

# Create a matrix from our table of counts
pca_matrix <- trans_cts %>% 
  # make the "gene" column become the rownames of the table
  column_to_rownames("gene") %>% 
  # coerce to a matrix
  as.matrix() %>% 
  # transpose the matrix so that rows = samples and columns = variables
  t()

# Perform the PCA
sample_pca <- prcomp(pca_matrix)

##Challenge: Take a look at the output. What is the class of the object you just created? How many principal components do we have?
class(sample_pca)
#That output is a little strange! It gives us "prcomp" because it returns a list with the following components:
str(sample_pca)

#stdev: these are the standard deviations of the principal components (aka the square roots of our eigenvalues)
#if we wanted to pull out our eigenvalues and store them in a vector, we could do the following:
pc_eigenvalues <- sample_pca$sdev^2

#rotation: contains the variable loadings for for each PC (these define our eigenvectors)
pc_loadings <- sample_pca$rotation #this is a matrix

#x: our PC scores
pc_scores <- sample_pca$x  #this is a matrix

#center: for this example, it contains the mean of each gene
sample_pca$center

#scale: will come back false because we didn't scale the data by the standard deviation

##finding the number of principal components
length(pc_eigenvalues) #answer should be 36
ncol(pc_scores)
ncol(pc_loadings)

##A NOTE FOR FUTURE PCAs: it's a good idea to standardize your variables before you run the PCA. Folks generally do this by centering the data on the mean and then scaling it by dividing by the standard deviation. This helps to ensure the PCA isn't influenced by genes that have higher general expression. prcomp() does the centering, not the scaling. Since we are using transformed data here, this isn't as much of an issue.

# Variance Explained by PCs -----------------------------------------------

#After you run a PCA, you want to investigate how many PCs you have and how much variance they explain.

#We can pull this information from our sample_pca object. In fact, we already did!
pc_eigenvalues
#The above vector holds the variance explained by each of our 36 PCs. 

#To plot this with ggplot, we'll need to have our data in a data frame or a tibble.
#They do a tibble, so let's go through the code below

#create a "tibble" manually with a variable indicating the PC number and a variable with the variances
pc_eigenvalues <- tibble(PC = factor(1:length(pc_eigenvalues)), #PC = factor(1:length(pc_eigenvalues)) gives us the number of PCs we have and stores it as PC
                         variance = pc_eigenvalues) %>% 
  # add a new column with the percent variance
  mutate(pct = variance/sum(variance)*100) %>% 
  # add another column with the cumulative variance explained
  mutate(pct_cum = cumsum(pct)) #cumsum() lets us find the cumulative sum of our percent variances we just calculated. We are adding each element of the sequence to the sum of the previous elements (a, a+b, a+b+c, ...)

# Let's take a look at this
pc_eigenvalues

#The above table can be used to generate a scree plot. 
#Scree plots show the fraction of the total variance explained by each PC

#let's make a figure to take a look at the variance explained by individual PCs as well as the cumulative variance. We are going to use a pareto chart: a figure that contains both bars and a line graph. Individual values are represented in descending order by the bars while the cumulative total is represented by the line.
pc_eigenvalues %>% 
  ggplot(aes(x = PC)) +
  geom_col(aes(y = pct)) +
  geom_line(aes(y = pct_cum, group = 1)) + 
  geom_point(aes(y = pct_cum)) +
  labs(x = "Principal component", y = "Fraction variance explained")
#As we move down the chart, PCs begin to explain less and less of our variance

# Visualizing Samples on PC Space -----------------------------------------

#Okay! Now it is time to look at our samples on our PC coordinates. Thankfully, we already have what we need!
# The PC scores are stored in the "x" value of the prcomp object
pc_scores <- sample_pca$x

#Let's make our PC plot!
pc_scores %>% 
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point()
#This is a simple example, but we can already see some clusters in our data!

#CHALLENGE: Make the above plot a little more informative! You'll want to color the figure by time (minute) and make the different strains (WT and mutant) different shapes. Hint: you'll need to join sample_info with sample_pca$x in order to make this happen!


pca_plot <- sample_pca$x %>% # extract the loadings from prcomp
  # convert to a tibble retaining the sample names as a new column
  as_tibble(rownames = "sample") %>% 
  # join with "sample_info" table by the "sample" column we just created
  full_join(sample_info, by = "sample") %>% 
  # create the plot
  ggplot(aes(x = PC1, y = PC2, color = factor(minute), shape = strain)) +
  geom_point()

# print the result (in this case a ggplot)
pca_plot

#What trends do we notice in the plot?
#samples seem to cluster by timepoint.
#T120 and T180 don't seem to be distinguishable with PC1 and PC2, but we may find other trends if we continue to explore the PC axis!
#Genotype doesn't seem to drive major changes in the transcriptome

##CHALLENGE: Pick another set of PCs and make a new plot. What trends do you see? Based on the scree plot, how much variance is explained by your chosen PCs?

pca_plot2 <- sample_pca$x %>% # extract the loadings from prcomp
  # convert to a tibble retaining the sample names as a new column
  as_tibble(rownames = "sample") %>% 
  # join with "sample_info" table by the "sample" column we just created
  full_join(sample_info, by = "sample") %>% 
  # create the plot
  ggplot(aes(x = PC2, y = PC3, color = factor(minute), shape = strain)) +
  geom_point()

# print the result (in this case a ggplot)
pca_plot2

# Exploring Correlations Between Genes and PCs ----------------------------

#Now we want to see which genes have the most influence on the PC axis. We can get this information from the variable loadings of the PCA within the "rotation" value of prcomp. We also already have this one!

pc_loadings <- sample_pca$rotation

#This is currently a matrix, so we need to convert it to a tibble/data frame for plotting/data manipulation
pc_loadings <- pc_loadings %>% 
  as_tibble(rownames = "gene")

# print the result
pc_loadings

#We have 6011 genes (wow). It will be too much to try and visualize them all at once, so let's focus on the top 10 genes with the highest loading on PC1 and PC2.

#Let's start by identifying the genes
top_genes <- pc_loadings %>% 
  # select only the PCs we are interested in
  select(gene, PC1, PC2) %>%
  # convert to a "long" format
  pivot_longer(matches("PC"), names_to = "PC", values_to = "loading") %>% 
  # for each PC
  group_by(PC) %>% 
  # arrange by descending order of loading
  arrange(desc(abs(loading))) %>% 
  # take the 10 top rows
  slice(1:10) %>% #this lets us subset rows using their positions
  # pull the gene column as a vector
  pull(gene) %>%  #this is similar to using "$" to look at a particular calumn
  # ensure only unique genes are retained
  unique()

top_genes

#Now that we have the gene names, let's subset the eigenvalues table to include just those
top_loadings <- pc_loadings %>% 
  filter(gene %in% top_genes)

#And we can use these values to visualize variable loadings (aka our eigenvectors - the correlations between the PC and the original variable)
loadings_plot <- ggplot(data = top_loadings) +
  geom_segment(aes(x = 0, y = 0, xend = PC1, yend = PC2), 
               arrow = arrow(length = unit(0.1, "in")),
               colour = "brown") +
  geom_text(aes(x = PC1, y = PC2, label = gene),
            nudge_y = 0.005, size = 3) +
  scale_x_continuous(expand = c(0.02, 0.02))

loadings_plot

##CHALLENGE: Select the top genes for the PCs you chose for the previous challenge, then make a new loadings_plot to visualize

#Let's start by identifying the genes
top_genes2 <- pc_loadings %>% 
  # select only the PCs we are interested in
  select(gene, PC2, PC3) %>%
  # convert to a "long" format
  pivot_longer(matches("PC"), names_to = "PC", values_to = "loading") %>% 
  # for each PC
  group_by(PC) %>% 
  # arrange by descending order of loading
  arrange(desc(abs(loading))) %>% 
  # take the 10 top rows
  slice(1:10) %>% #this lets us subset rows using their positions
  # pull the gene column as a vector
  pull(gene) %>%  #this is similar to using "$" to look at a particular calumn
  # ensure only unique genes are retained
  unique()

top_genes2

#Now that we have the gene names, let's subset the eigenvalues table to include just those
top_loadings2 <- pc_loadings %>% 
  filter(gene %in% top_genes2)

#And we can use these values to visualize variable loadings (aka our eigenvectors - the correlations between the PC and the original variable)
loadings_plot2 <- ggplot(data = top_loadings2) +
  geom_segment(aes(x = 0, y = 0, xend = PC2, yend = PC3), 
               arrow = arrow(length = unit(0.1, "in")),
               colour = "brown") +
  geom_text(aes(x = PC1, y = PC2, label = gene),
            nudge_y = 0.005, size = 3) +
  scale_x_continuous(expand = c(0.02, 0.02))

loadings_plot2

