---
  title: "ARS2017_P7_classification.Rmd"
author: "Oyedayo Oyelowo"
date: "24 April 2017"
output: html_document
---
  
  ---
  title: "Practical 4. Tree species classification using airborne laser scanning and imaging spectroscopy data part 2"
output:
  html_document: null
pdf_document: default
toc: yes
fig_caption: yes
---
  
  
  ### Load packages and set the version
  
  ```{r, eval = TRUE, message = FALSE, results = FALSE}
# Set version to student
ver = "/student_version" # "/student_version" for students

# Install packages
#install.packages("dplyr")
#install.packages("caret")
#install.packages("rgdal")
#install.packages("knitr")
#install.packages("kernlab")
#install.packages("randomForest")
#install.packages("spatialEco")
#install.packages("e1071")
# Require packages
require(dplyr)
require(caret)
require(rgdal)
require(rmarkdown)
require(knitr)
require(spatialEco)
require(kernlab)
```

If you have problems with the working directory paths, you can override the rmarkdown default settings with exknit package. Use this only if you run into problems.
```{r}
# library(ezknitr)
# ezknit(file = "C:/Users/ramip_000/Documents/git_projects/ARS_2017_new/final/ARS2017_P7_classification.Rmd",
#setwd("C:/Users/oyelo/Desktop/advanced remote sensing/exercise 7/final/student_version/samples")
#        )
```



## 3. Classification

I have extracted the hyperspectral data from raster files tree segments. Each segment has many pixels. We know that the reflectance on the sunny side of the tree is different than on the shadow side. The illumination conditions were also varying during the remote sensing campaign. Each tree might have different fraction of shadowed and bright pixels, which affects the classification result. Also, the co-registration between imaging spectroscopy data and laser scanning data was not perfect. The reflectance signal for a tree crown might actually come from nearby soil or other targets. We will reduce these effects with by masking the unwanted pixels from the tree crowns first.

You will use sample set and tree crown segmentation that has been prepared for you.

* eval = TRUE -> Code is run when knitr is run


### Read the samples first

```{r, eval = TRUE, message = TRUE}
samples = read.csv(paste(getwd(), ver, "/samples/final_samples.csv", sep = ""))
samples$X = NULL
```


### Masking the sample set

Generate masks to the pixel level data to remove shadows and soil

__Assignment: Fill the missing parts of the code__ 
* Create new vector mask and label the values as following:
  * Label all samples with reflectance at 836 nm less than 20% with "nir". Note that the
reflectance values have been scaled (1% is 100 and 10% is 1000). 
* Label all samples with ndvi < 0.5 with "ndvi"
* Label samples that fulfill both of the above conditions with "both"

```{r, eval = FALSE, message = TRUE}
mask = rep("none", nrow(samples))
mask[samples$R836 < 2000] = "nir" # Graves et al. 2016 for example on NDVI and NIR maskingg
mask[samples$ndvi <0.5 ] = "ndvi" 
mask[samples$R836 < 2000 & samples$ndvi < 0.5] = "both"
samples$mask = as.factor(mask)
```

Let's see what the data looks like. First we will select only Grevillea robusta trees and check how the masking worked

```{r, eval = FALSE, message = TRUE}
grev.all = samples[samples$species_na %in% c("Grevillea robusta"), ]
{plot(grev.all$R836, grev.all$ndvi, col = grev.all$mask)
points(mean(grev.all$R836), mean(grev.all$ndvi), col = "red")}
rm(grev.all)
```

Now we will use the mask to actually drop the masked samples

__Assignment: Modify the code so that it keeps only pixels that have mask value "none"__

```{r, eval = FALSE, message = TRUE}
samples.masked = samples[samples$mask == "none" , ]
samples.masked$mask = NULL
```

Aggregate data from pixel level to tree crown level

```{r, eval = FALSE, message = TRUE}
tmp = aggregate(samples.masked[, !names(samples.masked) %in% c("tree_id", "species_na")], by=list(tree_id = samples.masked$tree_id), FUN = mean)
species = samples.masked[!duplicated(samples.masked[, c("tree_id")]), c("tree_id", "species_na")]
samples.agg = merge(tmp, species, by = "tree_id"); rm(species, tmp)
samples.agg = samples.agg[complete.cases(samples.agg), ]
```


Now we can finally join the als features to the segments

```{r, eval = FALSE, message = TRUE}
itc = readOGR(paste(getwd(), ver, "/samples", sep = ""), "itc_alsmet") 
itc = itc[, !names(itc@data) %in% c("species_na", "f_height", "fname", "xmean", "ymean")] # "closest_d", 
samples.final = merge(samples.agg, itc@data, by = "tree_id"); rm(itc)
samples.final$tree_id = NULL  # Tree IDs are not needed anymore

# Move species_na as the first column
samples.final <- samples.final %>% dplyr::select(species_na, everything())

# You can write the aggregated samples to a csv file here if you wish
write.csv(samples.final, paste(getwd(), ver, "/samples/samples_agg.csv", sep = ""))
```

The number of samples per tree species varies a lot. There are many species that have less than four samples, which is not enough for any meaningful classification task. In this exercise we will remove these species before classification.

__Assignment: Fill the missing parts of the code, so that all species that have less than four samples are dropped__

```{r, eval = FALSE, message = TRUE}
# Check how many samples each class has
unique = as.data.frame(table(samples.final$species_na))

# Drop species with less than n observations
under.limit = unique[unique$Freq < 4 , ]
samples.final = droplevels(samples.final[!samples.final$species_na %in% under.limit$Var1,])

#print the frequencies (all species!)
unique = unique[with(unique, order(-Freq)), ]; unique
```



### Start the classification process

We will use [Caret package](https://topepo.github.io/caret/) for classification. Caret contains many helpful functions that make your life easier when you are doing supervised classification, regression modeling or other common machine learning tasks. This is remote sensing course, but from now on we will treat the classification task as any other machine learning problem. One of the problems with commercial remote sensing software is that it gears your thinking towards the common solutions used in remote sensing, instead of exploring the variety of techniques used in other disciplines. Caret package is used in addition to remote sensing scientists by economists, chemists, machine learning scientists and many other disciplines.


I will also introduce to a custom function called MergeClasses that checks the number of samples in each class, orders them by frequency and then combines classes. If you set the value to 1, it will only keep the class with most samples as an individual class and combine all other into same class. We will now set it to 13, to keep only classes with more than 10 samples. Smaller classes are combined to new class labeled as "other". You will also need it later, when you are building your own model.

```{r, eval = FALSE, results = TRUE}
mergeclasses <- function (s, combine.limit) {
# This function orders samples by the frequency of tree species 
# and combines all other classes to new class "other", except the 
# classes with most samples.
unique = as.data.frame(table(s$species_na))                 
unique = unique[with(unique, order(-Freq)), ]
keep = head(unique, combine.limit)  
keep = as.character(unlist(keep[, 1]))

s$species_na <- as.character(s$species_na)
s$species_na <- replace(s$species_na, !(s$species_na %in% keep), c("other"))
s$species_na <- as.factor(s$species_na)

droplevels(s)
}

# Take only species with more than n samples # Ref. Tree species abundance predictions in a tropical agricultural landscape ... Graves et al 2016.
s = mergeclasses(samples.final, combine.limit = 13)
summary(s$species_na)
```



fitControl is used to define cross-validation method. We will use repeated cross-validation. Even better would be to use leave-one-out (loocv) as we have so few samples that we will always want to maximise the size of the training set. However, it is much slower. We will use tuneGrid to define the different parameters with the classification. Random Forest (RF) has only few parameter options, which makes it easy to parameterize and use. We will set mtry to either 2 or square root of number of features (commonly used default value)

```{r, eval = FALSE, message = FALSE}
fitControl <- trainControl(
method = "repeatedcv"  # 10-fold CV
,number = 10
,repeats = 1
,savePredictions = "final"
)
tuneGrid <-  expand.grid(.mtry = c(2, round(sqrt(ncol(s)-1))))

fit <- train(species_na ~ ., data = s 
,method = "rf" # svmRadial rf
,trControl = fitControl
,tuneGrid=tuneGrid
)
```

Let's see what the results looked like

```{r, eval = FALSE, message = TRUE}
fit
```

Build confusion matrix

```{r, eval = FALSE, message = TRUE}
mod = as.data.frame(fit$pred)

cf.svm =confusionMatrix(data = mod$pred
                        , reference =  mod$obs
                        , mode = "prec_recall"
)
cf.svm
```

The results are not very nice at this stage. The overall accuracy is only around 50%. You're overall accuracy is not exactly the same (probably). It varies because we used RF classifier, which has "random" element. RF classifier builds multiple decision trees (usually 500-1000), each of which are classifications by themselves. Decision trees tend to overfit the classification result, and RF avoids this by takind random samples of the data while creating hundreds of decision trees, and labels each sample in the end with the mode of these separate classifications. Random samples are taken with bootstrap method with replacement. RS also selects randomly features during each split. Split, simply defined, splits the data into two sets, where the classes are most separable. Decision trees are built from these splits.

Read more about classication, Random Forests and generally about statistical learning (machine learning) from here [Statistical learning](http://www-bcf.usc.edu/~gareth/ISL/ISLR%20Sixth%20Printing.pdf)

Although this is a classification practical we do not dig deeply into how the classifiers work. There is excellent course on machine learning at the department of computer sciences called Introduction to Machine Learning that I strongly suggest for deeper understanding.


## Understanding the data

We were not happy with first classification results. Now we will study the characteristics of the data more deeply. Great classification accuracy is an achievement, surely, but more important is to understand the nature of the data and the context to the actual research question. For example, it might be important to identify one the tree species from all other species as well as possible, instead of trying to achieve good result with all classes. 

In our case, there were Eucalyptus saligna trees and other Eucalyptus trees (Eucalyptus sp.) that we identified only on genus level instead of species level. All Eucalyptuses serve similar role in the ecosystem of the Taita Hills (study area), as they are exotic (not native) species that are known to consume a lot of ground water, and are generally considered harmful for the ecosystems. For us, it does not matter if it is Eucalyptus saligna or Eucalyptus sp. Next, we will study if it makes sense to combine these classes.

We will study the spectral separability of the species using Jeffries-Matusita distance. Jeffries matusita [0 = identical, 2 = different]. This might also yield some interesting results on separability of the tree species that we can use in later classification attempts.

[link](http://stats.stackexchange.com/questions/106325/jeffries-matusita-distance-for-14-variables)

We use the pixel values to get higher sample size during the spectral separability studies. First, drop the species with less than four individual trees also from pixel based data

```{r, eval = FALSE, results=TRUE}
s <- droplevels(samples.masked[samples.masked$species_na %in% samples.final$species_na, ])
x = s[ , grepl( "R" , names(s) ) ]  # Select only the spectral data
df = cbind(species_na = s$species_na, x)
list.species = split(df, df$species_na)
```

Now, we will create new custom function that calculates the spectral separability for two classes for each band

```{r, eval = FALSE}
MultibandSeparability <- function(x, y, metric){
# This function returns chosen separability metric for two classes on n bands
res = list()
for (i in 1:ncol(x)){
x.sub = x[, i]
y.sub = y[, i]
sep = separability(x.sub, y.sub)
res[[length(res) + 1]] <- sep[, names(sep) %in% metric]
}

res.df = do.call(rbind.data.frame, res)
colnames(res.df) <- metric
res.df
}
# Try with first two species in the list
names.list = names(list.species)
x = list.species[[1]]; x$species_na = NULL
y = list.species[[2]]; y$species_na = NULL
test <- MultibandSeparability(x, y, metric = "JM")
test$wl <- colnames(x)
test = test[order(-test$JM), ]
names.list[1:2]
head(test) # The wavelengths where these two species are most separable
```

This shows the most important spectral wavelengths that separate  "Acacia mearnsii" and "Acacia Melliphera" tree species. 
Next, we calculate the separability between all possible combinations of species with all wavelengths.


```{r, eval = FALSE}
OneAgainstAllSeparability <- function(x, list.classes, round){
# compare one of the classes against all other classes
# with all of the bands
res <- list()
list.names = names(list.classes)
for(i in 1:length(list.classes)) {
#x = list[[1]]; x$classes = NULL
x$species_na = NULL
y = list.classes[[i]]; y$species_na = NULL
comb.name = paste(list.names[round], "-", list.names[i], sep = "")
multisep <- MultibandSeparability(x, y, metric = "JM")
colnames(multisep) <- comb.name

res[[length(res)+1]] <- multisep
}
final.df = do.call(cbind.data.frame, res)
final.df
}


SeparabilityAllCombinations <- function(list.species){
# This function iterates the separability for each species against all other species
res = list()
for(i in 1:length(list.species)){
print(i)
single.class = list.species[[i]]
multi.class.sep <- OneAgainstAllSeparability(single.class, list.species, i)
res[[length(res)+1]] <- multi.class.sep
}
res
}
res = SeparabilityAllCombinations(list.species)

# Select list of the separabilities of one species against all others
single = res[[1]]
boxplot(single, use.cols = TRUE, las = 2) # Plot ndvi for species
title(main = paste("Jeffries-Matusita distances (per wl)", names.list[1], "vs others"))
```

Check [this](http://rslab.disi.unitn.it/papers/R1-TGARS-JM.pdf) for multiclass JM distance for feature selection

Now we can calculate the mean separability on all bands 

```{r, eval = FALSE}
mean.sep <- lapply(res, function(x) colMeans(x))
#temp = mean.sep[[1]]
mean.sep.df = do.call(rbind.data.frame, mean.sep)
colnames(mean.sep.df) <- names.list
rownames(mean.sep.df) <- names.list

head(mean.sep.df)
```
You can now take a look at the mean.sep.df dataframe and see how well each of the species are separable
Let's see if the separability affects the classification. Eucalyptus saligna and Eucalyptus sp. had very low separability index, while Euphorbia kibwezensis was separated from both Eucalyptus classes well.

__Assignment:__ select only reflectance data for tree species: "Eucalyptus saligna", "Eucalyptus sp.", "Euphorbia kibwezensis"

__Assignment:__ select only reflectance data for tree species: "Eucalyptus saligna", "Eucalyptus sp.", "Euphorbia kibwezensis"

```{r, eval = FALSE}

sub = droplevels(samples.final[samples.final$species_na %in% c("Eucalyptus saligna", "Eucalyptus sp.", "Euphorbia kibwezensis"), ])
unique(sub$species_na)

# Fill this! Select only the spectral data
x = sub[, grepl( "R" , names(s) ) ]  
###

# Combine the tree species column with the reflectance data
sub = cbind(species_na = sub$species_na, x)
###
```


```{r, eval = FALSE}
ClassifyData <- function(dat){
  fitControl <- trainControl(
    method = "repeatedcv"  # 10-fold CV
    ,number = 10
    ,repeats = 1
    ,savePredictions = "final"
  )
  # SVM with radial basis function
  tuneGrid <-  expand.grid(C = c(1, 4, 8, 16), 
                           sigma = c(0.05, 0.1, 0.2, 0.5, 1))
  
  fit <- train(species_na ~ ., data = dat
               ,method = "svmRadial" # svmRadial rf
               ,trControl = fitControl
               ,tuneGrid=tuneGrid
  )
  mod = as.data.frame(fit$pred)
  
  cf.svm =confusionMatrix(data = mod$pred
                          , reference =  mod$obs
                          , mode = "prec_recall"
  )
  
  list(fit, cf.svm)
}

model = ClassifyData(sub)
model[1]  # training results
model[2]  # confusion matrix
```

The classification accuracy is now much higher, but we have only three classes. The Jeffries-Matusita separability index suggested that the two Eucalyptus classes are quite similar between each other, while the Euphorbia Kibwezensis was more separable. The classification results are in line with this observation as the Euphorbia Kibwezensis is classified with highest accuracy.

Now, let's combine these two classes and try again

```{r, eval = FALSE}
sub[sub$species_na %in% c("Eucalyptus saligna"), c("species_na")] <- "Eucalyptus sp."
unique(sub$species_na)
model = ClassifyData(droplevels(sub))
model[1]  # training results
model[2]  # confusion matrix

```
The overall accuracy is now over 90% and most of the field measurements labeled as Eucalyptus sp. were correctly classified (look at the confusion matrix). Caret gives precision and recall only for the "positive class" in binary setting. Precision is interpreted similarly as User's accuracy and recall as Producer's accuracy, which are more commonly used metrics in remote sensing community.

__Answer briefly to the following questions:__

* Why does combining species that are spectrally similar affect the classification results? In what kind of situations it makes sense to combine two species?

+ Answer here

* We did cloud masking and soil masking earlier. Why did we do this? How does it affect the classification accuracy?

+ Answer here

* We removed all tree species that have less than four samples. How can you justfify this decision? Can we simply rule them out as outliers, or are there any other ways how we could handle them.

+ Answer here




### Final task

As your final task you will make two classifications and compare their accuracies. First classification will use reflectance bands and second one reflectance data that has been transformed with minimum noise fraction (features starting with MNF). ALS features will be included in both classifications. MNF is used to reduce the dimensionality of the data, remove noise and to remove redundancy and band-to-band correlations. It is very effective tool that has many applications in remote sensing. It is closely related to principal component analysis, but has been shown to work even better for RS data. 

You will now write you're own code for the classification using the pieces of code used earlier. These are the requirements for your assignment:
  
  * All Eucalyptuses are combine to Eucalyptus sp.
* All Euphorbias are combined to Euphorbia sp.
* All species with less than four samples are removed completely
* Remaining species with 4-15 samples are combined to a class called "other"
* Trees are classified at the tree crown level
* First classification includes selected reflectance bands and ALS features
* Second classification includes selected MNF bands and ALS features
* Support vector machine classifier with radial basis function kernel is used as a classifier. The sigma and C parameters are tuned using cross validation to find the optimal parameters. You can use the same code that was used earlier with little modifications.
* Finally you will compare the classification accuracies of the two classifications. Study these
+ Interpret the changes in the confusion matrix
+ Interpret the changes in precision (Users accuracy) and recall (Producers accuracy) and overall accuracy
+ Use McNemars test to check if the change in the overall accuracy of these two classifications is statistically significant
* Divide you're code into meaningful sized chunks of R code and write a report of what you did in same fashion as the instruction have been written in this document

__Remember that this is the last exercise in the last remote sensing course that our department teaches, so it is meant to be challenging and might be time consuming. However, completing this exercise will help you a lot, if you are going to work with classifications or regression in your masters thesis. Also, machine learning is highly appreciated skill in the current job market. Not just in remote sensing, but also in many other fields.__

First clean your working environment and read the aggregated data from a file that is provided for you
```{r, eval = TRUE, message = FALSE}
rm(list=ls())
ver = "/student_version"
samples = read.csv(paste(getwd(), ver, "/samples/samples_agg.csv", sep = ""))
samples$X = NULL  # Unique IDs are not needed
```


You will need the following function to combine two specific classes. You need to complete the code for the Euphorbias.

```{r, eval = FALSE, message = FALSE}
renameSpecies <- function(data) {
# Use this function to rename values in species_na column
data[data$species_na %in% c("Eucalyptus saligna"), c("species_na")] <- "Eucalyptus sp."
# Repeat for Euphorbias
data[data$species_na %in% c("Euphorbia kibwezensis"), c("species_na")] <- "Euphorbia sp."

droplevels(data)
}
samples <- renameSpecies(samples)
```



Now you need to use the fragments of code I have shown you earlier and apply them to complete your own classification models.
```{r, eval = FALSE, message = TRUE}
# Check how many samples each class has
unique = as.data.frame(table(samples$species_na))

# Drop species with less than n observations
under.limit = unique[unique$Freq < 4 , ]
samples = droplevels(samples[!samples$species_na %in% under.limit$Var1,])

#print the frequencies (all species!)
unique = unique[with(unique, order(-Freq)), ]; unique
```

```{r, eval = FALSE, results = TRUE}
mergeclasses <- function (s, combine.limit) {
# This function orders samples by the frequency of tree species 
# and combines all other classes to new class "other", except the 
# classes with most samples.
unique = as.data.frame(table(s$species_na))                 
unique = unique[with(unique, order(-Freq)), ]
keep = head(unique, combine.limit)  
keep = as.character(unlist(keep[, 1]))

s$species_na <- as.character(s$species_na)
s$species_na <- replace(s$species_na, !(s$species_na %in% keep), c("other"))
s$species_na <- as.factor(s$species_na)

droplevels(s)
}

# Take only species with more than n samples # Ref. Tree species abundance predictions in a tropical agricultural landscape ... Graves et al 2016.
samples = mergeclasses(samples, combine.limit = 6)
summary(s$species_na)
```

```{r, eval = FALSE}
ClassifyData <- function(dat){
fitControl <- trainControl(
method = "repeatedcv"  # 10-fold CV
,number = 10
,repeats = 1
,savePredictions = "final"
)
# SVM with radial basis function
tuneGrid <-  expand.grid(C = c(1, 4, 8, 16), 
sigma = c(0.05, 0.1, 0.2, 0.5, 1))

fit <- train(species_na ~ ., data = dat
,method = "svmRadial" # svmRadial rf
,trControl = fitControl
,tuneGrid=tuneGrid
)
mod = as.data.frame(fit$pred)

cf.svm =confusionMatrix(data = mod$pred
, reference =  mod$obs
, mode = "prec_recall"
)

list(fit, cf.svm)
}

```




```{r, eval = FALSE}

refl=samples[,grepl("R", names(samples))]
als=samples[, 169: length(samples)]

###

#fill this! create new dataframe
refl.als.samples=cbind(species_na=samples$species_na, refl, als)

refl.als.model= ClassifyData(refl.als.samples)
refl.als.model[1]   #training results
refl.als.model[2]   #confusion matrix


```



```{r, eval = FALSE}

mnf=samples[,grepl("MNF", names(samples))]
als=samples[, 169: length(samples)]

###

#fill this! create new dataframe
mnf.als.samples=cbind(species_na=samples$species_na, mnf, als)

mnf.als.model= ClassifyData(mnf.als.samples)
mnf.als.model[1]   #training results
mnf.als.model[2]   #confusion matrix


```






```{r, eval = FALSE}
refl.als.conf=refl.als.model[[2]]  #confused matrix
refl.als.conf= as.matrix(refl.als.conf[[2]])

mnf.als.conf=mnf.als.model[[2]] #confused matrix
mnf.als.conf= as.matrix(mnf.als.conf[[2]])

true.refl= sum(diag(refl.als.conf))  #correctly classified samples in reflectance classification 281
false.refl=sum(colSums(refl.als.conf)) - sum(diag(refl.als.conf))  #falsely classified samples in refl classification

true.mnf= sum(diag(mnf.als.conf))  #correctly classified samples in reflectance classification
false.mnf=sum(colSums(mnf.als.conf)) - sum(diag(mnf.als.conf))  #falsely classified samples in mnf classification

(m<- matrix(c(true.refl, false.refl, false.mnf, true.mnf),
nrow = 2, ncol = 2, byrow=TRUE))
mcnemar.test(m, correct = FALSE)
```

```{r, eval = FALSE}
# your code
```






Now you should have built your own classification, and hopefully the classification accuracy is better. The final step is to assess if the increase in classification accuracy is statistically significant compared to the first classification. We will use McNemar's test (without correction for continuity) for the purpose. McNemar's test is a nonparametric test that can be applied to 2x2 confusion matrices. In this case we will use it to test if the total amount of correctly and falsely classified samples differed between the two classifications.
[example article](http://ieeexplore.ieee.org/document/1304900/)

__Assignment:__ You need to find a way to get four needed values for the test. Just insert the values in the correct places and run the test
```{r, eval = FALSE}

true.refl  = # Correctly classified samples in refl classification
false.refl = # Falsely classified samples in refl classification
true.mnf   = # Correctly classified samples in MNF classification
false.mnf  = # Falsely classified samples in MNF classification

( m <- matrix( c(true.refl, false.refl, false.mnf, true.mnf), 
nrow = 2, ncol = 2, byrow=TRUE) )           
mcnemar.test(m, correct = FALSE)   

```


__If you completed the exercise this far, you can tap yourself in the back as you just did some really challenging tree species classification using imaging spectroscopy and laser scanning data fusion using machine learning!__

If you have any questions or improvement suggestions email me at rami.piiroinen@helsinki.fi.