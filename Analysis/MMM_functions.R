

#################functions for "MMM_BatchMerge"##########################




# function to read and process a single CSV file, it processes all .csv files in the selected destination folder
process_file <- function(file_path) {
  cat(paste0("Processing ", file_path, "\n"))  # print file path
  read.csv(file_path, sep = ";") %>%     # read the csv file with semicolon delimiter
    mutate(filename = basename(file_path)) %>%     # add a column with the filename
    mutate(Batch = str_extract(filename, "B\\d")) %>%   # extract the Batch number from the filename
    mutate(Change = str_extract(filename, "CC\\d")) %>%   # extract the Change number from the filename
    select(-filename) # remove the filename column
}



#function to decide the sex of the mouse depending on the batch number
#Input "batch" is batch number of the individual
male_or_female <- function(batch){
  batch_name <-  str_extract(batch, "B\\d")
  if(batch_name == "B1" || batch_name == "B2"){
    return("m")
  }else{ #batch_name == "B3" || batch_name == "B4"
    return("f")
  }
}
  



# function to process and save as XLSX file
process_and_save_xlsx <- function(file_path) {
  file_base <- sub(".csv", "", basename(file_path))
  
  if(file.exists(paste0(sub(".csv", "", file_path),".xlsx"))){          #if the file is already processed to an xlsx it does not have to be prcessed again
    cat(paste0("file: ", file_base, " already exists as xlsx", "\n")) 
  } else {
    output_path <- paste0(dirname(file_path), "/", file_base, ".xlsx")
    csv_data <- process_file(file_path)
    write.xlsx(csv_data, output_path, rowNames = FALSE)
    cat(paste0("Saved ", output_path, "\n"))  # print output file path
    }
}





#################functions for "MMM_sleepAnalysis"##########################

### practical functions: ###

# function that checks if included factors exists as a column in the given data
# if it does not exist, the factor is not included
# no return value
# Input:  the factor to test (character)  
#         the data (dataframe) on which it is tested and 
#         the boolean value whether the factor is included or not
incldeFactorExist <-  function(factor, data, include_factor){
  if (include_factor && (factor %in% colnames(data))){
    include_factor <- TRUE
  }else{
    if(include_factor && !(factor %in% colnames(data))){
      cat(paste0("the given data does not contain the column ", factor,", ","include stays FALSE"))
    }
    include_factor <- FALSE
  }
  
}



## Function to calculate sleep bouts per phase
#calculate_total_sleep_info <- function(data) {
#  total_sleep_info <- data %>%
#    group_by(AnimalNum, Batch, Change, Group, Phase) %>%
#    summarise(
#      SleepCount = sum(ActivityIndex == 0),
#      TotalCount = n(),
#      PercentageSleep = (SleepCount / TotalCount) * 100,
#      TotalSleepingTime = sum(ActivityIndex == 0),
#      SleepBouts = sum(ActivityIndex == 0 & lag(ActivityIndex, default = 1) != 0),
#      AvgSleepBoutDuration = ifelse(SleepBouts == 0, 0, TotalSleepingTime / SleepBouts)
#    ) %>%
#    ungroup()
#  
#  return(total_sleep_info)
#}

## Assuming 'data_filtered' is your data frame
#total_sleep_info_per_change <- calculate_total_sleep_info(data_filtered)




# Function to generate plots for each variable and phase
generatePlot <- function(overallData, variableName, phase, sex) {
  filteredData <- overallData %>%
    filter(if (include_phase) Phase == phase else TRUE) %>%  # Include/exclude "Phase" based on the variable
    filter(if (include_sex) Sex == sex else TRUE)  # Include/exclude "Sex" based on the variable
  
  
  p <- ggplot(filteredData, aes(Group, .data[[variableName]], color = Group)) +
    # Customize plot aesthetics and labels
    scale_x_discrete(name = NULL, expand = c(0.3, 0.1)) + 
    scale_y_continuous(expand = c(0.1, 0.1)) +
    geom_jitter(aes(fill = Group), size = 4, alpha = 0.7, width = 0.2, shape = 16, na.rm = TRUE) +
    stat_summary(
      fun.min = function(z) {quantile(z, 0.25)},
      fun.max = function(z) {quantile(z, 0.75)},
      fun = median,
      color = "black",
      size = 0.8,
      shape = 16,
      na.rm = TRUE
    ) +
    labs(title = bquote(~bold(.(variableName))),
         subtitle = paste("(", phase, " ", sex, ")", sep = ""),
         caption = "",
         x = NULL,
         y = "z score [a.u.]") +
    scale_color_manual(name = NULL, values = groupColors) +
    scale_fill_manual(name = NULL, values = groupColors) +
    theme_minimal_hgrid(12, rel_small = 1) +
    theme(plot.title = element_text(hjust = 0.5, face = "plain"),
          plot.subtitle = element_text(hjust = 0.5, size = 10, face = "plain"),
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.text.x = element_text(),
          axis.ticks.x = element_blank())
  
  return(p)
}

### MATHEMATIC TESTS IN FUNCTIONS: ###


## parametric
# Function to perform post hoc pairwise tests for ANOVA
performPosthocAnova <- function(data, variableName) {
  anovaTest <- aov(as.formula(paste(variableName, "~ Group")), data = data)
  pairwiseResults <- pairwise_t_test(data, formula = as.formula(paste(variableName, "~ Group")),
                                     p.adjust.method = "bonferroni")

  # Remove duplicate comparisons
  pairwiseResults <- pairwiseResults[!duplicated(pairwiseResults[, c("group1", "group2")]), ]
  
  pairwiseResults$GroupComparison <- paste(pairwiseResults$group1, "vs.", pairwiseResults$group2)
  return(pairwiseResults)
}

# Function to perform post hoc pairwise tests for Two-Way ANOVA
performTwoWayAnovaPosthoc <- function(data, variableName) {
  anovaTest <- aov(as.formula(paste(variableName, "~ Group + Sex + Group:Sex")), data = data)
  pairwiseResults <- pairwise_t_test(data, 
                                     formula = as.formula(paste(variableName, "~ Group + Sex + Group:Sex")),
                                     p.adjust.method = "bonferroni")

  # Remove duplicate comparisons
  pairwiseResults <- pairwiseResults[!duplicated(pairwiseResults[, c("group1", "group2")]), ]
  
  pairwiseResults$GroupComparison <- paste(pairwiseResults$group1, "vs.", pairwiseResults$group2)
  return(pairwiseResults)
}


# Function to perform post hoc pairwise tests for Kruskal-Wallis
performPosthocKruskal <- function(data, variableName) {
  kruskalTest <- kruskal.test(as.formula(paste(variableName, "~ Group")), data = data)
  pairwiseResults <- dunn_test(data, formula = as.formula(paste(variableName, "~ Group")),
                               p.adjust.method = "holm")
  pairwiseResults$GroupComparison <- paste(pairwiseResults$group1, "vs.", pairwiseResults$group2)
  return(pairwiseResults)
}

# non parametric
# Function to perform Wilcoxon rank-sum test
performWilcoxonTest <- function(dataGroup1, dataGroup2) {
  if (length(dataGroup1) >= 3 && length(dataGroup2) >= 3) {
    return(wilcox.test(dataGroup1, dataGroup2))
  } else {
    return(NULL)
  }
}

# Function to perform normality test and appropriate statistical test for each variable and phase
testAndPlotVariable <- function(data, variableName, phase, sex) {
  #filtering specific phase or sex if needed 
  filteredData <- data %>%
    filter(if(include_phase) Phase == phase else TRUE) %>%   # Include/exclude "Phase" based on the variable
    filter(if(include_sex) Sex == sex else TRUE)            # Include/exclude "Sex" based on the variable

  # save unique group names
  uniqueGroups <- unique(filteredData$Group)  #SUS,RES,CON...
  # number of different groups in data
  numGroups <- length(uniqueGroups)
  
  # Check if the variable is numeric (if not, return Null)
  if (is.numeric(filteredData[[variableName]])) {
    if (numGroups == 2) {
      group1 <- uniqueGroups[1]
      group2 <- uniqueGroups[2]
      
      dataGroup1 <- filteredData[[variableName]][filteredData$Group == group1]
      dataGroup2 <- filteredData[[variableName]][filteredData$Group == group2]
      
      # if there are more than 3 columns containing the name of group1 AND group 2 , perform Wilcoxon test
      # WILCOXON
      if (sum(!is.na(dataGroup1)) >= 3 && sum(!is.na(dataGroup2)) >= 3) {
        wilcoxRes <- performWilcoxonTest(dataGroup1, dataGroup2)
        
        #if results of Wilcoxontest are not empty add results to testResults
        if (!is.null(wilcoxRes)) {
          testResults <- list(
            Variable = variableName,
            Phase = phase,
            Sex = sex,
            Test = "Wilcoxon rank-sum test",
            CON_Normality = NA,
            RES_Normality = NA,
            SUS_Normality = NA,
            P_Value = wilcoxRes$p.value,
            Significance_Level = sprintf("%.3f", wilcoxRes$p.value)
          )
          
          #generate plot of wilcoxontest results
          p <- generatePlot(filteredData, variableName, phase, sex)
          
          return(list(testResults = testResults, plot = p, posthocResults = NULL))
        }
      }
    } else {  # numGroups /= 2

        # initialize empty dataframes for ANOVA
        testResultsDf <- data.frame()
        posthocResultsDf <- data.frame()
     
        ## NORMALITY TEST
        # perform Shapiro-Wilk Normality Test for each group
        conNorm <- shapiro.test(filteredData[[variableName]][filteredData$Group == "CON"])
        resNorm <- shapiro.test(filteredData[[variableName]][filteredData$Group == "RES"])

        # check if SUS group exists and perform Shapiro-Wilk Normality Test if it does
        susGroupExists <- any(filteredData$Group == "SUS")
            if (susGroupExists) {
                susNorm <- shapiro.test(filteredData[[variableName]][filteredData$Group == "SUS"])
            } else {
                susNorm <- list(p.value = 1)
            }
            # if one of the p values of the Normality Testing is empty, return NULL
            if (is.na(conNorm$p.value) || is.na(resNorm$p.value) || is.na(susNorm$p.value)) {
                return(NULL)
            }

            # register Normality Test results in testResults for return
            testResults <- list(
                Variable = variableName,
                Phase = phase,
                Sex = sex,
                CON_Normality = conNorm$p.value,
                RES_Normality = resNorm$p.value,
                SUS_Normality = susNorm$p.value
            )

            ## ANOVA
            if (numGroups > 2) {
                # every p value has to be not significant (>=0.05)
                if (conNorm$p.value >= 0.05 && resNorm$p.value >= 0.05 && susNorm$p.value >= 0.05) {
                    anovaTest <- aov(as.formula(paste(variableName, "~ Group")), data = filteredData)
                    testResults$Test <- "ANOVA"
                    testResults$P_Value <- summary(anovaTest)[[1]][["Pr(>F)"]][1]
                    testResults$Significance_Level <- sprintf("%.3f", testResults$P_Value)
                    # posthoc for ANOVA
                    posthocResultsDf <- performPosthocAnova(filteredData, variableName)
                } else { # Kruskal Wallis
                    kruskalTest <- kruskal.test(as.formula(paste(variableName, "~ Group")), data = filteredData)
                    testResults$Test <- "Kruskal-Wallis"
                    testResults$P_Value <- kruskalTest$p.value
                    testResults$Significance_Level <- sprintf("%.3f", p.adjust(testResults$P_Value, method = "BH"))

                    posthocResultsDf <- performPosthocKruskal(filteredData, variableName)
                }

                #if (twoWayANOVA == TRUE) {
                #    twoWayAnovaTest <- performTwoWayAnova(filteredData, variableName)
                #    testResultsTwoWay <- list(
                #        Variable = variableName,
                #        Phase = phase,
                #        Sex = sex,
                #        Test = "Two-way ANOVA",
                #        P_Value = twoWayAnovaTest$p.value,
                #        Significance_Level = sprintf("%.3f", p.adjust(twoWayAnovaTest$p.value, method = "BH"))
                #    )
                #    # posthoc for two way ANOVA
                #    posthocResultsTwoWay <- performTwoWayAnova(filteredData, variableName)
                #}
                
                if (!is.null(posthocResultsDf) && ncol(posthocResultsDf) > 0) {
                    if (identical(names(testResultsDf), names(posthocResultsDf))) {
                        testResultsDf <- bind_rows(testResultsDf, posthocResultsDf)
                    } else {
                        testResultsDf <- plyr::rbind.fill(testResultsDf, posthocResultsDf)
                    }
                }
                
                
            }

        # generate plots in p for return
        p <- generatePlot(filteredData, variableName, phase, sex)

        return(list(testResults = testResults, plot = p, posthocResults = testResultsDf))
    }
    } else {
        cat("Variable", variableName, "is not numeric and will be skipped.\n")
        return(NULL)
    }
}


### saving functions: ###

# Save the plots as graphs(number and name of graph-file depends on included factors)
#Input:   allTestResults(dataframe with results)
#         allPlots(dataframe with plots), 
#         target_directory(string), 
#         fileType(string)
savePlotsInDir <- function(allTestResults, allPlots, target_directory, fileType){
  for (i in seq_along(allPlots)) { #for every plot that is documented
    #declare variables that are (potentially) included in filename
    variableName <- allTestResults[[i]]$Variable 
    ifelse(include_phase, phaseName <- paste0(allTestResults[[i]]$Phase, "_"), phaseName <-  "")    #new variable phaseName to use in the name of the file
    ifelse(include_sex, sexName <- paste0(allTestResults[[i]]$Sex, "_"), sexName <-  "") 

    
    #save in the directory for graphs 
    #alterate path if extra factors included
    if(include_phase) factorDir <-  "/include_phase"
    if(include_sex) factorDir <-  "/include_sex" 
    if(include_phase && include_sex) factorDir <- "/include_phase_and_sex"
    if(!include_phase && !include_sex) factorDir <- ""
    ggsave(filename = paste0(target_directory, factorDir, "/", phaseName, sexName, variableName, fileType), plot = allPlots[[i]], width = 2.8, height = 3)
    
  }
}