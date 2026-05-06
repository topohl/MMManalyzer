

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




# 1. Define the Nature Theme
theme_nature <- function() {
  theme_classic(base_size = 8) +
    theme(
      text = element_text(color = "black"),
      axis.line = element_line(linewidth = 0.3, color = "black"),
      axis.ticks = element_line(linewidth = 0.3, color = "black"),
      axis.text = element_text(size = 7, color = "black"),
      axis.title = element_text(size = 8, face = "bold"),
      plot.title = element_text(size = 9, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 7, hjust = 0.5),
      legend.position = "none",
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    )
}

# Helper: choose test strategy from normality and return pairwise stats
compute_pairwise_stats <- function(data, variableName) {
  data <- data %>% filter(!is.na(Group), !is.na(.data[[variableName]]))
  groups <- unique(as.character(data$Group))
  if (length(groups) < 2) return(NULL)

  split_data <- split(data[[variableName]], data$Group)
  normal_flags <- vapply(split_data, function(x) {
    x <- x[!is.na(x)]
    length(x) >= 3 && shapiro.test(x)$p.value >= 0.05
  }, logical(1))
  all_normal <- all(normal_flags)

  if (length(groups) == 2) {
    g1 <- groups[1]; g2 <- groups[2]
    x1 <- data[[variableName]][data$Group == g1]
    x2 <- data[[variableName]][data$Group == g2]

    pval <- if (all_normal) t.test(x1, x2)$p.value else wilcox.test(x1, x2)$p.value

    stats <- data.frame(
      group1 = g1,
      group2 = g2,
      p = pval,
      p.adj = pval,
      stringsAsFactors = FALSE
    )
  } else {
    if (all_normal) {
      stats <- pairwise_t_test(
        data,
        formula = as.formula(paste(variableName, "~ Group")),
        p.adjust.method = "bonferroni"
      )
    } else {
      stats <- dunn_test(
        data,
        formula = as.formula(paste(variableName, "~ Group")),
        p.adjust.method = "holm"
      )
    }
  }

  if (!"p.adj" %in% names(stats)) stats$p.adj <- stats$p

  stats$label <- ifelse(
    stats$p.adj < 0.001, "***",
    ifelse(stats$p.adj < 0.01, "**", ifelse(stats$p.adj < 0.05, "*", "ns"))
  )
  stats$method <- if (all_normal) "parametric" else "non-parametric"
  stats
}

# Nature-style plotting with statistical annotations
generatePlot <- function(overallData, variableName, phase, sex) {
  filteredData <- overallData %>%
    filter(if (exists("include_phase") && include_phase) Phase == phase else TRUE) %>%
    filter(if (exists("include_sex") && include_sex) Sex == sex else TRUE)

  subtitle_parts <- c()
  if (exists("include_phase") && include_phase) subtitle_parts <- c(subtitle_parts, phase)
  if (exists("include_sex") && include_sex) subtitle_parts <- c(subtitle_parts, sex)
  subtitle_text <- if (length(subtitle_parts) > 0) paste(subtitle_parts, collapse = " | ") else NULL

  p <- ggplot(filteredData, aes(Group, .data[[variableName]], color = Group)) +
    geom_jitter(width = 0.2, size = 2, alpha = 0.6, stroke = 0, na.rm = TRUE) +
    stat_summary(
      fun.min = function(z) quantile(z, 0.25, na.rm = TRUE),
      fun.max = function(z) quantile(z, 0.75, na.rm = TRUE),
      geom = "errorbar",
      width = 0.2,
      linewidth = 0.4,
      color = "black",
      na.rm = TRUE
    ) +
    stat_summary(
      fun = median,
      geom = "crossbar",
      width = 0.4,
      linewidth = 0.4,
      color = "black",
      na.rm = TRUE
    ) +
    labs(
      title = variableName,
      subtitle = subtitle_text,
      x = NULL,
      y = "z score [a.u.]"
    ) +
    scale_color_manual(values = groupColors) +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.2))) +
    theme_nature()

  # Add significance brackets
  stat_df <- compute_pairwise_stats(filteredData, variableName)
  if (!is.null(stat_df) && nrow(stat_df) > 0) {
    stat_df <- stat_df %>%
      dplyr::ungroup() %>%
      as.data.frame() %>%
      dplyr::filter(!is.na(group1), !is.na(group2), !is.na(p.adj))

    if (nrow(stat_df) > 0) {
      y_max <- max(filteredData[[variableName]], na.rm = TRUE)
      y_min <- min(filteredData[[variableName]], na.rm = TRUE)
      y_span <- ifelse(is.finite(y_max - y_min) && (y_max - y_min) > 0, y_max - y_min, 1)

      stat_df$y.position <- y_max + (seq_len(nrow(stat_df)) * 0.1 * y_span)

      p <- tryCatch({
        p +
          ggpubr::stat_pvalue_manual(
            stat_df,
            label = "label",
            xmin = "group1",
            xmax = "group2",
            y.position = "y.position",
            hide.ns = TRUE,
            tip.length = 0.01,
            bracket.size = 0.3,
            size = 2.5,
            step.increase = as.numeric(0)
          ) +
          labs(caption = paste0("Stats: ", unique(stat_df$method)))
      }, error = function(e) {
        p + labs(caption = paste0("Stats: ", unique(stat_df$method), " (annotation skipped)"))
      })
    }
  }

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

#' Non-parametric: Wilcoxon rank-sum (Mann–Whitney U)
#'
#' Compares two independent groups when normality cannot be assumed.
#' Uses stats::wilcox.test with default (unpaired) settings.
#'
#' @param dataGroup1 Numeric vector of observations for group 1.
#' @param dataGroup2 Numeric vector of observations for group 2.
#' @return An htest object from wilcox.test, or NULL if either group has fewer than 3 non-NA values.
performWilcoxonTest <- function(dataGroup1, dataGroup2) {
  if (length(dataGroup1) >= 3 && length(dataGroup2) >= 3) {
    return(wilcox.test(dataGroup1, dataGroup2))
  } else {
    return(NULL)
  }
}

# Function to perform normality test and appropriate statistical test for each variable and phase
testAndPlotVariable <- function(data, variableName, phase, sex) {
    cat("Processing variable:", variableName, "Phase:", phase, "Sex:", sex, "\n", append = TRUE)

  # Check if variableName exists and is numeric
  if (!(variableName %in% names(data))) {
    cat("Error: variableName", variableName, "does not exist in the data.\n", append = TRUE)
    return(NULL)
  }
  if (!is.numeric(data[[variableName]])) {
    cat("Error: variableName", variableName, "is not numeric.\n", append = TRUE)
    return(NULL)
  }

  # Filtering specific phase or sex if needed
  filteredData <- data %>%
    filter(if(include_phase) Phase == phase else TRUE) %>%   # Include/exclude "Phase" based on the variable
    filter(if(include_sex) Sex == sex else TRUE)            # Include/exclude "Sex" based on the variable
  # Check if there is data remaining after filtering
  if (nrow(filteredData) == 0) {
    cat("Error: No data remaining after filtering.\n", append = TRUE)
    return(NULL)
  }

  # save unique group names
  uniqueGroups <- unique(filteredData$Group)  #SUS,RES,CON...
  # number of different groups in data
  numGroups <- length(uniqueGroups)
  # calculate the n of each group
  nGroup1 <- sum(filteredData$Group == uniqueGroups[1])
  nGroup2 <- sum(filteredData$Group == uniqueGroups[2])
  nGroup3 <- sum(filteredData$Group == uniqueGroups[3])

  # print unique groups
  cat(paste0("Unique groups: ", uniqueGroups, "\n"))

  # print number of groups
  cat(paste0("Number of groups: ", numGroups, "\n"))

  # print the number of observations of each group
  cat(paste0("Number of observations in ", uniqueGroups[1], ": ", nGroup1, "\n"))
  cat(paste0("Number of observations in ", uniqueGroups[2], ": ", nGroup2, "\n"))
  cat(paste0("Number of observations in ", uniqueGroups[3], ": ", nGroup3, "\n"))

  # Debugging information
  cat("Unique groups:", paste(uniqueGroups, collapse = ", "), "\n")
  cat("Number of groups:", numGroups, "\n")
  for (group in uniqueGroups) {
    groupData <- filteredData[[variableName]][filteredData$Group == group]
    cat("Number of observations in", group, ":", length(groupData), "\n")
    cat("Sample data for", group, ":", head(groupData), "\n")
  }

  # Check if the variable is numeric (if not, return Null)
  if (is.numeric(filteredData[[variableName]])) {
    if (numGroups == 2) {
      group1 <- uniqueGroups[1]
      group2 <- uniqueGroups[2]

      dataGroup1 <- filteredData[[variableName]][filteredData$Group == group1]
      dataGroup2 <- filteredData[[variableName]][filteredData$Group == group2]

      # if there are more than 3 columns containing the name of
      # group1 AND group 2, perform Wilcoxon test
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

          # Generate plot of Wilcoxon test results
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
        # twoWayAnovaTest <- performTwoWayAnova(filteredData, variableName)
        # testResultsTwoWay <- list(
        # Variable = variableName,
        # Phase = phase,
        # Sex = sex,
        # Test = "Two-way ANOVA",
        # P_Value = twoWayAnovaTest$p.value,
        # Significance_Level = sprintf("%.3f", p.adjust(twoWayAnovaTest$p.value, method = "BH"))
        # )
        # posthoc for two way ANOVA
        # posthocResultsTwoWay <- performTwoWayAnova(filteredData, variableName)
        # }

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

# Function to create a simple GUI to get user input for analysis settings
get_user_input <- function() {
  tt <- tktoplevel(); tkwm.title(tt, "Analysis Settings"); tkconfigure(tt, bg = "#F7F9FC", padx = 10, pady = 20)
  includeChange_var <- tclVar(0); includeSex_var <- tclVar(0); includePhase_var <- tclVar(0)
  frame <- tkframe(tt, bg = "#F7F9FC"); tkgrid(frame, padx = 10, pady = 10)
  tkgrid(tklabel(frame, text = "Set Inclusion", bg = "#F7F9FC", font = c("Helvetica", 14)))
  tkgrid(tklabel(frame, text = "Parameters",   bg = "#F7F9FC", font = c("Helvetica", 14)))
  tkgrid(tklabel(frame, text="Change", bg="#F7F9FC", font=c("Helvetica",12)),
         tkcheckbutton(frame, variable = includeChange_var, bg="#F7F9FC"), padx=5, pady=5)
  tkgrid(tklabel(frame, text="Sex", bg="#F7F9FC", font=c("Helvetica",12)),
         tkcheckbutton(frame, variable = includeSex_var, bg="#F7F9FC"), padx=5, pady=5)
  tkgrid(tklabel(frame, text="Phase", bg="#F7F9FC", font=c("Helvetica",12)),
         tkcheckbutton(frame, variable = includePhase_var, bg="#F7F9FC"), padx=5, pady=5)
  onOK <- function() tkdestroy(tt)
  ok_button <- tkbutton(tt, text="OK", command=onOK, bg="#007AFF", fg="white", relief="flat", font=c("Helvetica",12))
  tkgrid(ok_button, padx=10, pady=10); tkwait.window(tt)
  list(
    includeChange = as.logical(as.numeric(tclvalue(includeChange_var))),
    includeSex    = as.logical(as.numeric(tclvalue(includeSex_var))),
    includePhase  = as.logical(as.numeric(tclvalue(includePhase_var)))
  )
}