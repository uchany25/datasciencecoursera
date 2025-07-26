## 1: Synopsis

    In this project, we will explore the effect caused by storms and other severe weather events that impact both public health and the economy such as fatalities, injuries, and property damage. Our primary goal is to identify ways to prevent these dangerous outcomes as much as possible through further analysis. 

    The database used in this analysis is the storm database from the U.S National Oceanic and Atmospheric Administration's (NOAA). The data is stored in a CSV file named StormData.csv and covers the period from 1950 to November 2011. It includes information about when and where major storms and weather event occured in the United States, as well as estimates of any faltalities, injuries, and property damage.

    You can download the file using the following link:
    [StormData.csv]("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2")

    The analysis will investigate which types of events are most harmful on:

    1. Healthy

    2. Property and crops (Economic consequences)

## 2: Data Processing

### 2.1: Data Loading

    setwd("C:/Users/PC/Documents/GitHub/datasciencecoursera/5. Reproducible Research/Project_2")

    if (isFALSE(("StormData.csv" %in% list.files(getwd())))) {
            url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
            download.file(url, file.path(getwd(), "StormData.csv"))
            df <- read.csv("StormData.csv")
    } else df <- read.csv("StormData.csv")

    head(df)

    ##   STATE__           BGN_DATE BGN_TIME TIME_ZONE
    ## 1       1  4/18/1950 0:00:00     0130       CST
    ## 2       1  4/18/1950 0:00:00     0145       CST
    ## 3       1  2/20/1951 0:00:00     1600       CST
    ## 4       1   6/8/1951 0:00:00     0900       CST
    ## 5       1 11/15/1951 0:00:00     1500       CST
    ## 6       1 11/15/1951 0:00:00     2000       CST
    ##   COUNTY COUNTYNAME STATE  EVTYPE BGN_RANGE BGN_AZI
    ## 1     97     MOBILE    AL TORNADO         0        
    ## 2      3    BALDWIN    AL TORNADO         0        
    ## 3     57    FAYETTE    AL TORNADO         0        
    ## 4     89    MADISON    AL TORNADO         0        
    ## 5     43    CULLMAN    AL TORNADO         0        
    ## 6     77 LAUDERDALE    AL TORNADO         0        
    ##   BGN_LOCATI END_DATE END_TIME COUNTY_END COUNTYENDN
    ## 1                                       0         NA
    ## 2                                       0         NA
    ## 3                                       0         NA
    ## 4                                       0         NA
    ## 5                                       0         NA
    ## 6                                       0         NA
    ##   END_RANGE END_AZI END_LOCATI LENGTH WIDTH F MAG
    ## 1         0                      14.0   100 3   0
    ## 2         0                       2.0   150 2   0
    ## 3         0                       0.1   123 2   0
    ## 4         0                       0.0   100 2   0
    ## 5         0                       0.0   150 2   0
    ## 6         0                       1.5   177 2   0
    ##   FATALITIES INJURIES PROPDMG PROPDMGEXP CROPDMG
    ## 1          0       15    25.0          K       0
    ## 2          0        0     2.5          K       0
    ## 3          0        2    25.0          K       0
    ## 4          0        2     2.5          K       0
    ## 5          0        2     2.5          K       0
    ## 6          0        6     2.5          K       0
    ##   CROPDMGEXP WFO STATEOFFIC ZONENAMES LATITUDE
    ## 1                                         3040
    ## 2                                         3042
    ## 3                                         3340
    ## 4                                         3458
    ## 5                                         3412
    ## 6                                         3450
    ##   LONGITUDE LATITUDE_E LONGITUDE_ REMARKS REFNUM
    ## 1      8812       3051       8806              1
    ## 2      8755          0          0              2
    ## 3      8742          0          0              3
    ## 4      8626          0          0              4
    ## 5      8642          0          0              5
    ## 6      8748          0          0              6

### 2.2 Data Preprocessing

    col_selected = c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")
    df_storms <- df[, col_selected]

    We will select only the following seven variables: EVTYPE, FATALITIES, INJURIES, PROPDMG, PROPDMGEXP, CROPDMG, and CROPDMGEXP. And we will make a new data frame named df_storms.

    df_storms$EVTYPE_cleaned <- toupper(df_storms$EVTYPE)
    df_storms$EVTYPE_cleaned <- gsub("[0-9]", "", df_storms$EVTYPE_cleaned)
    df_storms$EVTYPE_cleaned <- gsub("\\(.*\\)", "", df_storms$EVTYPE_cleaned)
    df_storms$EVTYPE_cleaned <- trimws(df_storms$EVTYPE_cleaned)
    df_storms$EVTYPE_cleaned <- gsub("\\s+", " ", df_storms$EVTYPE_cleaned)

    We observe that the EVTYPE variable contains many typos and is quite messy. So we will clean the text and store in a new variable named EVTYPE_cleaned.

### 2.3 Hierarchical clustering

    library(stringdist)
    unique_evtypes <- unique(df_storms$EVTYPE_cleaned)
    dist_matrix <- stringdistmatrix(unique_evtypes, unique_evtypes, method = "lv")
    rownames(dist_matrix) <- unique_evtypes
    colnames(dist_matrix) <- unique_evtypes

    hc <- hclust(as.dist(dist_matrix), method = "single")

    plot(hc, hang = -1)

![](ReproducibleResearch_Project_2_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    Now, we are using hierarchical clustering to identify potential similarities and redundancies within our EVTYPE_cleaned variable. We first calculate the Levenshtein distance between every pair of unique storm event types. The distance tells us how many single- character edits are needed to transform one event type string into another, effectively quantifying their textual similarity.

    Then, we apply hierarchical clustering with a 'single linkage' method to these distances. This process iteratively merge the closet pairs of event types (or cluster event types) until all event types are grouped into a single large cluster.

    The resulting dendrogram visually represents this merge process.

    clusters_h1 = cutree(hc, h = 1)
    grouped_evtypes_list <- split(hc$labels, clusters_h1)
    cluster_lengths <- sapply(grouped_evtypes_list, length)

    multi_member_clusters <- grouped_evtypes_list[cluster_lengths > 1]

    mapping_dict_draft <- list()
    for (i in seq_along(grouped_evtypes_list)) {
            cluster_members <- grouped_evtypes_list[[i]]
            
            standard_name <- cluster_members[1]
            
            for (member in cluster_members) {
                    mapping_dict_draft[[member]] <- standard_name
            }
    }

    mapping_dict_draft[["FOG"]] <- "FOG"
    mapping_dict_draft[["VOG"]] <- "VOG"
    mapping_dict_draft[["SUMMARY OF MAY AM"]] <- "SUMMARY OF MAY AM"
    mapping_dict_draft[["SUMMARY OF MAY PM"]] <- "SUMMARY OF MAY PM"
    mapping_dict_draft[["HAIL .)"]] <- "HAIL"
    mapping_dict_draft[["HAIL ."]] <- "HAIL"

    mapping_df <- data.frame(
            original_evtype = names(mapping_dict_draft),
            standard_evtype = unlist(mapping_dict_draft),
            stringsAsFactors = FALSE
    )

    final_mapping_df <- mapping_df

    final_mapping_dict <- setNames(final_mapping_df$standard_evtype, final_mapping_df$original_evtype)
    df_storms$EVTYPE_standardized <- final_mapping_dict[df_storms$EVTYPE_cleaned]

    we are cutting tree at a height of 1. After checking mapping_dict_draft, we observed that some clusters incorrectly grouped distinct event types. Therefore, we manually split and adjusted these mappings. The refined clusters are then stored in a new variable called EVTYPE_standardized within our df_storms dataset.

    df_storms <- df_storms[(df_storms$EVTYPE_standardized != "?" & (df_storms$INJURIES > 0 | df_storms$FATALITIES > 0 | df_storms$PROPDMG > 0 | df_storms$CROPDMG > 0)), c("EVTYPE_standardized", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]

    EXP_col <- c("PROPDMGEXP", "CROPDMGEXP")
    df_storms[EXP_col] <- lapply(df_storms[,EXP_col], toupper)

    We are excluding the value "?" from EVTYPE_standardized. And we are filtering out rows that none of the impact columns (INJURIES, FATALITIES, PROPDMG, or CROPDMG) have a value greater than zero. In other words, we retain only those storm events that resulted in at least one injury, fatality, property damage, or crop damage.

### 2.4 Converting Exponent Columns

    options(scipen = 999)
    sort(unique(df_storms$PROPDMGEXP))

    ##  [1] ""  "-" "+" "0" "2" "3" "4" "5" "6" "7" "B" "H"
    ## [13] "K" "M"

    sort(unique(df_storms$CROPDMGEXP))

    ## [1] ""  "?" "0" "B" "K" "M"

    df_storms$PROPDMGEXP[df_storms$PROPDMGEXP == ""] <- "EMPTY"
    df_storms$CROPDMGEXP[df_storms$CROPDMGEXP == ""] <- "EMPTY"

    PROPDMGEXP_numeric <- c("EMPTY" = 10^0, "-" = 10^0, "+" = 10^0, "0" = 10^0, "2" = 10^2, "3" = 10^3, "4" = 10^4, "5" = 10^5, "6" = 10^6, "7" = 10^7, "B" = 10^9, "H" = 10^2, "K" = 10^3, "M" = 10^6)
    CROPDMGEXP_numeric <- c("EMPTY" = 10^0, "?" = 10^0, "0" = 10^0, "K" = 10^3, "M" = 10^6, "B" = 10^9)

    df_storms$PROPDMGEXP <- PROPDMGEXP_numeric[df_storms$PROPDMGEXP]
    df_storms$CROPDMGEXP <- CROPDMGEXP_numeric[df_storms$CROPDMGEXP]

    In this part, we are converting the codes into numeric.

### 2.5 Making Economic Cost Columns

    df_storms <- transform(df_storms, PROPCOST = df_storms$PROPDMG*df_storms$PROPDMGEXP, CROPCOST = df_storms$CROPDMG*df_storms$CROPDMGEXP)

    In this step, we are calculating the total estimated costs of property and crop damage and adding them as new variables to our df_storms dataset.

### 2.6 Total Economy Damage Cost

    storms_cost <- aggregate(cbind(df_storms$PROPCOST, df_storms$CROPCOST, df_storms$PROPCOST+df_storms$CROPCOST) ~ df_storms$EVTYPE_standardized, data = df_storms, sum)

    colnames(storms_cost) <- c("EVTYPE", "PROPCOST", "CROPCOST", "TOTALCOST")
    storms_cost <- storms_cost[order(storms_cost$TOTALCOST, decreasing = TRUE),][1:10,]

    head(storms_cost)

    ##                EVTYPE     PROPCOST   CROPCOST
    ## 67              FLOOD 144663709807 5662018450
    ## 167 HURRICANE/TYPHOON  69305840000 2607872800
    ## 287           TORNADO  56947380677  414954270
    ## 266       STORM SURGE  43323536000       5000
    ## 99               HAIL  15735819513 3026044473
    ## 56        FLASH FLOOD  16831952479 1421317100
    ##        TOTALCOST
    ## 67  150325728257
    ## 167  71913712800
    ## 287  57362334947
    ## 266  43323541000
    ## 99   18761863986
    ## 56   18253269579

### 2.7 Total health Damage Cost

    storms_DMG <- aggregate(cbind(df_storms$FATALITIES, df_storms$INJURIES, df_storms$FATALITIES+df_storms$INJURIES) ~ df_storms$EVTYPE_standardized, data = df_storms, sum)

    colnames(storms_DMG) <- c("EVTYPE", "INJURIES", "FATALITIES", "TOTALDMG")

    storms_DMG <- storms_DMG[order(storms_DMG$TOTALDMG, decreasing = TRUE),][1:10,]

    head(storms_DMG)

    ##             EVTYPE INJURIES FATALITIES TOTALDMG
    ## 287        TORNADO     5633      91346    96979
    ## 46  EXCESSIVE HEAT     1903       6525     8428
    ## 298      TSTM WIND      505       6961     7466
    ## 67           FLOOD      470       6789     7259
    ## 193      LIGHTNING      817       5230     6047
    ## 105           HEAT      937       2100     3037

## 3: Results

### 3.1 Most population problem

    library(ggplot2)
    library(reshape2)

    population_problem <- melt(storms_DMG, id.vars = "EVTYPE", variable.name = "health_type")

    ggplot(population_problem, aes(x = reorder(EVTYPE, -value), y = value))+geom_col(aes(fill = health_type),position="dodge")+theme(axis.text.x = element_text(angle=45, hjust=1))+xlab("Event Type")+ylab("Frequency Count")+ggtitle("TOP 10 US storm events impact Population problem")

![](ReproducibleResearch_Project_2_files/figure-markdown_strict/unnamed-chunk-12-1.png)

    As you see the bar graph, Tornado is the most impactful event type in the US regarding population problem.

### 3.2 Most Economy problem

    economy_problem <- melt(storms_cost, id.vars = "EVTYPE", variable.name = "Damage_type")

    ggplot(economy_problem, aes(x = reorder(EVTYPE, -value), y = value))+geom_col(aes(fill = Damage_type), position = "dodge")+theme(axis.text.x = element_text(angle = 45, hjust = 1))+xlab("Event Type")+ylab("Damage Cost")+ggtitle("TOP 10 US storm events impact Economic problem")

![](ReproducibleResearch_Project_2_files/figure-markdown_strict/unnamed-chunk-13-1.png)

    As you see the bar graph, Flood is the most impactful event type in the US regarding property cost damage, while Drought is the most impactful event type in the US regarding crop cost damage.
