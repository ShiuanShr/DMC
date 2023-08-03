
##########################Install and clear rm#############################
rm(list = ls()) 
# Package names
packages <- c("hash", "rpart.plot", "stringr","dplyr", "knitr", 
              "ggplot2","randomForest", "reshape2", "pheatmap",
              "data.table","stringdist", "gplots","pheatmap",  "RColorBrewer")
install_or_load_pack <- function(pack){
    
    create.pkg <- pack[!(pack %in% installed.packages()[, "Package"])]
    
    if (length(create.pkg))
        
        install.packages(create.pkg, dependencies = TRUE)
    
    sapply(pack, require, character.only = TRUE)
}
install_or_load_pack(packages)

######################## SCENARIO SETTING #############################################
cp_treeA = 0.005
cp_treeB = 0.005
symetric_chaos = 0.04
train_ratio = 0.8

###################Data preprocessing#############################
df <- read.csv('raw_data/insurance.csv', header = T)
# Data preprocessing
df <- df %>% mutate(across(c("sex", "region"), as.character))
df <- na.omit(df)
str(df)

# Get the Train set  and test set
train.index <- sample(x=1:nrow(df), size=ceiling(train_ratio*nrow(df) ))

Train <- df[train.index, ]
Test <- df[-train.index, ]
dependent_variable <- "expenses"

Train_X = Train[, colnames(as.data.frame(Test)) != dependent_variable] # remove the DV in colnames for training set
Test_X = Test[, colnames(as.data.frame(Test)) != dependent_variable] # remove the DV in colnames for test set

marbels<- Train_X
####################add_noise############################


add_noise <- function(data, continuous = 0.02, discrete = 0.02, except = c("expenses", "children")) {
    ###############################################################
    ## Function for adding noise to continuous features and apply permutation to targeted discrete features
    ## INPUT:
    ## data: dataframe
    ## continuous: default 0.02 to maintain rmse variation below 5%.  If the data in a column is skewed, the noise magnitude would be based on standard deviation.
    ## If it's a normal distribution, it would be based on the range, e.g., 1%.
    ## discrete:  default 0.01.The percentage of the row data will be shuffled.
    ###############################################################
    # Identify numeric columns
    numeric_cols <- sapply(data, is.numeric)
    
    # Add noise to numeric columns
    for (col in names(data)[numeric_cols]) {
        noise_mag <- continuous * sd(data[[col]], na.rm = TRUE)
        data[[col]] <- data[[col]] + rnorm(n = nrow(data), mean = 0, sd = noise_mag)
    }
    ######################### Add noise to categorical columns: randomly swap the instances of  the categorical variables 
    permute_categorical <- function(dataframe, discrete,except) {
        categorical_cols <- sapply(dataframe,  function(x) is.factor(x) | is.character(x))
        discrete_cols <- categorical_cols & !names(dataframe) %in% except
        for (col in names(dataframe)[categorical_cols]) {
            col_values <- unique(dataframe[[col]])
            permute_count <- ceiling(discrete * nrow(dataframe))
            
            if (permute_count > 0) {
                permute_indices <- sample(which(dataframe[[col]] %in% col_values), permute_count)
                dataframe[permute_indices, col] <- sample(col_values, permute_count, replace = TRUE)
            }
        }
        
        return(dataframe)
    }
    if (discrete > 0) {
        data <- permute_categorical(data, discrete, except)
    }
    
    return(data)
}


# Notice that flipping categorical vars is more damaging, so we reduced their change to get ~5% delta of rmse

perturbed_Train <- add_noise(Train, continuous =  symetric_chaos, discrete = symetric_chaos,except =  c("expenses"))
# perturbed_Train <- Train # Create idential Tree
perturbed_Train_X = perturbed_Train[, colnames(as.data.frame(Test)) != dependent_variable] # Remove the label
# Print the perturbed df

# write.csv(perturbed_Train, 'raw_data/CH5/perturbed_Train.csv', row.names=TRUE)

####################Model Building############################


set.seed(100)
insurance_model <- rpart(expenses ~ age+sex+bmi +children +smoker +region, 
                         data = Train, 
                         method = "anova",
                         minsplit = 20 ,
                         cp = cp_treeA)
set.seed(100)
perturb_model <- rpart(expenses ~ age+sex+bmi +children +smoker +region, 
                       data = perturbed_Train,  
                       method = "anova",
                       minsplit = 20, 
                       cp = cp_treeB)


####################################################################################
# create additional plots
# par(mfrow=c(1,2)) # two plots on one page
# rsq.rpart(insurance_model) # visualize cross-validation results 
####################################################################################
# # Suppose the perturbated model's prediction shall closer to the  non-perturbated's testset prediction
perturb_pred <- predict(perturb_model, newdata=Test, type = "vector")
non_perturb_pred <- predict(insurance_model, newdata=Test, type = "vector")
pred <- data.frame(real=Test$expenses, perturb_pred = perturb_pred, non_perturb_pred = non_perturb_pred)
head(pred)

# 3. R SQUARED: 表示模型對目標變量變異性的解釋程度
RSQUARE = function(y_actual,y_predict){
  cor(y_actual,y_predict)^2
}
non_RSQUARE <- RSQUARE(pred$real, pred$non_perturb_pred)
Perb_RSQUARE <- RSQUARE(pred$real, pred$perturb_pred)
cat("RSQUARE(non_perturb_pred):", RSQUARE(pred$real, pred$non_perturb_pred), "\n")
cat("RSQUARE(perturb_pred):",RSQUARE(pred$real, pred$perturb_pred),  "\n\n")
cat("RMSE Variation", round(((Perb_RSQUARE - non_RSQUARE) / non_RSQUARE)*100,2), "%\n\n")
# 4. RMSE
RMSE = function(y_actual,y_predict){
  sqrt(mean((y_actual-y_predict)^2))
}
non_RMSE <- RMSE(pred$real, pred$non_perturb_pred)
Perb_RMSE <- RMSE(pred$real, pred$perturb_pred)
cat("RMSE(non_perturb_pred):", non_RMSE, "\n")
cat("RMSE(perturb_pred):", Perb_RMSE,  "\n\n")
cat("RMSE Variation:", round(((Perb_RMSE - non_RMSE) / non_RMSE)*100,2),  "%\n\n")


####################DT Plot######################
par(mfrow = c(1, 1), pty = "m", mar = c(0.5, 0.5, 0.5, 0.5))
plot_dt <- function(model, title){
    plot_model <- prp(model,
        type = 3,
        extra = "auto",
        fallen.leaves = TRUE,
        clip.right.labs = FALSE, # do not truncate the label even though it's mentioned 
        leaf.round = 0.5,
        main = ""
    ) 
    title(main = title, line = -1.5, outer = FALSE)
    return(plot_model)
}
plot_dt(insurance_model, "Tree A")
plot_dt(perturb_model, "Tree B")




####################Model Information############################


get_sorted_leaves <- function(rpart_model, layer = "<leaf>") {
    mask <- rpart_model$frame$var == layer
    leaves <- rpart_model$frame[mask, ]
    sorted_order <- order(row.names(leaves))
    sorted_leaves <- leaves[sorted_order, ]
    
    # Define private method  mask
    mask_method <- function() {
        return(leaves)
    }
    
    # Build a list contains original func & private method
    obj <- list(sorted_leaves = sorted_leaves, mask = mask_method)
    
    # Making private method invisible
    invisible(obj)
}

# Get the insurance_model's sorted leaf node information
sorted_leaves_insurance <- get_sorted_leaves(insurance_model)
print(sorted_leaves_insurance)

# Use sorted_leaves_insurance$mask() to get the  get_sorted_leaves 's leaves dataframe
leaves <- sorted_leaves_insurance$mask()
print(leaves)

# Get the  perturb_model's sorted leaf node information
sorted_leaves_perturb <- get_sorted_leaves(perturb_model)
perturb_leaves <- sorted_leaves_perturb$mask()

# write.csv(leaves, 'raw_data/CH5/ch5_leaves_model_info.csv', row.names=FALSE)
# write.csv(perturb_leaves, 'raw_data/CH5/ch5_perturb_leaves_model_info.csv', row.names = FALSE)

####################@code: Rule_induction_1############################
# Function Defination

rules <- rpart.rules(insurance_model) # return the leaf information directly
rules_df <- data.frame(rules, stringsAsFactors = FALSE );  

############################# 替代文字字串 Replace the specific string 
# ^XXX$ indicate thre 100% match condition
rules_df <- rules_df %>% 
    mutate_all(~ str_replace_all(., "^is$", "==")) %>% 
    mutate_all(~ str_replace_all(., "^no$", "FALSE")) %>% 
    mutate_all(~ str_replace_all(., "^yes$", "TRUE"))

# remove the DV in colnames ("Var.2" == when)
rules_fragments = rules_df[, colnames(rules_df) != "expenses" &  colnames(rules_df) != "Var.2"];



############重要!!!! 用TRAIN或是TEST當given instance在這邊改##########

concatenated_df <- data.frame(
    # 找出個leaf的index, 預測值, 將rule分割成塞入
    index =rownames(leaves),
    prediction = leaves$yval, 
    ruleset = apply(rules_fragments, 1, function(row) paste(row, collapse = " "))
); kable(concatenated_df)


leaf_bucket <- function(df, surrogate_model, iv_df, type= "vector" ){
    # Input df shall has at least 3 features [index, prediction, ruleset]
    # index is the node number of decision tree
    # prediction is the value of each leaf node
    # ruleset is the rule sequence from root to leaf as a string
    
    vector <-  predict(surrogate_model, newdata = iv_df, type = type); 
    for (i in 1:nrow(df)) {
        indivi_predicted <- as.numeric(df$prediction[i])
        index <- which(vector == indivi_predicted)
        df$includes_instance[i] <-  list(index)}
    return (df)
}


######TREEA instance插入改iv_df#####
insurance_bucket <- leaf_bucket(concatenated_df,surrogate_model = insurance_model,  iv_df = marbels , type = "vector")

# Get the rule and preprocesssed it
p_rules_df <- data.frame(rpart.rules(perturb_model), stringsAsFactors = FALSE ) 

# ^XXX$ indicate thre 100% match condition
p_rules_df <- p_rules_df %>%
    mutate_all(~ str_replace_all(., "^is$", "==")) %>%
    mutate_all(~ str_replace_all(., "^no$", "FALSE")) %>%
    mutate_all(~ str_replace_all(., "^yes$", "TRUE"))


# remove the DV in colnames ("Var.2" == when)
p_rules_fragments = p_rules_df[, colnames(p_rules_df) != "expenses" &  colnames(p_rules_df) != "Var.2"];
# p_rules_fragments


######TREEB#####
p_concatenated_df <- data.frame(
    index =rownames(perturb_leaves),
    prediction = perturb_leaves$yval,
    ruleset = apply(p_rules_fragments, 1, function(row) paste(row, collapse = " "))
);
p_concatenated_df

perturbated_bucket <- leaf_bucket(p_concatenated_df,surrogate_model = perturb_model,  iv_df = marbels , type = "vector")

# concatenation <- function(df){
#     df <- df %>% 
#         mutate_all(~ str_replace_all(., "^is$", "==")) %>% 
#         mutate_all(~ str_replace_all(., "^no$", "FALSE")) %>% 
#         mutate_all(~ str_replace_all(., "^yes$", "TRUE"))
#     rules_fragments = df[, colnames(df) != "expenses" &  colnames(df) != "Var.2"];
#     concatenated_df <- data.frame(
#         index =rownames(leaves),
#         prediction = leaves$yval, 
#         ruleset = apply(rules_fragments, 1, function(row) paste(row, collapse = " "))
#     )
#     return(concatenated_df)
# }
# leaf_bucket <- function(df, surrogate_model, iv_df, type= "vector" ){
#     vector <-  predict(surrogate_model, newdata = iv_df, type = type); 
#     for (i in 1:nrow(df)) {
#         indivi_predicted <- as.numeric(df$prediction[i])
#         index <- which(vector == indivi_predicted)
#         df$includes_instance[i] <-  list(index)}
#     return (df)
# }
# 
# # Tree A
# rules_df <- data.frame(rpart.rules(insurance_model), stringsAsFactors = FALSE );  
# concatenated_df <- concatenation(rules_df)
# insurance_bucket <- leaf_bucket(concatenated_df,surrogate_model = insurance_model,  iv_df = Train_X , type = "vector")
# 
#  
# # Tree B
# # Return the leaf information directly
# p_rules_df <- data.frame(rpart.rules(perturb_model), stringsAsFactors = FALSE )
# p_concatenated_df <- concatenation(p_rules_df)
# perturbated_bucket <- leaf_bucket(p_concatenated_df,surrogate_model = perturb_model,  iv_df = perturbed_Train_X , type = "vector")
# 

###@code: Rule_induction_2
### For the rulset comparison, the string need further regularized as following approach.
remove_spaces_in_column <- function(df, column_name) {
    # rule split based on '&' sign and remove the space
    split_strings <- strsplit(df[[column_name]], "&")
    split_strings <- lapply(split_strings, function(x) gsub(" ", "", x))
    df[[column_name]] <- split_strings
    return(df)
}

insurance_bucket1 <- remove_spaces_in_column(insurance_bucket, "ruleset")
perturbated_bucket1 <- remove_spaces_in_column(perturbated_bucket, "ruleset")


rule_set_TreeA_csv <- insurance_bucket1
rule_set_TreeB_csv <- perturbated_bucket1
rule_set_TreeA_csv$ruleset <- sapply(rule_set_TreeA_csv$ruleset, paste, collapse = " -->")
rule_set_TreeA_csv$includes_instance <- sapply(rule_set_TreeA_csv$includes_instance, paste, collapse = "、")
rule_set_TreeB_csv$ruleset <- sapply(rule_set_TreeB_csv$ruleset, paste, collapse = " -->")
rule_set_TreeB_csv$includes_instance <- sapply(rule_set_TreeB_csv$includes_instance, paste, collapse = "、")
# write.csv(rule_set_TreeA_csv, 'raw_data/CH5/rule_set_TreeA.csv', row.names=FALSE)
# write.csv(rule_set_TreeB_csv, 'raw_data/CH5/rule_set_TreeB.csv', row.names=FALSE)

#################################################################
check_pattern <- function(string, letters =  c("age", "gender", "children"), equal_sign= c('==', '>=', '<=')) {
    # ---------------------------------------------------------------
    # INSTRUCTION:
    # The purpose of this function is to detect the rule fragment like "age==52to59" then transform it into "52<=age<=59", it return True as input string match the conditions otherwise return False
    # ---------------------------------------------------------------
    # INPUT:
    # string is the splitting rule fregment
    # letters is the target features name, default  c('age', 'gender')
    # equal_sign is the legit symbol you want to test, default ('==', '>=', '<=')
    # it return True as It match the conditions otherwise False
    # TEST data under default setting: "age==52to59"----> TRUE// "children==1to2"----> FALSE// "age<=39" ----> FALSE
    # ---------------------------------------------------------------
    pattern <- paste0("^(", paste(letters, collapse = "|"), ")")
    
    if (grepl(pattern, string)) {
        start_index <- regexpr(pattern, string)[1] + attr(regexpr(pattern, string), "match.length")
        substring <- substr(string, start = start_index, stop = nchar(string))
        
        numeric_pattern <- "\\d+to\\d+"
        if (grepl(paste0("^", equal_sign, collapse = "|"), substring) && grepl(numeric_pattern, substring, perl = TRUE)) {
            return(TRUE)
        }
    }
    
    return(FALSE)
}

rule_regulate <- function(df, column_name = "ruleset") {
    # This function can translate the rule fragment in the column ruleset(default) one by one. Notice that the element of ruleset column is string list.
    # e.g "age==52to59"  to "52<=age<=59" 
    target_col <- df[[column_name]]
    filtered_list <- lapply(target_col, function(rule) {
        lapply(rule, check_pattern)
    })
    for (i in 1:length(filtered_list)) {
        for (j in 1:length(filtered_list[[i]])) {
            if (filtered_list[[i]][[j]]) {
                convert_string <- function(str) gsub("age==([0-9]+)to([0-9]+)", "\\1<=age<=\\2", str)
                target_col[[i]][[j]] <- convert_string(target_col[[i]][[j]])
            }
        }
    }
    df[[column_name]] <- target_col
    return(df)
}



insurance_bucket2 <- rule_regulate(insurance_bucket1, "ruleset")
perturbated_bucket2 <- rule_regulate(perturbated_bucket1, "ruleset")


##################################################################
##1. EASYMAP無效
##2. 
DMC_similarity <- function(df1, df2, field, index = "index", selective_df = "d1", heapmap_sorted = "pred1&pred2") {
    ##############################
    # Formal version:
    # Both the input dataframe shall have at least the cols below:
    # ----index <chr> | ruleset <list> | includes_instance <list>----
    # >>index: It shall be the leaf node number derived from rpart package
    # >>ruleset: the ruleset is the rule from root to the leaf node, which been proper text mining process.
    # >>includes_instance: shall be a integer  list, the unique integer represent the instances in that leaf node.
    # Notice that the df1, df2 shall comes from similar source data being proper perturbation or other noise-adding approach for other purpose.
    # : --------------- Input Parameter: -----------
    # df1, df2: dataframe
    # field: defaulted  as  "includes_instance". It shall be pointed to the instance storage column.
    # selective_df: It can  be "d1"(defaulted) or "d2".It start from the leaf nodes of df1 as the "d1" is assigned. Currently, it only affect the optimal_similarity. 
    # --------------- method -----------
    # similarity_matrix | string_df | optimal_pair| unique_pair
    ##############################
    library(dplyr)
    library(magrittr)
    library(pheatmap)
    library(dtw)
    ##############################
    # Create empty dataframe
    string_df <- data.frame()
    # Create an empty similarity matrix
    similarity_matrix <- matrix(0, nrow = nrow(df1), ncol = nrow(df2),
                                dimnames = list(df1[[index]], df2[[index]]))
    
    
    # define jaccard similarity formula
    jaccard_similarity <- function(x, y) {
        x_sorted <- sort(x)
        y_sorted <- sort(y)
        
        intersection <- length(intersect(x_sorted, y_sorted))
        union <- length(union(x_sorted, y_sorted))
        if (union == 0) {
            return(0)  # 如果聯集為空集，則相似度為0
        } else {
            return(intersection / union)
        }
    }
    
    # 比較兩個資料框的每一列
    for (i in 1:nrow(df1)) {
        for (j in 1:nrow(df2)) {
            # 比較includes_instance欄位的Jaccard相似度
            vector_of_leaf_number_df1 <-  df1[[field]][[i]];
            vector_of_leaf_number_df2 <- df2[[field]][[j]];
            similarity <- jaccard_similarity(vector_of_leaf_number_df1, vector_of_leaf_number_df2)
            # 創建索引字串
            index_str <- paste(deparse(substitute(df1)), "$", index, "==", df1[[index]][i], "&",
                               deparse(substitute(df2)), "$", index, "==", df2[[index]][j], sep = "")
            len_vec1 <- length(vector_of_leaf_number_df1); 
            len_vec2 <- length(vector_of_leaf_number_df2);
            union_len <- length(union(vector_of_leaf_number_df1, vector_of_leaf_number_df2))
            
            # 將結果加入到資料框中
            string_df <- rbind(string_df, data.frame(index1 = df1[[index]][i],
                                                     index2 = df2[[index]][j],
                                                     index_comparison = index_str,
                                                     similarity = similarity,
                                                     len_vec1 = len_vec1,
                                                     len_vec2 = len_vec2,
                                                     union_len = union_len)
            )
            similarity_matrix[i, j] <- similarity
        }
    }
    
    optimal_matching <- function(df, select = "d1") {
        if (select %in% c("d1", "d2")) {
            index_column <- ifelse(select == "d1", "index1", "index2")
            duplicated_df <- df[duplicated(df[[index_column]]) | duplicated(df[[index_column]], fromLast = TRUE), ]
            
            print(duplicated_df)
            optimal_pair <- duplicated_df %>%
                group_by_at(index_column) %>%
                slice(which.max(similarity)) %>%
                ungroup()
            return(optimal_pair)
        } else {
            stop("select parameter can only be 'd1' or 'd2' in string format.")
        }
    }
    unique_matching <- function(df){
        # Using dynamic programming for implementation
        idx1 <- df$index1[1]
        max_similarity <- -1
        max_similarity_index2 <- -1
        similarity_list <- c()
        result_df <- data.frame()
        for (i in 1:nrow(df)) {
            current_index1 <- df$index1[i]
            current_index2 <- df$index2[i]
            current_similarity <- df$similarity[i]
            if (current_index1 != idx1) {
                similarity_list[max_similarity_index2] <- max_similarity
                result_row <- data.frame(index1 = idx1,
                                         index2 = max_similarity_index2,
                                         similarity = max_similarity,
                                         stringsAsFactors = FALSE)
                result_df <- rbind(result_df, result_row)
                idx1 <- current_index1
                max_similarity <- -1
                max_similarity_index2 <- -1
            }
            if (current_similarity > max_similarity && !(current_index2 %in% names(similarity_list))) {
                max_similarity <- current_similarity
                max_similarity_index2 <- current_index2}
        }
        # Add the last set of values outside the loop
        similarity_list[max_similarity_index2] <- max_similarity
        result_row <- data.frame(index1 = idx1,
                                 index2 = max_similarity_index2,
                                 similarity = max_similarity,
                                 stringsAsFactors = FALSE)
        result_df <- rbind(result_df, result_row)
        return(result_df)
    }
    
    
    sort_machine <- function(sim_array, sorted) {
        df <- as.data.frame(sim_array)
        col_names <- colnames(sim_array)
        row_names <- rownames(sim_array)
        sort_appraoch = "pred1&pred2"
        valid_types <- c("index1", "index2", "Both", "None", "pred1", "pred2", "pred1&pred2")
        if (!sorted %in% valid_types) { stop("Invalid sorted setting. Please choose from: index1, index2, Both, None, pred1, pred2, pred1&pred2")}
        pred_order1 <- df1[order(df1$prediction), ]$index
        pred_order2 <- df1[order(df2$prediction), ]$index
        switch(
            sorted,
            "index1" = {
                # situation 1
                cat("Heapmap sorted by index 1 leaf Number ascending")
                sort_appraoch = "index1"
                df <- df[order(as.numeric(rownames(df))), ]
            }, 
            "index2" = {
                # situation 2
                cat("Heapmap sorted by index 2 leaf Number ascending")
                sort_appraoch = "index2"
                df <- df[, order(as.numeric(colnames(df)))]
            },
            "Both" = {
                # situation 3
                cat("Heapmap sorted by both index leaf Number ascending")
                sort_appraoch = "Both"
                df <- df[order(as.numeric(row.names(df))), order(as.numeric(colnames(df)))]
            },
            "None" = {
                # situation 4
                cat("Unsorted heatmap")
                sort_appraoch = "None"
                index_col <- order(as.integer(col_names))
                index_row <- order(row_names)
                shuffled_col <- index_col[sample(length(index_col))]
                shuffled_row <- index_row[sample(length(index_row))]
                sorted_matrix <- sim_array[, order(as.integer(shuffled_col))][order(shuffled_row), ]
                df <- as.data.frame(sorted_matrix)
            },
            "pred1&pred2" = {
                # situation 5
                cat("Heapmap sorted by the prediction value of leaf node of dt1 & dt2", "\n")
                # 將列(Tree 1 nodes)使用dt1的預測值由小到大順序排序。將欄(Tree 2 nodes)使用dt2的預測值由"大到小" (越大的排在越上面的row)順序排序
                ###############################################
                sort_appraoch = "pred1&pred2"
                col_names <- colnames(sim_array); 
                row_names <- row.names(sim_array); 
                df[ order(as.numeric(row.names(df))), ]
                col_sorted <- sim_array[,order(as.integer(col_names))]
                all_sorted <- col_sorted[order(as.integer(row_names), decreasing = T), ]
                df <- as.data.frame(all_sorted)
            },
            "pred1" = {
                # situation 6
                cat("Heapmap sorted by the prediction value of leaf node of dt1", "\n")
                sort_appraoch = "pred1"
                df <- df[order(match(rownames(df), pred_order1)), ]
            },
            "pred2" = {
                # situation 7
                cat("Heapmap sorted by the prediction value of leaf node of dt2","\n")
                sort_appraoch = "pred2"
                df <- df[, order(match(colnames(df, decreasing = T), pred_order2))]
            },
            {# Default
                stop("None of the options matched")
            })
        
        return(list(
            sorted_df = df,
            sorted_matrix = as.matrix(df),
            sort_approach = sort_appraoch,
            col_sorted = col_sorted)
            )
    }
    heapmap_generator <- function(matric, title = "Heatmap") {
        color_set <- colorRampPalette(brewer.pal(n = 7, name = "RdYlBu"))(100)
        string_list <- as.character(seq(min(matric), max(matric), by = (max(matric)-min(matric))/4))
        pheatmap::pheatmap(matric, 
                                        main = title, 
                                        legend_labels = string_list,
                                        legend = TRUE,
                                        cluster_rows = FALSE,
                                        cluster_cols = FALSE
                                        )
    }
    
    dtw_visual <- function(df1,df2, plot = "TRUE"){
        # https://rstudio-pubs-static.s3.amazonaws.com/474160_0761428e8683401e941a92a4177254a4.html
        sorted_pred_df1 <- df1[order(df1$prediction),c("index", "prediction")]
        sorted_pred_df2 <- df2[order(df2$prediction),c("index", "prediction")]
        
        # DTW Part 
        #####why symmetric2 as path strategy#############
        # We choose symmetric2 due to it's a widely used and often the default choice.
        # It's Normalizable, symmetric pattern with no local slope constraints, which allow the  sequences with different length can be compare with each other after normalized by dividing by the sum of the query and reference lengths (N+M)..  it's suitable for general matching problems.
        # Diagonal steps have the same cost as the two equivalent steps along the sides.
        # Widely used and often the default choice for DTW implementations.
        # Suitable for general matching problems.
        # Based on https://cran.r-project.org/web/packages/dtw/dtw.pdf (p.26)
        df1_pred <- as.numeric(sorted_pred_df1$prediction) # must be float
        df2_pred <- as.numeric(sorted_pred_df2$prediction)
        model <- dtw(df1_pred, df2_pred, keep = TRUE, step = symmetric2)
        
        pred_value_df1 <- model$query
        pred_value_df2 <- model$reference
        accum_sequence_length <- length(pred_value_df1)+length(pred_value_df2)
        index_x <- model$index1; 
        index_y <- model$index2;
        
        if(plot == "TRUE"){
            plot_types <- c("twoway", "threeway")
            for (plot_type in plot_types) {
                #plot.new()  # Create new plot window
                plot(model, main = paste("DTW", plot_type, "Plot (symmetric2)"), 
                     xlab = "Tree1 (index1)", 
                     ylab = "Tree2 (index2)", 
                     xaxp = c(min(index_x), 
                              max(index_x), 1), 
                     yaxp = c(min(index_y), 
                              max(index_y), 1), 
                     type = plot_type, 
                     col = c('blue', 'magenta')) } 
        }
        
        ### TRANSPOSE COST MATRIX
        normalized_matrix <- model$costMatrix/ accum_sequence_length
        reverse_matrix <- (max(normalized_matrix) + min(normalized_matrix)) - normalized_matrix
        normalized_reverse_matrix <- reverse_matrix/max(normalized_matrix)
        if(plot == "TRUE"){
            heapmap_generator(model$costMatrix, "DTW CostMatrix");
            heapmap_generator(normalized_matrix, "DTW Normalized CostMatrix");
            heapmap_generator(normalized_reverse_matrix, "DTW Normalized Reverse CostMatrix");
        }
        return (list(model = model,
                     LocalCostMatrix = model$localCostMatrix,
                     CostMatrix = model$costMatrix,
                     NormalizedMatrix = normalized_matrix,
                     RreverseSimilarityMatrix = normalized_reverse_matrix,
                     distance = model$distance,
                     index_df = data.frame(index_x =index_x, index_y = index_y)
        )
        )
    }
    
    # magic_func <- function (df1, df2){
    #     # OUTPUT 
    #     # index.x 代表index1的leaf node number, index.y 代表index2的leaf node number (皆為character)
    #     # index_x代表dtw產生的binding序號, 與同列對應的index_y為一對。例如=，(1,1), (2,2), (3,3)
    #     #     index_x index_y index.x pred_index_x index.y pred_index_y Jaccard_Sim
    #     # 1       1       1       4    5488.219       4    5578.496        xxx
    #     # 2       2       2       5   12394.247       5   12473.034        xxx
    #     # 3       3       3       6   21420.946       6   20819.181        xxx
    #     # 4       3       4       6   21420.946       6   20819.181        xxx
    #     # 5       4       5       7   41560.331      14   34222.699        xxx
    #     dtw_binding <- dtw_visual(df1, df2,  "FALSE")$index_df # pair of dtw 
    #     # 1. 從insurance_bucket2中取出c("index", "prediction")兩個欄位，需要排序由predicted value由小到大
    #     ins_dict <- df1[order(df1$prediction),c("index", "prediction", "ruleset")]
    #     pert_dict <- df2[order(df2$prediction),c("index", "prediction", "ruleset")]
    #     ## 改名ins_dict方便等等join
    #     names(ins_dict)[names(ins_dict) == "prediction"] <- "pred_index_x"
    #     names(ins_dict)[names(ins_dict) == "ruleset"] <- "ruleset_index_x" 
    #     
    #     ## 改名pert_dict方便等等join
    #     names(pert_dict)[names(pert_dict) == "prediction"] <- "pred_index_y"
    #     names(pert_dict)[names(pert_dict) == "ruleset"] <- "ruleset_index_y"
    #     
    #     #######  2. 為其新增欄位 稱作key_index，用來對應dtw自動產生之index
    #     ins_dict["key_index"] <- 1:nrow(ins_dict)
    #     pert_dict["key_index"] <- 1:nrow(pert_dict)
    #     new_dtw_binding <- merge(dtw_binding, ins_dict,  by.x = "index_x", by.y = "key_index", all.x = TRUE)
    #     new_dtw_binding <- merge(new_dtw_binding, pert_dict,  by.x = "index_y", by.y = "key_index", all.x = TRUE)
    #     
    #     ############## 3. 雙重join
    #     new_dtw_binding <- merge(dtw_binding, ins_dict,  by.x = "index_x", by.y = "key_index", all.x = TRUE)
    #     new_dtw_binding <- merge(new_dtw_binding, pert_dict,  by.x = "index_y", by.y = "key_index", all.x = TRUE)
    #     temp <- c()
    #     for(i in 1:nrow(new_dtw_binding)){
    #         leafnode_index1 <- as.character(new_dtw_binding$index.x[i])
    #         leafnode_index2 <- as.character(new_dtw_binding$index.y[i])
    #         target_ele <- similarity_matrix[leafnode_index1,leafnode_index2 ]
    #         temp <- c(temp, target_ele)
    #     }
    #     new_dtw_binding[['Jaccard_Sim']] = temp
    #     average_jaccard_sim_dtw_binding <-  sum(new_dtw_binding$Jaccard_Sim)/ nrow(new_dtw_binding)
    #     ###### dtw_pair_with_rule
    #     dtw_pair_with_rule <- new_dtw_binding
    #     dtw_pair_with_rule$ruleset_index_x <- sapply(dtw_pair_with_rule$ruleset_index_x, paste, collapse = " --> ")
    #     dtw_pair_with_rule$ruleset_index_y <- sapply(dtw_pair_with_rule$ruleset_index_y, paste, collapse = " --> ")
    #     
    #     # sort by Jaccard_Sim from highest to lowest, and rearrange the column by it's colname
    #     df_sorted <- dtw_pair_with_rule[order(dtw_pair_with_rule$Jaccard_Sim, decreasing = TRUE), order(names(dtw_pair_with_rule))]
    #     
    #     
    #     return(list(new_dtw_binding = new_dtw_binding, 
    #                 average_jaccard_sim_dtw_binding = average_jaccard_sim_dtw_binding,
    #                 dtw_pair_with_rule = df_sorted
    #     ))
    # }
    
    magic_func <- function (df1, df2){
        # OUTPUT 
        # index.x 代表index1的leaf node number, index.y 代表index2的leaf node number (皆為character)
        # index_x代表dtw產生的binding序號, 與同列對應的index_y為一對。例如=，(1,1), (2,2), (3,3)
        #     index_x index_y index.x pred_index_x index.y pred_index_y Jaccard_Sim
        # 1       1       1       4    5488.219       4    5578.496        xxx
        # 2       2       2       5   12394.247       5   12473.034        xxx
        # 3       3       3       6   21420.946       6   20819.181        xxx
        # 4       3       4       6   21420.946       6   20819.181        xxx
        # 5       4       5       7   41560.331      14   34222.699        xxx
        dtw_binding <- dtw_visual(df1, df2,  "FALSE")$index_df # pair of dtw 
        # 1. 從insurance_bucket2中取出c("index", "prediction")兩個欄位，需要排序由predicted value由小到大
        ins_dict <- df1[order(df1$prediction),c("index", "prediction", "ruleset")]
        pert_dict <- df2[order(df2$prediction),c("index", "prediction", "ruleset")]
        ## 改名ins_dict方便等等join
        names(ins_dict)[names(ins_dict) == "prediction"] <- "pred_index_x"
        names(ins_dict)[names(ins_dict) == "ruleset"] <- "ruleset_index_x" 
        
        ## 改名pert_dict方便等等join
        names(pert_dict)[names(pert_dict) == "prediction"] <- "pred_index_y"
        names(pert_dict)[names(pert_dict) == "ruleset"] <- "ruleset_index_y"
        
        #######  2. 為其新增欄位 稱作key_index，用來對應dtw自動產生之index
        ins_dict["key_index"] <- 1:nrow(ins_dict)
        pert_dict["key_index"] <- 1:nrow(pert_dict)
        new_dtw_binding <- merge(dtw_binding, ins_dict,  by.x = "index_x", by.y = "key_index", all.x = TRUE)
        new_dtw_binding <- merge(new_dtw_binding, pert_dict,  by.x = "index_y", by.y = "key_index", all.x = TRUE)
        
        ############## 3. 雙重join
        new_dtw_binding <- merge(dtw_binding, ins_dict,  by.x = "index_x", by.y = "key_index", all.x = TRUE)
        new_dtw_binding <- merge(new_dtw_binding, pert_dict,  by.x = "index_y", by.y = "key_index", all.x = TRUE)
        temp <- c()
        for(i in 1:nrow(new_dtw_binding)){
            leafnode_index1 <- as.character(new_dtw_binding$index.x[i])
            leafnode_index2 <- as.character(new_dtw_binding$index.y[i])
            target_ele <- similarity_matrix[leafnode_index1,leafnode_index2 ]
            temp <- c(temp, target_ele)
        }
        new_dtw_binding[['Membership_Similarity']] = temp
        average_jaccard_sim_dtw_binding <-  sum(new_dtw_binding$Membership_Similarity)/ nrow(new_dtw_binding)
        ###### dtw_pair_with_rule
        dtw_pair_with_rule <- new_dtw_binding
        dtw_pair_with_rule$ruleset_index_x <- sapply(dtw_pair_with_rule$ruleset_index_x, paste, collapse = " --> ")
        dtw_pair_with_rule$ruleset_index_y <- sapply(dtw_pair_with_rule$ruleset_index_y, paste, collapse = " --> ")
        
        # sort by Membership_Similarity from highest to lowest, and rearrange the column by it's colname
        df_sorted <- dtw_pair_with_rule[order(dtw_pair_with_rule$Membership_Similarity, decreasing = TRUE), order(names(dtw_pair_with_rule))]
        
        # Rename the outcome (這是該dataframe的最後一步)
        rename_columns <- function(df) {
            df <- df %>% 
                rename(DTW_no_Tree1 = index_x,
                       DTW_no_Tree2 = index_y,
                       leaf_no_Tree1 = index.x,
                       leaf_no_Tree2 = index.y,
                       
                       Prediction_Tree1 = pred_index_x,
                       Prediction_Tree2 = pred_index_y, 
                       Ruleset_Tree1 = ruleset_index_x, 
                       Ruleset_Tree2 = ruleset_index_y, 
                )
            return(df)
        }
        df_sorted <- rename_columns(df_sorted)
        
        return(list(new_dtw_binding = new_dtw_binding, 
                    average_jaccard_sim_dtw_binding = average_jaccard_sim_dtw_binding,
                    dtw_pair_with_rule = df_sorted
        ))
    }
    
    ################
    dtw_binding_sim <- magic_func(df1, df2)
    
    # Convert the similarity matrix to a dataframe
    similarity_df <- as.data.frame(similarity_matrix, select_df = "len_vec1")
    
    # Optimal_pair
    optimal_pair <- optimal_matching(string_df,  select = selective_df)
    
    avg_sim <- function(binding_df){
        sim <- binding_df$similarity
        avg_unique_sim <- round(sum(sim) / length(sim),3)
        return(avg_unique_sim)
    }
    opt_avg_sim <- avg_sim(optimal_pair)
    # Unique_pair
    unique_df <-  unique_matching(string_df)
    unique_pair <- unique_df[order(unique_df$index1),]
    ######## UNIQUE (BLOCKED)######
    
    sim <- unique_pair$similarity
    avg_unique_sim <- round(sum(sim) / length(sim),3)
    
    # Heapmap generator
    sorted_data <- sort_machine(similarity_matrix, sorted = heapmap_sorted)
    if (heapmap_sorted=="None"){
        sorted_heatmap <- heapmap_generator(sorted_data$sorted_matrix, title = "Unsorted Heatmap")
    } else{
        sorted_heatmap <- heapmap_generator(sorted_data$sorted_df, title = paste("Heatmap sorted by ",heapmap_sorted ))
    }
    

    
    return(list(similarity_matrix = similarity_matrix, 
                string_df = string_df, 
                optimal_pair = list(details = optimal_pair,average = opt_avg_sim),
                unique_pair = unique_pair,
                heapmap = sorted_heatmap,
                sorted_data = sorted_data,
                sorted_df = round(sorted_data$sorted_df, 3),
                sorted_matrix  = round(sorted_data$sorted_matrix, 3),
                dtw_visual = dtw_visual(df1, df2, plot= TRUE),
                dtw_binding_sim = dtw_binding_sim
    ))
}


## Unsorted
# membership_comparison_none <- DMC_similarity(insurance_bucket2, perturbated_bucket2, "includes_instance", index = "index", heapmap_sorted = "None")
# kable(round(membership_comparison_none$similarity_matrix, 3))
# membership_comparison_none$string_df
# membership_comparison_none


## Sorted Membership Sim figure
membership_comparison <- DMC_similarity(insurance_bucket2, perturbated_bucket2, "includes_instance", index = "index", heapmap_sorted ="pred1&pred2")


membership_comparison$dtw_binding_sim

membership_comparison$dtw_binding_sim$average_jaccard_sim_dtw_binding
membership_comparison$dtw_binding_sim

Sorted_round_MB_Sim <- as.data.frame(membership_comparison$sorted_matrix)
# write.csv(Sorted_round_MB_Sim, 'raw_data/CH5/ch5_Sorted_round_MB_Sim.csv', row.names=TRUE)
#########################
csv_dtw_leaf_pairs <- membership_comparison$dtw_binding_sim$dtw_pair_with_rule
# write.csv(csv_dtw_leaf_pairs, 'raw_data/CH5/ch5_dmc_alignment_conditions.csv', row.names=FALSE)



##########################Test I reverse the reference and 

# membership_comparison_Reverse <- DMC_similarity(perturbated_bucket2, insurance_bucket2, "includes_instance", index = "index", heapmap_sorted ="pred1&pred2")
# csv_dtw_leaf_pairs_Reverse <- membership_comparison$dtw_binding_sim$dtw_pair_with_rule
# 
# write.csv(csv_dtw_leaf_pairs_Reverse, 'raw_data/CH5/ch5_Reverse_dmc_alignment_conditions.csv', row.names=FALSE)

