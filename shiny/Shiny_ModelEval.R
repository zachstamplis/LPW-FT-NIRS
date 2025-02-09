# Load packages
packages <- c("caret", "devtools", "DT", "dplyr", "gglm", "ggplot2", "gratia", "knitr", "mdatools", "mgcv", "MuMIn","shiny","shinyWidgets")
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  utils::install.packages(pkgs = packages[!installed_packages])
}
invisible(lapply(packages, library, character.only = TRUE)) # load all packages in list
rm(installed_packages, packages) # remove objects from environment

# Functions to run all models, extract models and create model summary tables
run_models <- function(mydf){
df <- {{mydf}}
df <- df[complete.cases(df$read_age), ] 
pca_temp <- pca(df[, 21:ncol(df), ])
pc_df <- data.frame(PC1 = rep(0,nrow(df)))
for (i in 1:10) {
  pc_df[, paste0("PC", i)] <- pca_temp$res$cal$scores[, i]
  rm(i)
}
pc_df <- cbind(pc_df,df) 
global_lm <- lm(data = pc_df, read_age ~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10)
global_gam <- gam(data = pc_df, read_age ~ s(PC1) + s(PC2) + s(PC3) + s(PC4) + s(PC5) + s(PC6)) # adding too many PC's makes GAM have horrible fit and R2

# dredge to find top 5 models
options(na.action = "na.fail")
dredge_lm <- dredge(global_lm)
top5_lm <- get.models(dredge_lm, subset = 1:5)

# get.models was not working for gam in a function format so had to adapt this insane code to extract the top models....
dredge_gam <- dredge(global_gam)
top5_gam_temp <- dredge_gam[1:5,] 
reconstruct_formula <- function(model_row) {
  terms_used <- names(model_row)[!is.na(model_row)]   # Extract the terms used in the model
  terms_used <- terms_used[!terms_used %in% c("(Intercept)", "df", "logLik", "AICc", "delta", "weight")]   # Remove non-predictor columns (e.g., AIC, delta, weight)
  as.formula(paste("read_age ~", paste(terms_used, collapse = " + ")))   # Construct the formula
}
top5_gam <- lapply(1:5, function(i) {reconstruct_formula(top5_gam_temp[i, ])})
rm(top5_gam_temp, reconstruct_formula)

# create lists for top 5 models of lm and gam
terms_lm <- list()
terms_gam <- list()
for(i in 1:5){
  terms_lm[[i]] <- top5_lm[[i]]$terms
  terms_gam[[i]] <- top5_gam[[i]]
  rm(i)
}
rm(global_gam, global_lm, top5_gam, top5_lm, pc_df, pca_temp, dredge_gam,dredge_lm)

# 10 fold CV split - create folds
set.seed(6)
splits <- caret::createFolds(df$read_age, k = 10, list = TRUE, returnTrain = FALSE)
# extract PC's for each calibration set, create test sets with ages and spectra
cal <- list()
test <- list()
for (i in 1:10) {
  # calibration set and PC's
  pc.mod <- preProcess(df[-splits[[i]], -c(1:20)], method = c("pca","center"), pcaComp = 10)
  pc.cal <- predict(pc.mod, df[-splits[[i]], -c(1:20)])
  pc.cal <- cbind(pc.cal, df[-splits[[i]], ])
  cal[[i]] <- pc.cal
  rm(pc.cal)
  # test sets
  pc.test <- predict(pc.mod, df[splits[[i]], -c(1:20)])
  pc.test <- cbind(pc.test, df[splits[[i]], ])
  test[[i]] <- pc.test
  rm(pc.test, pc.mod, i)
}

mods.lm <- replicate(10, list())
r2_lm <- list()
rmse_lm <- list()
for (i in 1:10) {
  calibrate <- cal[[i]]
  testing <- test[[i]]
  r2_temp <- rep(0, 5)
  rmse_temp <- rep(0, 5)
  for (j in 1:5) {
    mod <- lm(data = calibrate, terms_lm[[j]])
    preds <- predict(mod, newdata = testing)
    rmse_temp[j] <- caret::RMSE(pred = preds, obs = testing[, "read_age"])
    RSS <- sum((testing$read_age - preds)^2)
    TSS <- sum((testing$read_age - mean(testing$read_age))^2)
    r2_temp[j] <- 1 - (RSS / TSS)
    mods.lm[[i]][[j]] <- mod
    rm(RSS, TSS, preds)
  }
  r2_lm[[i]] <- data.frame(Model = paste0("Linear ", 1:5), r2 = r2_temp)
  rmse_lm[[i]] <- data.frame(Model = paste0("Linear ", 1:5), rmse = rmse_temp)
  rm(mod, i, j, r2_temp, rmse_temp, calibrate, testing)
}
# Unlist the r2 & rmse values and calc means for 10 splits, combine into dataframe
r2_lm <- sapply(r2_lm, "[[", "r2")
r2_lm <- rowMeans(r2_lm)
rmse_lm <- sapply(rmse_lm, "[[", "rmse")
rmse_lm <- rowMeans(rmse_lm)
(lm_result <- data.frame(
  Model = paste0("Linear ", 1:5),
  Mean_R2 = r2_lm,
  Mean_RMSE = rmse_lm
))
rm(r2_lm, rmse_lm)

mods.gam <- replicate(10, list())
r2_gam <- list()
rmse_gam <- list()
for (i in 1:10) {
  calibrate <- cal[[i]]
  testing <- test[[i]]
  r2_temp <- rep(0, 5)
  rmse_temp <- rep(0, 5)
  for (j in 1:5) {
    mod <- gam(data = calibrate, terms_gam[[j]])
    preds <- predict(mod, newdata = testing)
    rmse_temp[j] <- caret::RMSE(pred = preds, obs = testing[, "read_age"])
    RSS <- sum((testing$read_age - preds)^2)
    TSS <- sum((testing$read_age - mean(testing$read_age))^2)
    r2_temp[j] <- 1 - (RSS / TSS)
    mods.gam[[i]][[j]] <- mod
    rm(RSS, TSS, preds)
  }
  r2_gam[[i]] <- data.frame(Model = paste0("GAM ", 1:5), r2 = r2_temp)
  rmse_gam[[i]] <- data.frame(Model = paste0("GAM ", 1:5), rmse = rmse_temp)
  rm(j, i, testing, calibrate, mod, r2_temp, rmse_temp)
}
r2_gam <- sapply(r2_gam, "[[", "r2")
r2_gam <- rowMeans(r2_gam)
rmse_gam <- sapply(rmse_gam, "[[", "rmse")
rmse_gam <- rowMeans(rmse_gam)
gam_result <- data.frame(
  Model = paste0("GAM ", 1:5),
  Mean_R2 = r2_gam,
  Mean_RMSE = rmse_gam
)
(results <- rbind(lm_result,gam_result))
rm(r2_gam, rmse_gam, lm_result, gam_result)

mods.pls <- list()
mods.vip <- list()
r2.pls <- list()
rmse.pls <- list()
for (i in 1:10) {
  calibrate <- df[-splits[[i]], ]
  testing <- df[splits[[i]], ]
  mod <- pls(calibrate[, 31:ncol(calibrate)], calibrate[, "read_age"],
             scale = F, center = T, cv = 1, #ncomp.selcrit = "wold", removing for now, seems too conservative with number of selected components
             info = "Age Prediction Model",
             x.test = testing[, 31:ncol(testing)],
             y.test = testing[, "read_age"]
  )
  ncomp <- mod$ncomp.selected
  rmse.pls$pls.test[i] <- mod$testres$rmse[[ncomp]] # extract rmse
  r2.pls$pls.test[i] <- mod$testres$r2[[ncomp]] # extract r2
  
  # VIP filter of wavenumbers
  mods.pls[[i]] <- mod
  mods.pls[[1]]$ncomp.selected
  vip <- as.data.frame(vipscores(mod))
  mod <- pls(calibrate[, 31:ncol(calibrate)], calibrate[, "read_age"],
             # ncomp.selcrit = "wold", 
             scale = F, center = T, cv = 1,
             info = "Age Prediction Model",
             x.test = testing[, 31:ncol(testing)],
             y.test = testing[, "read_age"],
             exclcols = vip$V1 < 0.5
  )
  ncomp <- mod$ncomp.selected
  r2.pls$vip.test[i] <- mod$testres$r2[[ncomp]] # extract r2
  mods.vip[[i]] <- mod
  rmse.pls$vip.test[i] <- mod$testres$rmse[[ncomp]] # extract rmse
  rm(calibrate, mod, ncomp, testing, i, vip)
}
(results <- rbind(results, 
                  c("PLS", mean(r2.pls$pls.test), mean(rmse.pls$pls.test)),
                  c("VIP", mean(r2.pls$vip.test), mean(rmse.pls$vip.test))
))
rm(rmse.pls, r2.pls)
mods.lm <<- mods.lm
mods.gam <<- mods.gam
mods.pls <<- mods.pls
mods.vip <<- mods.vip
return(results)
}
get.my.models <- function() {
  model.list <- list(lm = list(), gam = list(), pls = list(), vip = list())
  model.list$lm <- mods.lm
  model.list$gam <- mods.gam
  model.list$pls <- mods.pls
  model.list$vip <- mods.vip
  rm(mods.lm, mods.gam, mods.pls, mods.vip, pos = .GlobalEnv)
  return(model.list)
}

# Including all wavenumbers, no outliers removed
results_allwaves <- readRDS("shiny/results_allwaves.RDS")
mods_allwaves <-readRDS("shiny/mods_allwaves.RDS")

# Excluding wavenumbers > 7500, no outliers removed
results_filtered <- readRDS("shiny/results_filtered.RDS")
mods_filtered <-readRDS("shiny/mods_filtered.RDS")

# All wavenumbers, remove specimen #77 (very large)
results_allwaves_no77 <- readRDS("shiny/results_allwaves_no77.RDS")
mods_allwaves_no77 <-readRDS("shiny/mods_allwaves_no77.RDS")

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
        #formula_info {
            padding: 5px;
            font-size: 0.9em;
            line-height: 1.2;
            max-height: 100px;
            overflow-y: auto;
            margin: 0;
        }
    "))
  ),
  fluidRow(
    column(3,
           selectInput(inputId = "results_list",
                       label = "Select Results List:",
                       choices = NULL),
           
           radioButtons(inputId = "model_type",
                        label = "Select Model Type:",
                        choices = c("Linear Models" = "lm",
                                    "GAM Models" = "gam",
                                    "PLS Models" = "pls",
                                    "PLS with VIP" = "vip")),
           
           conditionalPanel(
             condition = "input.model_type == 'lm' || input.model_type == 'gam'",
             br(),
             radioGroupButtons(
               inputId = "model_variation",
               label = "Select Model Variation:",
               choices = 1:5,
               selected = 1,
               status = "primary",
               size = "sm",
               justified = TRUE
             )
           ),
           
           conditionalPanel(
             condition = "input.model_type == 'gam'",
             radioButtons("plot_type",
                          "Plot Type:",
                          choices = c("Appraise" = "appraise",
                                      "Draw" = "draw"))
           ),
           
           conditionalPanel(
             condition = "input.model_type == 'pls' || input.model_type == 'vip'",
             br(),
             radioGroupButtons(
               inputId = "n_components",
               label = "Number of Components:",
               choices = 1:10,
               selected = 1,
               status = "primary",
               size = "sm",
               justified = TRUE
             )
           ),
           
           br(),
           radioGroupButtons(
             inputId = "cv_split",
             label = "Select CV Split:",
             choices = 1:10,
             selected = 1,
             status = "primary",
             size = "sm",
             justified = TRUE
           )
    ),
    column(9,
           plotOutput(outputId = "model_plot"),
           conditionalPanel(
             condition = "input.model_type == 'lm' || input.model_type == 'gam'",
             br(),
             wellPanel(
               style = "padding: 5px; margin-bottom: 5px;",
               h4("Model Formula:", align = "center", style = "margin: 5px;"),
               verbatimTextOutput("formula_info", placeholder = FALSE)
             )
           ),
           br(),
           DT::dataTableOutput(outputId = "results_table")
    ),
    align = "center"
  )
)

# Define server logic
server <- function(input, output, session) {
  
  observe({
    available_lists <- ls(envir = .GlobalEnv)[grep("^mods_", ls(envir = .GlobalEnv))]
    updateSelectInput(session, "results_list",
                      choices = available_lists)
  })
  
  selected_model <- reactive({
    req(input$results_list)
    model_results <- get(input$results_list, envir = .GlobalEnv)
    model_list <- model_results[[input$model_type]]
    cv_split <- model_list[[as.numeric(input$cv_split)]]
    
    if(input$model_type %in% c("lm", "gam")) {
      return(cv_split[[as.numeric(input$model_variation)]])
    } else {
      return(cv_split)
    }
  })
  
  selected_results <- reactive({
    req(input$results_list)
    results_name <- gsub("mods_", "results_", input$results_list)
    tryCatch({
      results_df <- get(results_name, envir = .GlobalEnv)
      return(results_df)
    }, error = function(e) {
      return(data.frame(Message = "No results available"))
    })
  })
  
  output$model_plot <- renderPlot({
    req(selected_model())
    model <- selected_model()
    
    switch(input$model_type,
           "lm" = gglm(model),
           "gam" = {
             if(input$plot_type == "appraise") {
               appraise(model)
             } else {
               draw(model)
             }
           },
           "pls" = plot(model, ncomp = input$n_components),
           "vip" = plot(model, ncomp = input$n_components))
  })
  
  output$formula_info <- renderText({
    req(selected_model())
    model <- selected_model()
    
    if(input$model_type %in% c("lm", "gam")) {
      deparse(formula(model))
    }
  })
  
  output$results_table <- DT::renderDataTable({
    selected_results()
  }, 
  options = list(
    ordering = TRUE,
    dom = 't',
    searching = FALSE
  ),
  rownames = FALSE,
  class = 'cell-border stripe hover'
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

