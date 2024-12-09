library(shiny)
library(readxl)
library(tidyverse)
library(pscl)
library(corrplot)
library(MASS)
library(lmtest)
library(mgcv)
library(flexmix)
library(mixtools)
library(openxlsx)

#File location
path <- "C:/Users/OyinozaIbrahim/4most/General Insurance - Documents/Pricing/GLM Models/Example Model"

#Loading the data
data <- read_excel(file.path(path,"CHGLMData_202302_20230615_v01.xlsx"))

#Data cleaning

#Create a data cleaning function:
glm_prep <- function(d){
  d %>% 
    mutate_at(vars(City, CityGroup, Renewal, Region, RegionCode,
                   Occupation0, Occupation, AgeGroup,
                   BMIGroup0, BMIGroup1, BMIGroup2,
                   Gender, MaritalStatus, MaritalStatus2,
                   ProdNo,Paket,Grup),
              as.factor) %>% 
    mutate(Exposure1=pmin(Exposure,1),
           BMIGroup = if_else(BMIGroup0=="Risky","Underweight",BMIGroup0),
           BMIGroup=fct_relevel(BMIGroup,
                                "Normal","Underweight","Overweight","Obese"),
           BMIGroup1=fct_relevel(BMIGroup1,
                                 "Baby","Underweight","Normal","Overweight","Obese1","Obese2"),
           BMIGroup2=fct_relevel(BMIGroup2,
                                 "Baby","Risky","Normal","Overweight","Obese"),
           CityGroup=fct_relevel(CityGroup,
                                 "1","2","3","4","5"),
           Occupation.Status = if_else(Occupation%in%c("Whitecollor","Civilservant","Teacher"),
                                       "Whitecollor",
                                       Occupation),
           Marital.Status=if_else(MaritalStatus2%in%c("Divorced","Widowed"),
                                  "Divorced/Widowed",
                                  MaritalStatus2),
           Marital.Status = fct_relevel(Marital.Status,
                                        "Child","Single","Married","Divorced/Widowed"),
           ClaimCount=pmin(ClaimCountOut,10),
           ClaimFlag = if_else(ClaimCount==0,0,1),
           Exposure.band = case_when(
             between(Exposure1,0,0.1)~"0.0-0.1",
             between(Exposure1,0.1,0.2)~"0.1-0.2",
             between(Exposure1,0.2,0.3)~"0.2-0.3",
             between(Exposure1,0.3,0.4)~"0.3-0.4",
             between(Exposure1,0.4,0.5)~"0.4-0.5",
             between(Exposure1,0.5,0.6)~"0.5-0.6",
             between(Exposure1,0.6,0.7)~"0.6-0.7",
             between(Exposure1,0.7,0.8)~"0.7-0.8",
             between(Exposure1,0.8,0.9)~"0.8-0.9",
             between(Exposure1,0.9,1)~"0.9-1"
           ))
}

#prepare specific data 
data1 <- glm_prep(data) %>% filter(ClaimAmountOut<5000)

#GLM in R Shiny

#User-interface: Layout controls
ui <- fluidPage(
  sidebarPanel(
    selectInput("ll_var",
                "Select large loss variable",
                choices = names(data1)),
    sliderInput("ll_thresh_var",
                "Set large loss threshold",
                min = 0, max = 10000, value = 5000), #slider to select the large loss threshold
    selectInput("nil_claims",
                "Nil Claims",
                choices = c("Include","Exclude")),
    selectInput("response_var",
                "Select Response Variable",
                choices = names(data1)), #drop-down to select 'response'; single choice
    selectInput("offset_var",
                "Select Offset Variable",
                choices = names(data1)), #drop-down to select 'offset'; single choice
    selectInput("predictor_vars",
                "Select Predictor Variables",
                choices = names(data1),
                multiple = T), #drop-down to select 'predictors'; multiple choices
    selectInput("model_option",
                "Select Model Option",
                choices = c("Poisson","Negative Binomial",
                            "Zero Inflated Poisson",
                            "Zero Inflated Negative Binomial",
                            "Poisson Mixture")), #drop-down to select 'model type'; single choice
    sliderInput("model_count_var",
                "Set number of mixtures",
                min = 2, max = 10, value = 2), #slider to select the large loss threshold
    actionButton("fit_button","Run Model")  #action button to run the model
  ),
  mainPanel(
    tabsetPanel( 
      tabPanel("Fit",
               verbatimTextOutput("glm_output0")),
      tabPanel("Exp(Coefficients)",
               tableOutput("glm_output1"),
               downloadButton("download_relativities", "Download Exp(Coefficients)")),
      tabPanel("Diagnostics",
               tableOutput("glm_output2")),
      tabPanel("Distribution",
               tableOutput("glm_output3")),
      tabPanel("Plot1",
               plotOutput("plot_dist")),
      tabPanel("Plot2",
               plotOutput("plot_exp"))
    )
  )
)

server <- function(input, output){
  
  model <- reactiveVal(NULL)
  
  fit_model <- eventReactive(input$fit_button,{
    
    req(input$response_var, input$predictor_vars)
    
    ll.var <- input$ll_var
    
    llt <- input$ll_thresh_var
    
    response <- input$response_var
    
    offset <- input$offset_var
    
    predictors <- input$predictor_vars
    
    model.option <- input$model_option
    
    model_count <- input$model_count_var
    
    nil.claims <- input$nil_claims
    
    if(nil.claims=="Include"){
      data1 <- glm_prep(data) %>% 
        filter(!!sym(ll.var)<llt)
    }
    
    else{
      data1 <- glm_prep(data) %>% 
        filter(!!sym(ll.var)<llt) %>% 
        filter(!!sym(response)>0)
    }
    
    if(model.option=="Poisson"){
      
      
      
      model <- glm(paste(paste(response,"~",paste(predictors, collapse = "+"), sep = ""),"+ offset(log(",offset,"))"),
                   data = data1,
                   #offset = log(offset),
                   family = "poisson"(link = "log"))
      
      exp.coef <- exp(coef(model))
      
      fitted.val <- model$fitted.values
      
      exp.coef.data <- data.frame(
        exp.coef = round(exp.coef,2)) %>% 
        rownames_to_column(.,var = "Predictor")
      
      aic <- round(AIC(model),2)
      
      bic <- round(BIC(model),2)
      
      loglik <- round(logLik(model),2)
      
      rmse <- round(sqrt(mean((data1[[response]]-model$fitted.values)^2)),2)
      
      gof.table <- tibble(
        gof.measure = c("Log Likelihood","AIC","BIC","RMSE"),
        gof.values = c(loglik, aic, bic, rmse)
      )
      
      dist <- tibble(
        mean_actual = mean(data1[[response]]),
        var_actual = var(data1[[response]]),
        min_actual = min(data1[[response]]),
        Q1_actual = quantile(data1[[response]], probs = 0.25),
        median_actual = median(data1[[response]]),
        Q3_actual = quantile(data1[[response]],probs = 0.75),
        max_actual = max(data1[[response]]),
        mean_fitted = mean(fitted.val),
        var_fitted = var(fitted.val),
        min_fitted = min(fitted.val),
        Q1_fitted = quantile(fitted.val, probs = 0.25),
        median_fitted = median(fitted.val),
        Q3_fitted = quantile(fitted.val,probs = 0.75),
        max_fitted = max(fitted.val)
      ) %>% 
        pivot_longer(1:14, names_to = "Variable",values_to = "Values") %>% 
        mutate(Measure = str_split_fixed(Variable,"_",2)[,1],
               Data = str_split_fixed(Variable,"_",2)[,2]) %>% 
        dplyr::select(-Variable) %>% 
        pivot_wider(names_from = "Data", values_from = "Values")
      
      summary.model <- summary(model)
    }
    
    else if(model.option == "Negative Binomial"){
      
      #data1 <- glm_prep(data) %>% filter(ClaimAmountOut<llt)
      
      model <- glm.nb(paste(paste(response,"~",paste(predictors, collapse = "+"), sep = ""),"+ offset(log(",offset,"))"),
                      data = data1,
                      link = "log")
      
      exp.coef <- exp(coef(model))
      
      fitted.val <- model$fitted.values
      
      exp.coef.data <- data.frame(
        exp.coef = round(exp.coef,2)) %>% 
        rownames_to_column(.,var = "Predictor")
      
      aic <- round(AIC(model),2)
      
      bic <- round(BIC(model),2)
      
      loglik <- round(logLik(model),2)
      
      rmse <- round(sqrt(mean((data1[[response]]-model$fitted.values)^2)),2)
      
      gof.table <- tibble(
        gof.measure = c("Log Likelihood","AIC","BIC","RMSE"),
        gof.values = c(loglik, aic, bic, rmse)
      )
      
      dist <- tibble(
        mean_actual = mean(data1[[response]]),
        var_actual = var(data1[[response]]),
        min_actual = min(data1[[response]]),
        Q1_actual = quantile(data1[[response]], probs = 0.25),
        median_actual = median(data1[[response]]),
        Q3_actual = quantile(data1[[response]],probs = 0.75),
        max_actual = max(data1[[response]]),
        mean_fitted = mean(fitted.val),
        var_fitted = var(fitted.val),
        min_fitted = min(fitted.val),
        Q1_fitted = quantile(fitted.val, probs = 0.25),
        median_fitted = median(fitted.val),
        Q3_fitted = quantile(fitted.val,probs = 0.75),
        max_fitted = max(fitted.val)
      ) %>% 
        pivot_longer(1:14, names_to = "Variable",values_to = "Values") %>% 
        mutate(Measure = str_split_fixed(Variable,"_",2)[,1],
               Data = str_split_fixed(Variable,"_",2)[,2]) %>% 
        dplyr::select(-Variable) %>% 
        pivot_wider(names_from = "Data", values_from = "Values")
      
      summary.model <- summary(model)
    }
    
    else if(model.option == "Zero Inflated Poisson"){
      
      #data1 <- glm_prep(data) %>% filter(ClaimAmountOut<llt)
      model <- zeroinfl(as.formula(paste(paste(response,"~",paste(predictors, collapse = "+"), sep = ""),"+ offset(log(",offset,"))")),
                        data = data1,
                        dist = "poisson")#,
      #offset = log(Exposure1))
      
      exp.coef.poisson <- exp(model$coefficients$count)
      exp.coef.binomial <- exp(model$coefficients$zero)
      
      fitted.val <- model$fitted.values
      
      exp.coef.data <- data.frame(
        exp.coef.poisson = round(exp.coef.poisson,2),
        exp.coef.binomial = round(exp.coef.binomial,2)) %>% 
        rownames_to_column(.,var = "Predictor") 
      
      aic <- round(AIC(model),2)
      
      bic <- round(BIC(model),2)
      
      loglik <- round(logLik(model),2)
      
      rmse <- round(sqrt(mean((data1[[response]]-model$fitted.values)^2)),2)
      
      gof.table <- tibble(
        gof.measure = c("Log Likelihood","AIC","BIC","RMSE"),
        gof.values = c(loglik, aic, bic, rmse)
      )
      
      dist <- tibble(
        mean_actual = mean(data1[[response]]),
        var_actual = var(data1[[response]]),
        min_actual = min(data1[[response]]),
        Q1_actual = quantile(data1[[response]], probs = 0.25),
        median_actual = median(data1[[response]]),
        Q3_actual = quantile(data1[[response]],probs = 0.75),
        max_actual = max(data1[[response]]),
        mean_fitted = mean(fitted.val),
        var_fitted = var(fitted.val),
        min_fitted = min(fitted.val),
        Q1_fitted = quantile(fitted.val, probs = 0.25),
        median_fitted = median(fitted.val),
        Q3_fitted = quantile(fitted.val,probs = 0.75),
        max_fitted = max(fitted.val)
      ) %>% 
        pivot_longer(1:14, names_to = "Variable",values_to = "Values") %>% 
        mutate(Measure = str_split_fixed(Variable,"_",2)[,1],
               Data = str_split_fixed(Variable,"_",2)[,2]) %>% 
        dplyr::select(-Variable) %>% 
        pivot_wider(names_from = "Data", values_from = "Values")
      
      summary.model <- summary(model)
    }
    
    else if(model.option == "Zero Inflated Negative Binomial"){
      
      # data1 <- glm_prep(data) %>% filter(ClaimAmountOut<llt)
      
      model <- zeroinfl(as.formula(paste(paste(response,"~",paste(predictors, collapse = "+"), sep = ""),"+ offset(log(",offset,"))")),
                        data = data1,
                        dist = "negbin")#,
      #offset = log(Exposure1))
      
      
      exp.coef.negbin <- exp(model$coefficients$count)
      exp.coef.binomial <- exp(model$coefficients$zero)
      
      fitted.val <- model$fitted.values
      
      exp.coef.data <- data.frame(
        exp.coef.negbin = round(exp.coef.negbin,2),
        exp.coef.binomial = round(exp.coef.binomial,2)) %>% 
        rownames_to_column(.,var = "Predictor")
      
      aic <- round(AIC(model),2)
      
      bic <- round(BIC(model),2)
      
      loglik <- round(logLik(model),2)
      
      rmse <- round(sqrt(mean((data1[[response]]-model$fitted.values)^2)),2)
      
      gof.table <- tibble(
        gof.measure = c("Log Likelihood","AIC","BIC","RMSE"),
        gof.values = c(loglik, aic, bic, rmse)
      )
      
      dist <- tibble(
        mean_actual = mean(data1[[response]]),
        var_actual = var(data1[[response]]),
        min_actual = min(data1[[response]]),
        Q1_actual = quantile(data1[[response]], probs = 0.25),
        median_actual = median(data1[[response]]),
        Q3_actual = quantile(data1[[response]],probs = 0.75),
        max_actual = max(data1[[response]]),
        mean_fitted = mean(fitted.val),
        var_fitted = var(fitted.val),
        min_fitted = min(fitted.val),
        Q1_fitted = quantile(fitted.val, probs = 0.25),
        median_fitted = median(fitted.val),
        Q3_fitted = quantile(fitted.val,probs = 0.75),
        max_fitted = max(fitted.val)
      ) %>% 
        pivot_longer(1:14, names_to = "Variable",values_to = "Values") %>% 
        mutate(Measure = str_split_fixed(Variable,"_",2)[,1],
               Data = str_split_fixed(Variable,"_",2)[,2]) %>% 
        dplyr::select(-Variable) %>% 
        pivot_wider(names_from = "Data", values_from = "Values")
      
      summary.model <- summary(model)
    }
    
    
    
    else if(model.option == "Poisson Mixture"){
      
      # data1 <- glm_prep(data) %>% filter(ClaimAmountOut<llt)
      
      model <- flexmix(as.formula(paste(paste(response,"~",paste(predictors, collapse = "+"), sep = ""),"+ offset(log(",offset,"))")),
                       data = data1, 
                       k = model_count,
                       model = FLXMRglm(family = "poisson"))
      
      exp.coef <- round(exp(parameters(model)),2)
      
      posteriors <- posterior(model)
      
      #fitted.val <- fitted(model)[,1]*posteriors[,1]+fitted(model)[,2]*posteriors[,2]
      fitted.val <- rowSums(sapply(1:model_count, function(i) fitted(model)[, i] * posteriors[, i]))
      
      exp.coef.data <- data.frame(exp.coef) %>% 
        rownames_to_column(.,var = "Predictor")
      
      aic <- round(AIC(model),2)
      
      bic <- round(BIC(model),2)
      
      loglik <- round(model@logLik,2)
      
      rmse <- round(sqrt(mean((data1[[response]]-fitted.val)^2)),2)
      
      gof.table <- tibble(
        gof.measure = c("Log Likelihood","AIC","BIC","RMSE"),
        gof.values = c(loglik, aic, bic, rmse)
      )
      
      dist <- tibble(
        mean_actual = mean(data1[[response]]),
        var_actual = var(data1[[response]]),
        min_actual = min(data1[[response]]),
        Q1_actual = quantile(data1[[response]], probs = 0.25),
        median_actual = median(data1[[response]]),
        Q3_actual = quantile(data1[[response]],probs = 0.75),
        max_actual = max(data1[[response]]),
        mean_fitted = mean(fitted.val),
        var_fitted = var(fitted.val),
        min_fitted = min(fitted.val),
        Q1_fitted = quantile(fitted.val, probs = 0.25),
        median_fitted = median(fitted.val),
        Q3_fitted = quantile(fitted.val,probs = 0.75),
        max_fitted = max(fitted.val)
      ) %>% 
        pivot_longer(1:14, names_to = "Variable",values_to = "Values") %>% 
        mutate(Measure = str_split_fixed(Variable,"_",2)[,1],
               Data = str_split_fixed(Variable,"_",2)[,2]) %>% 
        dplyr::select(-Variable) %>% 
        pivot_wider(names_from = "Data", values_from = "Values")
      
      summary.model <- summary(model)
      
      
      
    }
    
    
    
    list(exp.coef = exp.coef.data,
         gof.table = gof.table,
         dist = dist,
         summary.model = summary.model
    )
  })
  
  output$glm_output0 <- renderPrint({
    fit_model()$summary.model
  })
  
  output$glm_output1 <- renderTable({
    fit_model()$exp.coef
  }
  )
  
  output$download_relativities <- downloadHandler(
    filename = function() {
      paste0(input$model_option, "_exp.coef.xlsx")
    },
    content = function(file) {
      write.xlsx(fit_model()$exp.coef, file, row.names = FALSE)
    }
  )
  output$glm_output2 <- renderTable({
    fit_model()$gof.table
  },
  caption = "Larger value of log-likelihood and smaller values of AIC, BIC and RMSE indicate that the model is a good fit!")
  
  output$glm_output3 <- renderTable({
    fit_model()$dist
  }
  )
  
  output$plot_dist <- renderPlot({
    
    req(input$response_var, input$predictor_vars)
    
    response <- input$response_var
    
    offset <- input$offset_var
    
    predictors <- input$predictor_vars
    
    model.option <- input$model_option
    
    model_count <- input$model_count_var
    
    if(model.option=="Poisson"){
      
      model <- glm(paste(response,"~",paste(paste(predictors, collapse = "+"), sep = ""),"+ offset(log(",offset,"))"),
                   data = data1,
                   #offset = log(Exposure1),
                   family = "poisson"(link = "log"))
      fitted.val <- model$fitted.values
    }
    
    else if(model.option == "Negative Binomial"){
      model <- glm.nb(paste(paste(response,"~",paste(predictors, collapse = "+"), sep = ""),"+ offset(log(",offset,"))"),
                      data = data1,
                      link = "log")
      fitted.val <- model$fitted.values
    }
    
    else if(model.option == "Zero Inflated Poisson"){
      model <- zeroinfl(as.formula(paste(paste(response,"~",paste(predictors, collapse = "+"), sep = ""),"+ offset(log(",offset,"))")),
                        data = data1,
                        dist = "poisson")#,
      #offset = log(Exposure1))
      
      fitted.val <- model$fitted.values
    }
    
    else if(model.option == "Zero Inflated Negative Binomial"){
      model <- zeroinfl(as.formula(paste(paste(response,"~",paste(predictors, collapse = "+"), sep = ""),"+ offset(log(",offset,"))")),
                        data = data1,
                        dist = "negbin")#,
      #offset = log(Exposure1))
      
      fitted.val <- model$fitted.values
    }
    
    
    if(model.option=="Poisson Mixture"){
      
      model <- flexmix(as.formula(paste(paste(response,"~",paste(predictors, collapse = "+"), sep = ""),"+ offset(log(",offset,"))")),
                       data = data1, 
                       k = model_count,
                       model = FLXMRglm(family = "poisson"))
      
      posteriors <- posterior(model)
      
      #fitted.val <- fitted(model)[,1]*posteriors[,1]+fitted(model)[,2]*posteriors[,2]
      fitted.val <- rowSums(sapply(1:model_count, function(i) fitted(model)[, i] * posteriors[, i]))
    }
    
    
    test <- tibble(
      actual = data1[[response]],
      fitted = fitted.val) 
    
    test1 <- test %>%
      pivot_longer(1:2, names_to = "Model", values_to = "Values") 
    
    plot1 <- ggplot(data = test1)+
      geom_histogram(aes(x = Values,y=..density.., fill=Model),
                     #size=1.25,
                     bins = 10,
                     alpha = 0.7,
                     color="grey50")+
      facet_grid(~Model)+
      labs(title = "Actual vs Fitted Distribution")+
      scale_fill_manual(values = c("blue","red"))+
      xlab("Claim Count")+
      ylab("Density")+
      theme_minimal()+
      theme(plot.title = element_text(size = 16),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            legend.position = "none",
            strip.text = element_text(size = 14)
      )
    
    
    print(plot1)
    
  })
  
  output$plot_exp <- renderPlot({
    
    req(input$response_var, input$predictor_vars)
    
    response <- input$response_var
    
    offset <- input$offset_var
    
    predictors <- input$predictor_vars
    
    model.option <- input$model_option
    
    model_count <- input$model_count_var
    
    if(model.option=="Poisson"){
      
      model <- glm(paste(paste(response,"~",paste(predictors, collapse = "+"), sep = ""),"+ offset(log(",offset,"))"),
                   data = data1,
                   #offset = log(Exposure1),
                   family = "poisson"(link = "log"))
      fitted.val <- model$fitted.values
    }
    
    else if(model.option == "Negative Binomial"){
      model <- glm.nb(paste(paste(response,"~",paste(predictors, collapse = "+"), sep = ""),"+ offset(log(",offset,"))"),
                      data = data1,
                      link = "log")
      fitted.val <- model$fitted.values
    }
    
    else if(model.option == "Zero Inflated Poisson"){
      model <- zeroinfl(as.formula(paste(paste(response,"~",paste(predictors, collapse = "+"), sep = ""),"+ offset(log(",offset,"))")),
                        data = data1,
                        dist = "poisson")#,
      #offset = log(Exposure1))
      
      fitted.val <- model$fitted.values
    }
    
    else if(model.option == "Zero Inflated Negative Binomial"){
      model <- zeroinfl(as.formula(paste(paste(response,"~",paste(predictors, collapse = "+"), sep = ""),"+ offset(log(",offset,"))")),
                        data = data1,
                        dist = "negbin")#,
      #offset = log(Exposure1))
      
      fitted.val <- model$fitted.values
    }
    
    
    if(model.option=="Poisson Mixture"){
      
      model <- flexmix(as.formula(paste(paste(response,"~",paste(predictors, collapse = "+"), sep = ""),"+ offset(log(Exposure1))")),
                       data = data1, 
                       k = model_count,
                       model = FLXMRglm(family = "poisson"))
      
      posteriors <- posterior(model)
      
      #fitted.val <- fitted(model)[,1]*posteriors[,1]+fitted(model)[,2]*posteriors[,2]
      fitted.val <- rowSums(sapply(1:model_count, function(i) fitted(model)[, i] * posteriors[, i]))
    }
    
    exp_band <- tibble(
      actual = data1[[response]],
      fitted = fitted.val,
      Exposure = data1[["Exposure.band"]]
    ) %>% 
      group_by(Exposure) %>% 
      summarise(Actual = mean(actual),
                Fitted = mean(fitted)) %>% 
      ungroup() %>% 
      pivot_longer(cols = c(Actual,Fitted),
                   names_to = "Data",
                   values_to = "Values")
    
    
    plot2 <- ggplot(data = exp_band)+
      geom_col(aes(x = Values, y = Exposure, fill = Data),
               position = "dodge")+
      coord_flip()+
      labs(title = "Average Actual vs Fitted Counts by Exposure Band")+
      scale_fill_manual(values = c("blue","red"))+
      xlab("Average Count")+
      ylab("Exposure")+
      theme_minimal()+
      theme(plot.title = element_text(size = 16),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            legend.text = element_text(size = 12))
    
    
    
    print(plot2)
    
  })
  
}

shinyApp(ui = ui, server = server)


