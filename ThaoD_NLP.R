require(shiny)
require(rvest)
require(dplyr)
require(tidyr)
require(tm)
require(stringr)
require(scales)
require(tidytext)
require(MASS)
require(ggplot2)
require(data.table)
require(shinythemes)

# Define UI for data upload
ui <- fluidPage(
  theme = shinytheme("slate"),
  # Assignment title
  br(),
  titlePanel("ThaoDuong_Natural_Language_Processing", windowTitle="ThaoD_NLP"),
  br(),
  # Create a sidebar for input action
  sidebarPanel(
    # User input for training data
    h3("Toyota Camry Review"),
    h5("Source: www.cars.com/research/toyota-camry/"),
    numericInput("train_from", "Training Data, from year:",2012, min=1992, max=2018),
    numericInput("train_to", "Training Data, to year:",2016, min=1992, max=2018),
    # Input for test data
    numericInput("test", "Test Data, year: ", 2017, min=1992, max=2018),
    # Action Button
    actionButton("do", "Show Reviews"),
    width = 2
  ),
  
  # Create main panel for results
  mainPanel(
    tabsetPanel(type="tabs",
                navbarMenu("Orginal Data",
                          tabPanel("Train Data", dataTableOutput('train')),
                          tabPanel("Test Data", dataTableOutput('test'))),
                navbarMenu("Normalized Data",
                          tabPanel("Normalized Train", dataTableOutput('trainn')),
                          tabPanel("Normalized Test", dataTableOutput('testn'))),
                navbarMenu("Data with Tags",
                          tabPanel("Tags Train", dataTableOutput('traint')),
                          tabPanel("Tags Test", dataTableOutput('testt'))),
                navbarMenu("Sentiment Score",
                          tabPanel("Train Score", dataTableOutput('trainscore')),
                          tabPanel("Test Score", dataTableOutput('testscore'))),
                tabPanel("Average Sentiment",
                        fluidRow(
                           column(width=5,
                                  h4("Training Data - Scaled"),
                                  tableOutput('averagetrain')),
                           column(width=5,
                                  h4("Per Tag - Scaled"),
                                  tableOutput('averagetag'))),
                        fluidRow(
                          h4("For all train data:"),
                          h5("The average sentiment scores are much lower than the star ratings, given the similar scale from 1 to 5."),
                          h4("For each tag:"),
                          h5("The average of sentiment scores for each tag does not correctly refrect the its average star, when comparing with other tags. 
                              For exampe, interior-tag has the lowest average star but it has the highest sentiment score in both Afinn, Bing, and second highest in NRC.
                              This shows that the sentiment analysis is not adequate to explain the star rating.
                              Compared with the average star rating and senstiment score for the whole train set, the average scores for all tags are allhigher.")
                        )),
                tabPanel("Star Prediction",
                        h4("Model: Ordinal Logistic Regression. Accuracy: 0.88"),
                        dataTableOutput('starpredict')),
                tabPanel("TF-IDF", plotOutput('tfidf'))
                ),
    width = 10
  )
)

server <- function(input,output, session) {
  observeEvent(input$do, {
    source("./ThaoD_Source.R")
    
    #Because we need to get the train and test data in every part, create a reactive for quick input
    basictrain <- reactive({
      years <- input$train_from : input$train_to
      #Get all the urls over years and pages
      urls <- bind_rows(lapply(years, count_urls), .id = NULL)
      urls <- unlist(urls, use.names = FALSE)
      #Read the reviews
      train <- bind_rows(lapply(urls, read_reviews))
      colnames(train) <- c("Year", "Star", "Review")
      train
    })
    
    basictest <- reactive({
      #Test set only comes from 1 year
      url0 <- paste("https://www.cars.com/research/toyota-camry-",input$test,"/consumer-reviews/?pg=", sep = "")
      npages <- url0 %>% read_html() %>% html_nodes(".page-list li a") %>% html_text() %>% tail(.,1) %>% as.numeric()
      urls <- paste(url0, 1:npages, sep="")
      test <- bind_rows(lapply(urls, read_reviews))
      colnames(test) <- c("Year", "Star", "Review")
      test      
    })
    
    ######## Objective 1 ###############################################  
    # Show Train Data Table
    output$train <- renderDataTable({
      train1 <- basictrain()
    })
    ######## Objective 2 ###############################################
    # Show Test Data Table
    output$test <- renderDataTable({
      test1 <- basictest()
    })
    
    ######## Objective 3 ###############################################
    # Normalize train table
    output$trainn <- renderDataTable({
      train2 <- basictrain()
      train2$Normalized <- train2$Review %>% tolower() %>% removePunctuation(., preserve_intra_word_dashes = TRUE)
      train2
    })
    # Normalize test table
    output$testn <- renderDataTable({
      test2 <- basictest()
      test2$Normalized <- test2$Review %>% tolower() %>% removePunctuation(., preserve_intra_word_dashes = TRUE)
      test2
    })
    
    ######## Objective 4 ###############################################
    # Tag each review in the train table
    output$traint <- renderDataTable({
      train3 <- basictrain()
      train3 <- create_tags(train3)
      train3
    })
    
    # Tag each review in the test table
    output$testt <- renderDataTable({
      test3 <- basictest()
      test3 <- create_tags(test3)
      test3
    })
    
    ######## Objective 5 ###############################################
    # Calculate sentiment score for train data
    output$trainscore <- renderDataTable({
      train4 <- basictrain()
      words <- create_words(train4)
      scores <- sentiment_score(train4, words)
      scores
    })
    
    # Calculate sentiment score for test data
    output$testscore <- renderDataTable({
      test4 <- basictest()
      words <- create_words(test4)
      scores <- sentiment_score(test4, words)
      scores
    })
    
    ######## Objective 6 ###############################################
    # Find average ratings
    output$averagetrain <- renderTable({
      train4 <- basictrain()
      words <- create_words(train4)
      scores <- sentiment_score(train4, words)
      
      #Scale the sentiment scores to range(1,5) so we can compare with the average star
      scores$Afinn <- rescale(scores$Afinn, to = c(1,5))
      scores$Bing <- rescale(scores$Bing, to = c(1,5))
      scores$NRC <- rescale(scores$NRC, to = c(1,5))
      #Compute the means
      average1 <- scores %>% summarise_at(vars(Star, Afinn, Bing, NRC), mean, na.rm = TRUE)
      average1
      #     Star    Afinn     Bing      NRC
      # 1 4.511905 2.582946 2.805085 1.997653
      ############# Comments ###########
      # The average sentiment scores are much lower than the star ratings, given the similar scale.
      
  })
    
    # Find average ratings per Tag
    output$averagetag <- renderTable({
      train4 <- basictrain()
      #Calculate sentiment score for all reviews
      words <- create_words(train4)
      scores <- sentiment_score(train4, words)
      
      #Scale the sentiment scores to range(1,5) so we can compare with the average star
      scores$Afinn <- rescale(scores$Afinn, to = c(1,5))
      scores$Bing <- rescale(scores$Bing, to = c(1,5))
      scores$NRC <- rescale(scores$NRC, to = c(1,5))
      
      #Create tag for each review
      scores <- create_tags(scores)
      scores$Tags <- unlist(scores$Tags)
      
      tags <- c("service", "price", "handling", "interior")
      
      average <- data.frame()
      
      for (i in 1:4) {
        avg_tag <- scores %>% group_by(Condition = str_detect(scores$Tags, tags[i])) %>% 
          summarise_at(vars(Star, Afinn, Bing, NRC), mean, na.rm=TRUE) %>%
          #Name the new Tags column to know which tag[i] is used
          mutate(Tags = tags[i])
        avg <- data.frame(avg_tag)
        average <- rbind(average, avg)
      }
      
      #Only select results for TRUE condition
      average2 <- subset(average, Condition == "TRUE", select = c(Tags, Star, Afinn, Bing, NRC))
      average2
      #     Tags     Star    Afinn     Bing      NRC
      # 2  service 4.555556 2.728831 2.851852 2.154971
      # 4    price 4.671053 2.773206 2.921053 2.072281
      # 6 handling 4.608696 3.178656 3.207729 2.249428
      # 8 interior 4.407080 2.835065 2.935103 2.222170
      
      ##### Comments ######
      # The average of sentiment scores for each tag does not correctly refrect the its average star, when comparing with other tags.
      # For exampe, "interior" has the lowest average star but it has the highest sentiment score in both Afinn, Bing, and second highest in NRC.
      # This shows that the sentiment analysis is not adequate to explain the star rating.
      # Compared with the average star rating and senstiment score for the whole train set, the average scores for all tags are higher.
    })

    
    ######## Objective 7+8 ###############################################
    output$starpredict <- renderDataTable({
    #Create train dataset
      train <- basictrain()
      trainwords <- create_words(train)
      trainset <- sentiment_score(train, trainwords)
      trainset <- nrc_score(trainset, trainwords)
      trainset[, 5:10][is.na(trainset[,5:10])] <- 0

    #Create test dataset
      test <-basictest()
      testwords <- create_words(test)
      testset <- sentiment_score(test, testwords)
      testset <- nrc_score(testset, testwords)
      testset[, 5:10][is.na(testset[,5:10])] <- 0

    #Linear model
      lm <- lm(Star ~ Bing + NRC + Fear + Anger + Trust, data = trainset)
      prediction <- round(predict(lm, testset), digits = 0)
      table(testset$Star, prediction)
      # prediction
      #     4   5   6
      # 1   3   0   0
      # 3   2   1   0
      # 4   8  29   0
      # 5  46 234   2
      accuracy <- sum(prediction == testset$Star)/nrow(testset)
      #[1] 0.7446154

    # Ordinal Logistic Regression
      #After running a few models, this one gives all significant variables
      ml <- polr(as.factor(Star) ~ Afinn + Bing + Fear + Trust, data = trainset, Hess = TRUE)
      
      summary(ml)
      # Call:
      #   polr(formula = as.factor(Star) ~ Afinn + Bing + Fear + Trust, 
      #        data = trainset, Hess = TRUE)
      # 
      # Coefficients:
      #   Value Std. Error t value
      # Afinn  0.05001    0.02284   2.190
      # Bing   0.21301    0.04112   5.180
      # Fear  -0.40731    0.08990  -4.531
      # Trust -0.14863    0.03813  -3.898
      # 
      # Intercepts:
      #   Value    Std. Error t value 
      # 1|2  -4.0991   0.2742   -14.9494
      # 2|3  -3.0153   0.1948   -15.4797
      # 3|4  -1.8562   0.1475   -12.5881
      # 4|5  -0.4588   0.1232    -3.7242
      # 
      # Residual Deviance: 1361.788 
      # AIC: 1377.788 
      
      # Optimize the function to find the best model
      summary(update(ml, method = "probit", Hess = TRUE), digits = 3)
      # Call:
      #   polr(formula = as.factor(Star) ~ Afinn + Bing + Fear + Trust, 
      #        data = trainset, Hess = TRUE, method = "probit")
      # 
      # Coefficients:
      #   Value Std. Error t value
      # Afinn  0.026450   0.012171  2.1732
      # Bing   0.115235   0.020932  5.5052
      # Fear  -0.241190   0.051692 -4.6659
      # Trust -0.085412   0.019777 -4.3188
      # 
      # Intercepts:
      #   Value     Std. Error t value  
      # 1|2  -2.24879   0.12462  -18.04547
      # 2|3  -1.72607   0.09594  -17.99109
      # 3|4  -1.12223   0.07885  -14.23222
      # 4|5  -0.33434   0.06939   -4.81837
      # 
      # Residual Deviance: 1360.224 
      # AIC: 1376.224 
      
      summary(update(ml, method = "logistic", Hess = TRUE), digits = 3)
      # Call:
      #   polr(formula = as.factor(Star) ~ Afinn + Bing + Fear + Trust, 
      #        data = trainset, Hess = TRUE, method = "logistic")
      # 
      # Coefficients:
      #   Value Std. Error t value
      # Afinn  0.050015   0.022840  2.1898
      # Bing   0.213006   0.041123  5.1798
      # Fear  -0.407309   0.089898 -4.5308
      # Trust -0.148629   0.038134 -3.8975
      # 
      # Intercepts:
      #   Value     Std. Error t value  
      # 1|2  -4.09915   0.27420  -14.94943
      # 2|3  -3.01533   0.19479  -15.47973
      # 3|4  -1.85616   0.14745  -12.58806
      # 4|5  -0.45878   0.12319   -3.72423
      # 
      # Residual Deviance: 1361.788 
      # AIC: 1377.788 
      
      summary(update(ml, method = "cloglog", Hess = TRUE), digits = 3)
      # Call:
      #   polr(formula = as.factor(Star) ~ Afinn + Bing + Fear + Trust, 
      #        data = trainset, Hess = TRUE, method = "cloglog")
      # 
      # Coefficients:
      #   Value Std. Error t value
      # Afinn  0.050909   0.017861  2.8504
      # Bing   0.151476   0.026326  5.7539
      # Fear  -0.359932   0.061223 -5.8791
      # Trust -0.071937   0.024775 -2.9036
      # 
      # Intercepts:
      #   Value     Std. Error t value  
      # 1|2  -3.85557   0.25038  -15.39898
      # 2|3  -2.86762   0.17134  -16.73629
      # 3|4  -1.83687   0.12386  -14.82995
      # 4|5  -0.65357   0.09587   -6.81744
      # 
      # Residual Deviance: 1366.391 
      # AIC: 1382.391 
      
      #Adding interaction terms
      ml2 <- stepAIC(ml, ~.^2)
      # Start:  AIC=1377.79
      # as.factor(Star) ~ Afinn + Bing + Fear + Trust
      # 
      # Df    AIC
      # + Afinn:Bing   1 1345.9
      # + Bing:Trust   1 1365.2
      # + Fear:Trust   1 1368.0
      # + Afinn:Trust  1 1375.5
      # <none>           1377.8
      # + Bing:Fear    1 1378.2
      # + Afinn:Fear   1 1379.6
      # - Afinn        1 1380.6
      # - Trust        1 1391.2
      # - Fear         1 1396.6
      # - Bing         1 1406.2
      # 
      # Step:  AIC=1345.9
      # as.factor(Star) ~ Afinn + Bing + Fear + Trust + Afinn:Bing
      # 
      # Df    AIC
      # + Fear:Trust   1 1332.8
      # + Afinn:Trust  1 1342.1
      # <none>           1345.9
      # + Afinn:Fear   1 1347.0
      # + Bing:Fear    1 1347.8
      # + Bing:Trust   1 1347.8
      # - Trust        1 1350.2
      # - Fear         1 1353.0
      # - Afinn:Bing   1 1377.8
      # 
      # Step:  AIC=1332.77
      # as.factor(Star) ~ Afinn + Bing + Fear + Trust + Afinn:Bing + 
      #   Fear:Trust
      # 
      # Df    AIC
      # + Bing:Fear    1 1325.8
      # + Bing:Trust   1 1330.3
      # + Afinn:Fear   1 1330.4
      # <none>           1332.8
      # + Afinn:Trust  1 1334.7
      # - Fear:Trust   1 1345.9
      # - Afinn:Bing   1 1368.0
      # 
      # Step:  AIC=1325.78
      # as.factor(Star) ~ Afinn + Bing + Fear + Trust + Afinn:Bing + 
      #   Fear:Trust + Bing:Fear
      # 
      # Df    AIC
      # <none>           1325.8 <= this is the best model
      # + Bing:Trust   1 1326.3
      # + Afinn:Fear   1 1327.7
      # + Afinn:Trust  1 1327.7
      # - Bing:Fear    1 1332.8
      # - Fear:Trust   1 1347.8
      # - Afinn:Bing   1 1358.2
      
      summary(ml2)
      # Call:
      #   polr(formula = as.factor(Star) ~ Afinn + Bing + Fear + Trust + 
      #          Afinn:Bing + Fear:Trust + Bing:Fear, data = trainset, Hess = TRUE)
      # 
      # Coefficients:
      #   Value Std. Error t value
      # Afinn       0.08351   0.022635   3.689
      # Bing        0.39977   0.052808   7.570
      # Fear       -0.53537   0.114532  -4.674
      # Trust      -0.26708   0.051445  -5.192
      # Afinn:Bing -0.01025   0.001658  -6.181
      # Fear:Trust  0.09629   0.021677   4.442
      # Bing:Fear  -0.05269   0.017638  -2.987
      # 
      # Intercepts:
      #   Value    Std. Error t value 
      # 1|2  -4.2813   0.3018   -14.1845
      # 2|3  -3.0728   0.2196   -13.9906
      # 3|4  -1.7816   0.1738   -10.2531
      # 4|5  -0.2812   0.1529    -1.8392
      # 
      # Residual Deviance: 1303.776 
      # AIC: 1325.776 
      
      #Predict the probablity of the star rating
      predict <- predict(ml2, testset, type = "probs")
      
      #The predicted value is the one with maximum probabilities
      predict2 <- as.data.table(predict)[, MAX := colnames(.SD)[max.col(.SD, ties.method="first")]]
      
      # Confusion matrix
      table(testset$Star, predict2$MAX)
      #     4   5
      # 1   0   3
      # 3   0   3
      # 4   4  33
      # 5   0 282
      
      accuracy <- sum(predict2$MAX == testset$Star)/nrow(testset)
      #[1] 0.88
      
      final <- data.frame(Review = testset$Review, Star = testset$Star, predict2)
      colnames(final) <- c("Review", "Star", "1-Prob", "2-Prob", "3-Prob", "4-Prob", "5-Prob", "Pred_Star")
      final
    })
    
    ######## Objective 9 ###############################################
    output$tfidf <- renderPlot({
    #Compute TF-IDF for every word in the set by tag
    
    #First, create a table for each tag
    train <- basictrain()
    train$Normalized <- train$Review %>% tolower() %>% removePunctuation(., preserve_intra_word_dashes = TRUE)
    #Create separate table for each tag
    price <- train[str_detect(train$Normalized, "price"),]
    handling <- train[str_detect(train$Normalized, "handling"),]
    interior <- train[str_detect(train$Normalized, "interior"),]
    service <- train[str_detect(train$Normalized, "service"),]
    
    #Add tag name before binding all in a table
    price$tag <- "price"
    handling$tag <- "handling"
    interior$tag <- "interior"
    service$tag <- "service"
    
    #Find all words per tag and count their occurrences, as well as total words in each tag
    price_words <- create_train_words(price)
    handling_words <- create_train_words(handling)
    interior_words <- create_train_words(interior)
    service_words <- create_train_words(service)
    
    #Create a table of all tags and words associated with each tag
    train_words <- bind_rows(price_words, handling_words, interior_words, service_words, .id = NULL)
    
    #Use bind_tf_idf function in tidytext package to calculate tf-idf for each word in each tag
    train_words <- train_words %>% bind_tf_idf(word, tag, n) %>% arrange(desc(tf_idf))
    
    #Remove stopwords
    train_words <- train_words[!(train_words$word %in% stopwords("en")),]
    
    #Visualize the top 10 tf-idf words for each tag
    plot <- train_words %>% 
      group_by(tag) %>% 
      top_n(10, tf_idf) %>%
      ungroup() %>%
      mutate(word = reorder(word, tf_idf)) %>%
      ggplot(aes(word, tf_idf, fill = tag)) +
      geom_col(show.legend = FALSE) +
      labs(x = NULL, y = "tf-idf") + 
      facet_wrap(~tag, ncol = 2, scales = "free") + coord_flip()
    plot
    })
  })
}


shinyApp(ui = ui, server = server)






