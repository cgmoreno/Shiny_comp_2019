shinyServer(function(input, output, session) {
  ## ------------------------------------------- TAB 1 -------------------------------------------- ##
  
  output$background <- renderText({
    paste("This Shiny app makes use of the Tidy Tuesday 'fastfood_calories.csv' dataset published 09/04/2018*.
          The goal is to allow users to explore different hierarchical clustering methods by varying the distance calculation and linkage types.
          It also aims to enable interactive interpretation of the resulting clusters via a side-by-side comparison.\n Albeit a silly example, such an analysis
          could be used to address 'can I predict if a fast food item is vegetarian based on the nutrition profile alone?' or 
          'what does it mean if salads get clustered with burgers?'")
  })
  
  output$univariate_view <- renderTable({
    if(input$var_list %in% cat_x) {
      food_use %>% select(Level = input$var_list) %>% count(Level)
    }
    else if(input$var_list %in% cont_x) {
      tibble::tibble(`Min.` = food_use %>% pull(input$var_list) %>% min(),
                     `1st Qu.` = food_use %>% pull(input$var_list) %>% quantile(0.25),
                     Median = food_use %>% pull(input$var_list) %>% median(),
                     Mean = food_use %>% pull(input$var_list) %>% mean(),
                     `3rd Qu.` = food_use %>% pull(input$var_list) %>% quantile(0.75),
                     `Max.` = food_use %>% pull(input$var_list) %>% max())
                     
    }
  })
  output$dataset <- renderDataTable({
    food_use %>% select(-X1) %>% 
      select(restaurant, item, cal_fat, cal_nonfat, sat_fat, trans_fat, other_fat, cholesterol, sodium, 
             total_carb, fiber, sugar, protein)
  })
  ## ------------------------------------------- TAB 2 -------------------------------------------- ##
  
  ## Cluster 1
  clust1_res <- eventReactive(input$conduct_clust1, {
    dist_mat <- dist(food_use_clust %>% select(-X1, -restaurant, -item, -bbq, -bacon, -salad, -crispy, -veg), 
                     method = input$dist1)
    
    if(input$linkage1 %in% "minimax") {
      clust1 <- protoclust(dist_mat)
    }
    else {
      clust1 <- hclust(dist_mat, method = input$linkage1)
    }
    return(list(cluster = clust1, linkage = input$linkage1))
  })
  
  output$isolate_clust1 <- renderUI({
    numericInput("which_box_clust1", "Select cluster", value = 1, min = 1, max = input$cutree1)
  })
  
  output$hclust_plot1 <- renderPlot({
    if(input$isolate1) {
      which_val <- input$which_box_clust1
    }
    else {
      which_val <- NULL
    }
      clust1 <- as.dendrogram(clust1_res()$cluster)
      plot(clust1, hang = -1, leaflab = "none", main = clust1_res()$linkage)
      rect.hclust(clust1_res()$cluster, k = input$cutree1, 
                  which = which_val)
  })
  
  output$view_items_clust1 <- renderDataTable(options = list(pageLength = 5), {
    food_use_clust %>% mutate(clust = cutree(clust1_res()$cluster, k = input$cutree1)) %>% 
      filter(clust %in% input$which_box_clust1) %>% select(item)
  })
  
  ## Cluster 2
  clust2_res <- eventReactive(input$conduct_clust2, {
    dist_mat <- dist(food_use_clust %>% select(-X1, -restaurant, -item, -bbq, -bacon, -salad, -crispy, -veg), 
                     method = input$dist1)
    
    if(input$linkage2 %in% "minimax") {
      clust2 <- protoclust(dist_mat)
    }
    else {
      clust2 <- hclust(dist_mat, method = input$linkage2)
    }
    return(list(cluster = clust2, linkage = input$linkage2))
  })
  
  output$isolate_clust2 <- renderUI({
    numericInput("which_box_clust2", "Select cluster", value = 1, min = 1, max = input$cutree2)
  })
  
  output$hclust_plot2 <- renderPlot({
    if(input$isolate2) {
      which_val <- input$which_box_clust2
    }
    else {
      which_val <- NULL
    }
      clust2 <- as.dendrogram(clust2_res()$cluster)
      plot(clust2, hang = -1, leaflab = "none", main = clust2_res()$linkage)
      rect.hclust(clust2_res()$cluster, k = input$cutree2, 
                  which = which_val)
  })
  
  output$view_items_clust2 <- renderDataTable(options = list(pageLength = 5), {
    food_use_clust %>% mutate(clust = cutree(clust2_res()$cluster, k = input$cutree2)) %>% 
      filter(clust %in% input$which_box_clust2) %>% select(item)
  })
  
  ## Interp
  output$counts_by_group_fig <- renderPlot({
    
    if(input$custom_group %in% "") {
      dat <- food_use_clust %>% mutate(clust = cutree(clust1_res()$cluster, k = input$cutree1)) %>% 
        select(clust, group = input$group) %>% 
        count(clust, group) %>% 
        mutate(type = "Method 1") %>% 
        bind_rows(food_use_clust %>% mutate(clust = cutree(clust2_res()$cluster, k = input$cutree2)) %>% 
                    select(clust, group = input$group) %>% 
                    count(clust, group) %>% 
                    mutate(type = "Method 2"))
    }
     
    
    else {
      dat <- food_use_clust %>% mutate(clust = cutree(clust1_res()$cluster, k = input$cutree1),
                                       group = ifelse(str_detect(item, input$custom_group), 
                                                      input$custom_group, 
                                                      paste0("non-", input$custom_group))) %>% 
        select(clust, group) %>% 
        count(clust, group) %>% 
        mutate(type = "Method 1") %>% 
        bind_rows(food_use_clust %>% mutate(clust = cutree(clust2_res()$cluster, k = input$cutree2),
                                            group = ifelse(str_detect(item, input$custom_group), 
                                                           input$custom_group, 
                                                           paste0("non-", input$custom_group))) %>% 
                    select(clust, group) %>% 
                    count(clust, group) %>% 
                    mutate(type = "Method 2"))
    }
      dat %>% 
        ggplot(mapping = aes(x = group, y = n, fill = factor(clust))) + 
        geom_col(width = 0.5, position = "dodge") +
        facet_wrap(~type) +
        labs(fill = "cluster") +
        theme(axis.text = element_text(size = 14), axis.text.x = element_text(angle = 45), axis.title = element_text(size = 14),
            strip.text = element_text(size = 14), legend.text = element_text(size = 14), legend.title = element_text(size = 14))
  })
  
  
  output$custom_plot1 <- renderPlot({
    if(is.null(input$conduct_clust1)) {
      NULL
    }
    ## categorical x (boxplot) -- categorical color only
    else if(input$xvar1 %in% cat_x) {
      if(input$colorvar1 %in% cont_x) {
        ggplot(mapping = aes(y = 0, x = 0)) + 
          geom_text(label = "cannot view categorical 'X' with continuous 'Color'", size = 10) +
          theme(axis.text = element_blank(), axis.ticks = element_blank())
      }
      else {
        food_use %>% mutate(cluster_method_1 = as.character(cutree(clust1_res()$cluster, k = input$cutree1)),
                                  cluster_method_2 = as.character(cutree(clust2_res()$cluster, k = input$cutree2))) %>% 
    select(facet = input$facet_var1, cluster_method_1, cluster_method_2) %>% 
          bind_cols(food_use) %>%
          ggplot(mapping = aes_string(x = input$xvar1, y = input$yvar1, color = input$colorvar1)) + 
          geom_boxplot(varwidth = TRUE) + 
          facet_wrap(~facet, labeller = "label_value") +
          theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), 
                axis.text.x = element_text(angle = 45), 
                legend.text = element_text(size = 14), legend.title = element_text(size = 14),
                strip.text = element_text(size = 14))
      }
    }
    ## continuous x (scatterplot) -- categorical or continuous color
    else if(input$xvar1 %in% cont_x) {
      g1 <- food_use_clust %>% mutate(cluster_method_1 = as.character(cutree(clust1_res()$cluster, k = input$cutree1)),
                                      cluster_method_2 = as.character(cutree(clust2_res()$cluster, k = input$cutree2))) %>% 
        select(facet = input$facet_var1, cluster_method_1, cluster_method_2) %>% 
        bind_cols(food_use_clust) %>% 
        ggplot(mapping = aes_string(x = input$xvar1, y = input$yvar1, color = input$colorvar1)) + 
        geom_point(size = 3) +
        facet_wrap(~facet, labeller = "label_value") +
        theme(axis.text = element_text(size = 14), axis.title = element_text(size = 14), 
              legend.text = element_text(size = 14), legend.title = element_text(size = 14),
              strip.text = element_text(size = 14))
        if(input$colorvar1 %in% cont_x) {
          g1 + scale_color_viridis()
        }
        else {
          g1
        }
    }

  })
  

})