server <- function(input, output, session) {
  
  c<- c('pie1','pie2', 'pie3')
  c2 <- c('his1','his2','his3')
  c3 <- c('char1','char2','char3')
  names <- c('Churn customers', 'Non churn customers', 'Loyal customers', 'Long-term cus churn')
  a<- list(churn, no_churn, loyalty)
  
  ###Plot pie chart for churn group
  output[[c[1]]] <- renderPlot({
    select <- a[[1]]
    data <- select %>% group_by(select[[input$var]]) %>% summarise('count'=n()) %>% 
      mutate('proportion'= round(count*100/nrow(select), digits = 2))
    names(data)[1]<- input$var
    data %>% ggplot(aes(x=2, y=proportion, fill= data[[input$var]])) +
      geom_bar(alpha=0.7, stat = 'identity',color='white') +
      coord_polar('y', start = 0, direction = 1) +
      scale_fill_brewer(palette = 'Set1', name= input$var) + 
      theme_void()+ xlim(0.5,2.5) + xlab('') +ylab('')+
      geom_label_repel(aes(y = 100-cumsum(proportion) + proportion/2, label=str_c(proportion,'%'))) +
      ggtitle(names[1]) + theme(plot.title = element_text(size=15,face="bold"),
                                legend.title = element_text(face = 'bold'))
    
  })
  
  output[[c[2]]] <- renderPlot({
    select <- a[[2]]
    data <- select %>% group_by(select[[input$var]]) %>% summarise('count'=n()) %>% 
      mutate('proportion'= round(count*100/nrow(select), digits = 2))
    names(data)[1]<- input$var
    data %>% ggplot(aes(x=2, y=proportion, fill= data[[input$var]])) +
      geom_bar(alpha=0.7, stat = 'identity',color='white') +
      coord_polar('y', start = 0, direction = 1) +
      scale_fill_brewer(palette = 'Set1', name= input$var) + 
      theme_void()+ xlim(0.5,2.5) + xlab('') +ylab('')+
      geom_label_repel(aes(y = 100-cumsum(proportion) + proportion/2, label=str_c(proportion,'%'))) +
      ggtitle(names[2]) + theme(plot.title = element_text(size=15,face="bold"),
                                legend.title = element_text(face = 'bold'))
    
  })
  
  output[[c[3]]] <- renderPlot({
    select <- a[[3]]
    data <- select %>% group_by(select[[input$var]]) %>% summarise('count'=n()) %>% 
      mutate('proportion'= round(count*100/nrow(select), digits = 2))
    names(data)[1]<- input$var
    data %>% ggplot(aes(x=2, y=proportion, fill= data[[input$var]])) +
      geom_bar(alpha=0.7, stat = 'identity',color='white') +
      coord_polar('y', start = 0, direction = 1) +
      scale_fill_brewer(palette = 'Set1', name= input$var) + 
      theme_void()+ xlim(0.5,2.5) + xlab('') +ylab('')+
      geom_label_repel(aes(y = 100-cumsum(proportion) + proportion/2, label=str_c(proportion,'%'))) +
      ggtitle(names[3]) + theme(plot.title = element_text(size=15,face="bold"),
                                legend.title = element_text(face = 'bold'))
  })
  
  
  ###Histogram
  output[[c2[1]]] <- renderPlot({
    select <- a[[1]]
    select %>% ggplot(aes(select[[input$var1]])) +geom_histogram(bins =25,fill='orange', color='black', alpha=0.5) +
      xlab(input$var1) + theme_bw() + ggtitle(names[1]) +
      theme(axis.title = element_text(size=10, face='bold'),
            plot.title = element_text(size=15,face="bold"))
  }) 
  
  
  output[[c2[2]]] <- renderPlot({
    select <- a[[2]]
    select %>% ggplot(aes(select[[input$var1]])) +geom_histogram(bins =25,fill='orange', color='black', alpha=0.5) +
      xlab(input$var1) + theme_bw() + ggtitle(names[2]) +
      theme(axis.title = element_text(size=10, face='bold'),
            plot.title = element_text(size=15,face="bold"))
  }) 
  
  
  output[[c2[3]]] <- renderPlot({
    select <- a[[3]]
    select %>% ggplot(aes(select[[input$var1]])) +geom_histogram(bins =25,fill='orange', color='black', alpha=0.5) +
      xlab(input$var1) + theme_bw() + ggtitle(names[3]) +
      theme(axis.title = element_text(size=10, face='bold'),
            plot.title = element_text(size=15,face="bold"))
    
  })
  
  ###############Chart Tenure
  output[[c3[1]]] <- renderPlot({
    select <- a[[1]]
    
    select %>% ggplot(aes(select[['Tenure']], fill= select[[input$var]])) +
      geom_bar(position='fill', color= 'black', alpha= 0.7)+ 
      scale_fill_brewer(palette = 'Set1', name= input$var) +
      scale_x_continuous(labels = as.character(select[['Tenure']]), breaks = select[['Tenure']])+
      xlab('Tenure') + ylab('proportion')+ theme_bw() + ggtitle(names[1]) +
      
      theme(axis.title = element_text(size=10, face='bold'),
            plot.title = element_text(size=15,face="bold"))
  })
  
  output[[c3[2]]] <- renderPlot({
    select <- a[[2]]
    
    select %>% ggplot(aes(select[['Tenure']], fill= select[[input$var]])) +
      geom_bar(position='fill', color= 'black', alpha= 0.7)+ 
      scale_fill_brewer(palette = 'Set1', name= input$var) +
      scale_x_continuous(labels = as.character(select[['Tenure']]), breaks = select[['Tenure']])+
      xlab('Tenure') + ylab('proportion')+theme_bw() + ggtitle(names[2]) +
      
      theme(axis.title = element_text(size=10, face='bold'),
            plot.title = element_text(size=15,face="bold"))
  })
  
  output[[c3[3]]] <- renderPlot({
    select <- a[[3]]
    
    select %>% ggplot(aes(select[['Tenure']], fill= select[[input$var]])) +
      geom_bar(position='fill', color= 'black', alpha= 0.7)+ 
      scale_fill_brewer(palette = 'Set1', name= input$var) +
      scale_x_continuous(labels = as.character(select[['Tenure']]), breaks = select[['Tenure']])+
      xlab('Tenure') + ylab('proportion') + theme_bw() + ggtitle(names[3]) +
      
      theme(axis.title = element_text(size=10, face='bold'),
            plot.title = element_text(size=15,face="bold"))
  })
  ####Task 2
  output$tab1 <- renderPrint({
    prop.table(table(alpha[[input$var]], alpha$Churn))*100
  })
  
  output$bar <- renderPlot({
    data <- data.frame(prop.table(table(alpha[[input$var]], alpha$Churn), margin=1)*100) 
    names(data)<-c(input$var,'Churn','proportion')
    data$proportion <- round(data$proportion,digits = 2)
    data <- data %>% mutate('xaxis'= ifelse(Churn=='No',1.75,2.25))
    
    ggplot(data, aes(x=xaxis, y= proportion, fill=Churn)) + 
      geom_bar(position='dodge',stat= 'identity')+ facet_grid(.~data[[input$var]]) +
      geom_text(aes(y = proportion+3, label=str_c(proportion,'%'))) +
      ggtitle(input$var) +theme(plot.title = element_text(size=15,face="bold"),
                                axis.title.x = element_blank(),
                                axis.text.x = element_blank(),
                                axis.ticks = element_blank()) 
  })
}

