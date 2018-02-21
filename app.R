### boxplots for competitive index Shiny app Wojciech Rymaszewski 2017-03-19
library(ggplot2)
library(shiny)
library(XLConnect)

##app
ui = fluidPage(
  includeCSS("styles.css"),
  navbarPage(title = "Competitive index",
    tabPanel("CI Calculation",
             sidebarLayout(
               sidebarPanel(
                 radioButtons("raw_or_ci", label = "Upload..", choices = c('Raw data', "Calculated CI")),
                 fileInput(inputId = "file", label = ""),
                 actionButton("sample","Sample Data")
                ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("Raw data", tableOutput('raw')),
                   tabPanel("Competitive index",
                            downloadButton("download_CI", 'Download CSV'),
                            tableOutput('CI')),
                   tabPanel("Means and Statistics",
                            downloadButton("download_means", 'Download CSV'),
                            tableOutput('means'))
                 )
                   
                 )
               )
             
   ),
    tabPanel("Plotting",
                sidebarLayout(
                   sidebarPanel(
                    textInput(inputId = "colors", label = "Colors", value = "blue, red, green, purple"),
                    sliderInput("minimum", "Min CI", value=-10, min=-20, max=20, step=0.1),
                    sliderInput("maximum", "Max CI", value=10, min=-20, max=20, step=0.1),
                    checkboxInput("dot_dash","Dashed line", value=T),
                    checkboxInput("legend","Legend", value=T),
                    checkboxInput("points","Points", value=T),
                    checkboxInput("means", "Means"),
                    radioButtons("errorbars", "Errorbars", choices = c("SD", "SEM"), selected = 'SD'),
                    sliderInput('aspect_ratio', 'Aspect ratio', value=1, min=0.5, max=2, step=0.01)
                  ),
                  mainPanel(
                    tabsetPanel(
                      tabPanel("Boxplot", plotOutput("boxplot", height = '600px')),
                      tabPanel("Barplot", plotOutput("barplot", height = '600px'))
                    )
                  )
                )
            )
    )
)

server = function(input, output) {
  
  output$download_CI = downloadHandler(
    filename = function() {'Calculated_CI.csv'},
    content = function(file){
      write.csv(normalized(), file)
    }
  )
  output$download_means = downloadHandler(
    filename = function() {'Means.csv'},
    content = function(file){
      write.csv(means(), file)
    }
  )
  wb = reactive({
    if(is.null(input$file) & input$sample<1) {
      return(NULL)
    } else if (input$sample>=1) {
      wb = loadWorkbook('czarny_ci1.xlsx')
    } else {
      wb = loadWorkbook(input$file$datapath)
    }
    wb
  })
  
  
  dat = reactive({
    if(is.null(wb()) | input$raw_or_ci == "Calculated CI") {
      return(NULL)
    } 
    sheet = readWorksheet(wb(), sheet=1, endCol = 6)
    sheet$experiment = as.factor(sheet$experiment)
    sheet$target = as.character(sheet$target)
    sheet$reference = as.character(sheet$reference)
    sheet$dpi = as.factor(sheet$dpi)
    sheet$count_target = as.numeric(sheet$count_target)
    sheet$count_reference = as.numeric(sheet$count_reference)

    if(is.null(input$select_exp)) {
      insertUI (
        selector = "#sample",
        where = "afterEnd",
        ui = textInput("select_exp", "Choose experiments",
                       value = paste(unique(sheet$experiment), collapse = ", ")
        )
      )
    }
    sheet
  })
  
  normalized = reactive({
    if(is.null(dat()) & !input$raw_or_ci == "Calculated CI"){
      return(NULL)
    } else if (input$raw_or_ci == "Calculated CI"){
      sheet = readWorksheet(wb(), sheet=1, endCol = 4)
      sheet$dpi = as.factor(sheet$dpi)
      sheet$name = as.factor(sheet$name)
      sheet$experiment = as.integer(sheet$experiment)
      sheet$CI = as.numeric(sheet$CI)
      norm = sheet                             
    }
    
    if (!is.null(dat())) {
      dat = dat()
      dat$experiment = as.integer(dat$experiment)
      norm = data.frame()
      it = 0
      for (e in unique(dat$experiment)){
        for(i in as.numeric(rownames(dat[!dat$dpi==0 & dat$experiment==e,]))) {
          it = it+1
          targ = dat[i, 'target']
          ref = dat[i, 'reference']
          zero_ci = mean(dat[dat$dpi==0 & dat$experiment==e & dat$target==targ & dat$reference==ref,'count_target']/
                           dat[dat$dpi==0 & dat$experiment==e & dat$target==targ & dat$reference==ref,'count_reference'])
          print (zero_ci)
          norm[it, "experiment"] = e
          norm[it, 'name'] = paste(dat[i,'target'],
                                   dat[i,'reference'], sep='/')
          norm[it, 'dpi'] = dat[i, 'dpi']
          norm[it, 'CI'] = (dat[i, "count_target"]/
                              dat[i, 'count_reference'])/zero_ci
        }
      }
      if(!is.null(input$select_exp)) {
        exps = strsplit(as.character(input$select_exp), split = ', ')[[1]]
        exps = as.numeric(exps)
      }
      norm = norm[norm$experiment %in% exps,]
    }
      norm
  })
  
  means = reactive({
    norm = normalized()
    means = aggregate(norm$CI, by=list(norm$experiment, norm$name, norm$dpi), FUN=mean, na.rm=T)
    means$sd = aggregate(norm$CI, by=list(norm$experiment, norm$name, norm$dpi), FUN=sd, na.rm=T)$x
    means$N = aggregate(norm$CI, by=list(norm$experiment, norm$name, norm$dpi), FUN=length)$x
    means$SEM  = means$sd/sqrt(means$N)
    means = means[,c(1,2,3,4,5,7,6)]
    colnames(means) = c('experiment', 'variants', 'DPI', 'mean', 'SD', 'SEM', 'N')
    means$combo = paste(means$experiment, means$varian, means$DPI, sep=':')
    norm$combo = paste(norm$experiment, norm$name, norm$dpi, sep=':')
    for (i in means$combo){
      means[means$combo==i, 'shap'] = shapiro.test(norm[norm$combo==i, 'CI'])$p.value
      means[means$combo==i, 't-test'] = t.test(norm[norm$combo==i, 'CI'], mu=1)$p.value
      means[means$combo==i, 'wilcox'] = wilcox.test(norm[norm$combo==i, 'CI'], mu=1)$p.value
    }
    means$combo = NULL
    means
  })
  
  output$raw = renderTable({
    if (is.null(dat())) {
      return(NULL)
    }
    dat = dat()
    colnames(dat) = c('Experiment', 'Target', 'Reference', 'DPI', 'Target count', 'Reference count')
    dat
  }, align='c')
  output$CI = renderTable({
    if (is.null(normalized())) {
      return(NULL)
    }
    norm = normalized()
    colnames(norm) = c("Experiment", 'Combination', 'DPI', 'CI')
    norm
  }, align='c')
  output$means = renderTable({
    if (is.null(means())) {
      return(NULL)
    }
    means = means()
    colnames(means) = c('Exp', 'Combination', 'DPI', 'Mean', 'SD', 'SEM', 'N', 'Shapiro', 't-test', 'Wilcoxon')
    means
  }, digits=5, align='c')
  
  
  ready = eventReactive(input$ready_data, {
    ready = read.csv(input$ready_data$datapath, sep=';')
  })
  
  
  output$boxplot = renderPlot({
    if(!is.null(normalized())){
      dat = normalized()
    }
    
    if(input$dot_dash) {
      line = geom_hline(yintercept=1, linetype='dotdash')
    } else{
      line = NULL
    }
    
    print(colnames(dat))
    
    if(input$legend) {
      leg.pos = 'top'
    } else{
      leg.pos = 'none'
    }
    
    if(input$points) {
      points = geom_point(position=position_jitterdodge(dodge.width=0.85), aes(fill=name))
    } else{
      points = NULL
    }
    
    if(input$means) {
      me = stat_summary(fun.y=mean, geom="point", 
                        shape='+', size=6, show_guide = FALSE, aes(group=name),
                        position = position_dodge(width=0.85))
    } else {
      me = NULL
    }
    cols = strsplit(input$colors, split=', ')[[1]]
    
    ggplot(dat, aes(x=dpi, y=CI))+
      line+
      geom_boxplot(aes(fill=name), position = position_dodge(width=0.85), outlier.shape = 1)+
      points+
      me+
      scale_fill_manual(values = cols)+
      scale_y_continuous(breaks=seq(-40, 40, 1))+
      theme(panel.background = element_rect(fill="white"),
            legend.position = leg.pos,
            legend.title = element_blank(),
            axis.line = element_line(color="black"),
            axis.title.y = element_text(size=16),
            axis.title.x = element_text(size=16),
            axis.text = element_text(size=12, color="black"),
            aspect.ratio = input$aspect_ratio)+
      xlab("DPI")+
      ylab("CI")+
      ylim(input$minimum,input$maximum)
  }, res = 110)

  output$barplot = renderPlot({
    if(!is.null(means())){
      means = means()
    }
    
    if(input$dot_dash) {
      line = geom_hline(yintercept=1, linetype='dotdash')
    } else{
      line = NULL
    }
    
    if(input$legend) {
      leg.pos = 'top'
    } else{
      leg.pos = 'none'
    }
    
    if(input$errorbars=='SD') {
      errorbars = geom_errorbar(aes(group = variants, ymin = means$mean-means$SD,
                                    ymax=means$mean+means$SD), width=0.25, 
                                position = position_dodge(width=0.85))
    }
    if(input$errorbars=='SEM') {
      errorbars = geom_errorbar(aes(group = variants, ymin = means$mean-means$SEM,
                                    ymax=means$mean+means$SEM), width=0.25,
                                position = position_dodge(width=0.85))
    }
    cols = strsplit(input$colors, split=', ')[[1]]
    
    ggplot(means, aes(x=DPI, y=mean))+
      line+
      geom_bar(aes(fill=variants), position = "dodge", stat='identity')+
      errorbars+
      scale_fill_manual(values = cols)+
      scale_y_continuous(breaks=seq(-40, 40, 1))+
      theme(panel.background = element_rect(fill="white"),
            legend.position = leg.pos,
            legend.title = element_blank(),
            axis.line = element_line(color="black"),
            axis.title.y = element_text(size=16),
            axis.title.x = element_text(size=16),
            axis.text = element_text(size=12, color="black"),
            aspect.ratio = input$aspect_ratio)+
      xlab("DPI")+
      ylab("CI")+
      ylim(input$minimum,input$maximum)
  }, res = 110)
}

shinyApp(ui = ui, server = server)