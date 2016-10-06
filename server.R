library(shiny)
library(calibrate)
library(car)
library(clue)
library(flexclust)
library(cluster)
library(ggvis)
library(amap)
library(stringr)
library(clValid)
library(reshape)

shinyServer(function(input, output, session) {
  
  d1 <- read.csv2(file="data/info_1periodas.txt", as.is=TRUE, header=TRUE, row.names=1, check.names=FALSE, quote="")
  d2 <- read.csv2(file="data/info_2periodas.txt", as.is=TRUE, header=TRUE, row.names=1, check.names=FALSE, quote="")
  d3 <- read.csv2(file="data/info_3periodas.txt", as.is=TRUE, header=TRUE, row.names=1, check.names=FALSE, quote="")
  d4 <- read.csv2(file="data/info_4periodas.txt", as.is=TRUE, header=TRUE, row.names=1, check.names=FALSE, quote="")
  d5 <- read.csv2(file="data/info_5periodas.txt", as.is=TRUE, header=TRUE, row.names=1, check.names=FALSE, quote="")
  ds1 <- read.csv2(file="data/info_1periodas_stand.txt", as.is=TRUE, header=TRUE, row.names=1, check.names=FALSE, quote="")
  ds2 <- read.csv2(file="data/info_2periodas_stand.txt", as.is=TRUE, header=TRUE, row.names=1, check.names=FALSE, quote="")
  ds3 <- read.csv2(file="data/info_3periodas_stand.txt", as.is=TRUE, header=TRUE, row.names=1, check.names=FALSE, quote="")
  ds4 <- read.csv2(file="data/info_4periodas_stand.txt", as.is=TRUE, header=TRUE, row.names=1, check.names=FALSE, quote="")
  ds5 <- read.csv2(file="data/info_5periodas_stand.txt", as.is=TRUE, header=TRUE, row.names=1, check.names=FALSE, quote="")
  dr1 <- read.csv2(file="data/info_1periodas_results.txt", as.is=TRUE, header=TRUE, row.names=1, check.names=FALSE, quote="")
  dr2 <- read.csv2(file="data/info_2periodas_results.txt", as.is=TRUE, header=TRUE, row.names=1, check.names=FALSE, quote="")
  dr3 <- read.csv2(file="data/info_3periodas_results.txt", as.is=TRUE, header=TRUE, row.names=1, check.names=FALSE, quote="")
  dr4 <- read.csv2(file="data/info_4periodas_results.txt", as.is=TRUE, header=TRUE, row.names=1, check.names=FALSE, quote="")
  dr5 <- read.csv2(file="data/info_5periodas_results.txt", as.is=TRUE, header=TRUE, row.names=1, check.names=FALSE, quote="")
  
  process <- reactive({
    
    if (input$balskod==1)
      switch(input$periodai,
        "1" = { data <- d1 },
        "2" = { data <- d2 },
        "3" = { data <- d3 },
        "4" = { data <- d4 },
        "5" = { data <- d5 }
      )
    else
      switch(input$periodai,
        "1" = { data <- ds1 },
        "2" = { data <- ds2 },
        "3" = { data <- ds3 },
        "4" = { data <- ds4 },
        "5" = { data <- ds5 }
      )
    
    desc <- data[(nrow(data)-4):nrow(data),]
    data <- data[1:(nrow(data)-5),]
    total <- nrow(data)
    
    # atrenkami balsavimai pagal data
    switch(input$periodai,
      "1" = { date.range.start <- input$datos1[1]; date.range.end <- input$datos1[2] },
      "2" = { date.range.start <- input$datos2[1]; date.range.end <- input$datos2[2] },
      "3" = { date.range.start <- input$datos3[1]; date.range.end <- input$datos3[2] },
      "4" = { date.range.start <- input$datos4[1]; date.range.end <- input$datos4[2] },
      "5" = { date.range.start <- input$datos5[1]; date.range.end <- input$datos5[2] }
    )
    nuo <- as.Date(date.range.start)
    iki <- as.Date(date.range.end)
    dates <- as.Date(unique(data$data))
    if (!(nuo %in% dates)){
      closest <- which.min(abs(nuo-dates))
      while (dates[closest] < nuo)
        closest <- closest + 1
      nuo <- dates[closest]
    }
    if (!(iki %in% dates)){
      closest <- which.min(abs(iki-dates))
      while (dates[closest] > iki)
        closest <- closest - 1
      iki <- dates[closest]
    }
    last.day <- nrow(data[data$data==iki,])
    data <- data[which(data$data == nuo):(which(data$data == iki)+last.day-1),]
    
    # pastaba:
    # pasirinkus didesne data NUO, negu IKI, rodo kreivai - paima tik po pirma krastutiniu dienu balsavima
    output$infokiek <- renderUI({
      a <- paste("Showing ", ncol(data), "/", total, " votings")
      a
    })
    
    # atrenkami balsavimai pagal tema
    if (input$temos1 == 0) { data <- data[data[,ncol(data)] != "1",] }
    if (input$temos2 == 0) { data <- data[data[,ncol(data)] != "2",] }
    if (input$temos3 == 0) { data <- data[data[,ncol(data)] != "3",] }
    if (input$temos4 == 0) { data <- data[data[,ncol(data)] != "4",] }
    if (input$temos5 == 0) { data <- data[data[,ncol(data)] != "5",] }
    if (input$temos6 == 0) { data <- data[data[,ncol(data)] != "6",] }
    if (input$temos7 == 0) { data <- data[data[,ncol(data)] != "7",] }
    
    data <- data[,1:(ncol(data)-4)]
    
    if (input$balskod==1){
      # cia reikes sliderio
      # taip pat galima bus pritaikyti optimizavima, kuomet pakeitus kitu parametru (ne kodavimo) reiksmes,
      # nebutu perkoduojama kiekviena kart is naujo, o rezultatai nuskaitomi is failo
      data[data == 1111] <- input$uz
      data[data == 5555] <- input$neatvyko
      data[data == 4444] <- input$nebalsavo
      data[data == 2222] <- input$susilaike
      data[data == 3333] <- input$pries
      data[data == 6666] <- input$noinfo
    }
    
    data <- t(data)
    data <- na.omit(data)
    
    if (input$mdsatstumas == 1)
      diss <- "manhattan"
    else
      diss <- "euclidean"
    mds <- cmdscale(dist(data, method=diss), eig=FALSE, k=2)
    #final.data <- cbind(mds$points[,1], mds$points[,2], desc)
    final.data <- data.frame(cbind(mds[,1], mds[,2]))
    colnames(final.data) <- c("pirmas", "antras")
    
    descriptive <- data.frame(t(desc))
    colnames(descriptive) <- c("vardas", "frakcijask", "perbegelis", "pozicija", "frakcijazod")
    
    if (input$rodymas == 2){
      switch (input$periodai,
        "1" = { fac.selection <- input$frakcijos1; obs.fac.selection <- input$steb_frakcijos1 },
        "2" = { fac.selection <- input$frakcijos2; obs.fac.selection <- input$steb_frakcijos2 },
        "3" = { fac.selection <- input$frakcijos3; obs.fac.selection <- input$steb_frakcijos3 },
        "4" = { fac.selection <- input$frakcijos4; obs.fac.selection <- input$steb_frakcijos4 },
        "5" = { fac.selection <- input$frakcijos5; obs.fac.selection <- input$steb_frakcijos5 }
      )
      for (i in 1:length(unique(descriptive$frakcijask)))
        if (!(i %in% fac.selection)){
          final.data <- final.data[descriptive$frakcijask != i,]
          descriptive <- descriptive[descriptive$frakcijask != i,]
        }
    }
    
    fill.vect <- rep("gray", nrow(final.data))
    if (input$spalvos == 1){
      # atskiros frakcijos
      for (i in 1:nrow(descriptive))
        switch(toString(descriptive$frakcijazod[i]),
          "TS-LKDF" = { fill.vect[i] <- "#99CC33" },
          "LSDPF" = { fill.vect[i] <- "#FF4455" },
          "LCSF" = { fill.vect[i] <- "#FFCC15" },
          "JF-LCSF" = { fill.vect[i] <- "#FFCC15" },
          "FTT" = { fill.vect[i] <- "#330066" },
          "LSF" = { fill.vect[i] <- "#FF9933" },
          "DPF" = { fill.vect[i] <- "#00CCFF" },
          "MG" = { fill.vect[i] <- "#CCCCCC" },
          "TPPF" = { fill.vect[i] <- "#990000" },
          "TPPF-FVL" = { fill.vect[i] <- "#669966" },
          "AF-TPPF" = { fill.vect[i] <- "#990000" },
          "KPF" = { fill.vect[i] <- "#669966" },
          "LVLS" = { fill.vect[i] <- "#000000" },
          "DKF" = { fill.vect[i] <- "#DF00DF" },
          "LLRAF" = { fill.vect[i] <- "#000000" }
        )
    }
    if (input$spalvos == 2)
      # pozicija/opozicija
      for (i in 1:nrow(descriptive))
        switch(toString(descriptive$pozicija[i]),
          "1" = { fill.vect[i] <- "red" },
          "2" = { fill.vect[i] <- "blue" },
          "3" = { fill.vect[i] <- "gray" }
        )
    if (input$spalvos == 3)
      # parlamentaru stebejimas
      for (i in 1:nrow(descriptive))
        if (descriptive$vardas[i] %in% input$steb_nariai)
          fill.vect[i] <- "red"
    
    if (input$spalvos == 4){
      obs.fac.selection <- input$steb_frakcijos1
      # frakciju stebejimas
      for (i in 1:nrow(descriptive))
        if (toString(descriptive$frakcijazod[i]) %in% obs.fac.selection)
          fill.vect[i] <- "red"
    }
    if (length(fill.vect) < 1)
      fill.vect <- "gray"
    
    stroke.vect <- fill.vect
    if (input$perbegeliai == 2)
      # isskirti tusciaviduriais taskais
      for (i in 1:nrow(descriptive))
        if (descriptive$perbegelis[i] == 2)
          fill.vect[i] <- "#FFFFFF"
    if (input$perbegeliai == 3){
      # nerodyti perbegeliu grafike
      descriptive <- descriptive[final.data$perbegelis != 2,]
      descriptive <- descriptive[descriptive$perbegelis != 2,]
    }
    
    final.data$nr <- 1:nrow(final.data)
    #descriptive$nr <- 1:nrow(descriptive)
    
    point.info.hover <- function(data) {
      if(is.null(data)) return(NULL)
      isol.data <- isolate(descriptive)
      row <- isol.data[isol.data$nr == data$nr,]
      paste("<b>", row$vardas, " </b><br>", row$frakcijazod)
    }
    
    final.data %>% 
      ggvis(~pirmas, ~antras, key := ~nr) %>%
      layer_points(size := input$taskudydis*12.5, size.hover := input$taskudydis*50, stroke := ~stroke.vect, strokeWidth := 2, fill := ~fill.vect, fillOpacity := input$ryskumas*0.1, fillOpacity.hover := 1) %>% 
      add_tooltip(point.info.hover, "hover") %>%
      #add_tooltip(point.info.click, "click") %>%
      add_axis("x", title = "Dimension 1") %>%
      add_axis("y", title = "Dimension 2") %>%
      #add_legend(scales = c("fill"), title = "Simboliai:") %>%
      set_options(width = 550, height = 550, renderer = "canvas", duration = 0)
    
  }) %>% bind_shiny("plot1", "plot1_ui")
  
  output$balsrez <- renderDataTable({
    switch(input$periodai,
      "1" = { rez <- dr1 },
      "2" = { rez <- dr2 },
      "3" = { rez <- dr3 },
      "4" = { rez <- dr4 },
      "5" = { rez <- dr5 }
    )
    rez <- rez[,1:5]
    ids <- rez[,3]
    links <- rez[,5]
    rez <- rez[,-5]
    rez <- rez[,-3]
    rez <- cbind(ids, rez, ids, links)
    for (i in 1:nrow(rez)){
      if (nchar(toString(rez[i,4])) > 39)
        short <- toString(paste(substr(rez[i,4], 1, 40),"...",sep=""))
      else
        short <- toString(rez[i,4])
      rez[i,5] <- short
    }
    for (i in 1:nrow(rez)){
      link <- rez[i,6]
      #link <- HTML(paste('<a href=',link,'>Link</a>',sep=""))
      #link <- 5
      rez[i,6] <- link
      #rez[i,4] <- gsub(" + ", "\n", rez[i,4], fixed = TRUE)
    }
    rez <- data.frame(rez)
    colnames(rez) <- c("ID", "Date", "Time", "Topic(s)", "Name", "Link")
    #cbind(' ' = '&dtrif;', rez)
    rez
  },
  escape = FALSE,
  #rownames = FALSE,
  options = list(
    lengthMenu = list(c(10, 25, 50, 100, -1), c('10', '25', '50', '100', 'All')),
    pageLength = 10,
    bSortClasses = TRUE,
    columnDefs = list(
      list(visible = FALSE, targets = 3),
      list(searchable = FALSE, targets = 1),
      list(orderable = FALSE, className = 'details-control', targets = 0)
      #list(className="alignCenter", targets=c(0,1,2,3,4))
    ),
    processing = TRUE,
    language = list(
      processing = "Processing...",
      zeroRecords = "No results",
      paginate = list(
        'next' = "Next",
        previous = "Previous",
        first = "First",
        last = "Last"
      ),
      search = "Search in table: ",
      loadingRecords = "Loading...",
      info = "Rodomi vartotojai: nuo _START_ iki _END_ is _TOTAL_",
      infoEmpty = "Rodoma 0 vartotoju",
      infoFiltered = "(is viso: _MAX_)",
      lengthMenu = "_MENU_ irasu puslapyje"
    ),
    drawCallback = I("
function(settings) {
    var api = this.api();
    var callback = (function($api) {
        return function() {
            var tr = $(this).parent();
            var row = $api.row(tr);
            if (row.child.isShown()) {
                row.child.hide();
                tr.removeClass('shown');
            }
            else {
                row.child(format(row.data())).show();
                tr.addClass('shown');
            }
        }
    })(api);

    $(this).on('click', 'td.details-control', callback);
}"
    )
  ))
  
})
  