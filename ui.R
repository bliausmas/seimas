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

format.func <- "
<script type='text/javascript'>
function format (d) {
    return '<table cellpadding=\"5\" cellspacing=\"0\" border=\"0\" style=\"padding-left:50px;\">'+
        '<tr>'+
            '<td>Pilnas pavadinimas:</td>'+
            '<td>'+d[3]+'</td>'+
        '</tr>'+
    '</table>';
}
</script>
"

shinyUI(navbarPage(HTML("<font color=#FFFFFF>VOTING OF SEIMAS</font>"), inverse=FALSE, fluid=TRUE,
  
  windowTitle="Voting of Seimas",
  header=tags$head(tags$style(HTML("
    .progress-striped .bar {
      background-color: #089E14;
      background-image: -webkit-gradient(linear, 0 100%, 100% 0, color-stop(0.25, rgba(255, 255, 255, 0.6)), color-stop(0.25, transparent), color-stop(0.5, transparent), color-stop(0.5, rgba(255, 255, 255, 0.6)), color-stop(0.75, rgba(255, 255, 255, 0.6)), color-stop(0.75, transparent), to(transparent));
      background-image: -webkit-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
      background-image: -moz-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
      background-image: -o-linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
      background-image: linear-gradient(45deg, rgba(255, 255, 255, 0.6) 25%, transparent 25%, transparent 50%, rgba(255, 255, 255, 0.6) 50%, rgba(255, 255, 255, 0.6) 75%, transparent 75%, transparent);
      -webkit-background-size: 40px 40px;
      -moz-background-size: 40px 40px;
      -o-background-size: 40px 40px;
      background-size: 40px 40px;
    }
    .navbar {
      margin-bottom: 15px;
      background-color: #11D421;
      font-weight: bold;
      font-size: 16px;
      font-family: verdana;
      color: #FFFFFF;
    }
    .navbar-inner {
      background-color: #11D421;
    }
    .shiny-progress .progress {
      height: 12px;
    }
    .shiny-progress .progress-text {
      position: absolute;
      right: 10px;
      height: 24px;
      width: 380px;
      background-color: #eef8ff;
      margin: 0px;
      padding: 2px 3px;
      opacity: 1;
    }
    .well {
      color: #000000;
      background-color: #CDFAD1;
    }
    .well:hover{
      color: #000000;
      background-color: #CDFAD1;
    }
    .action-button {
      color: #FFFFFF;
      background-color: #11D421;
      width: 200px;
      height: 50px;
      font-size: 16px;
      font-family: verdana;
    }
    .action-button:hover {
      color: #FFFFFF;
      background-color: #059C11;
    }
    body {
      background-color: #FAFAFF;
    }
    .navbar .nav > li > a {
      color: #FFFFFF;
      background-color: #11D421;
    }
    .navbar .nav > li > a:hover {
      color: #FFFFFF;
      background-color: #059C11;
    }
    a {
      font-weight: bold;
    }
    "))),
    
    tabPanel("Seimas dynamics",
      sidebarLayout(
        sidebarPanel(
          tabsetPanel("nust", type="pills",
            tabPanel("Settings","",
              HTML("<br>"),
              helpText("Under construction..."),
              HTML("<br>"),
              helpText("Please check other tabs.")
            )
          )
        ),
      
        mainPanel(
          plotOutput("plot0")
        )
      )
    ),
    
    tabPanel("Voting results",
      dataTableOutput("balsrez"), tags$style(HTML(format.func))
      #, tags$style(".table .alignCenter {color: black; text-align:center;}")
    ),
  
    tabPanel("Voting analysis",
      sidebarLayout(
        sidebarPanel(
          tabsetPanel("nustatymai", type="pills",
            tabPanel("Settings","",
              
              HTML("<br>"),
              div(class="span1", selectInput("periodai", label = strong("Select time period:"),
                choices = list("From 2008-11-17 to 2009-07-15" = 1, "From 2009-07-16 to 2010-01-21" = 2, "From 2010-02-11 to 2010-09-16" = 3, "From 2010-09-21 to 2012-11-14" = 4, "From 2012-11-16 to 2014-03-25" = 5), selected = 1, multiple=FALSE, width=270), align="center"),
              
              HTML("<br>"),
              conditionalPanel(condition = "input.periodai==1",
                div(class="span1",
                  dateRangeInput('datos1',
                    label = strong("Show only votings in period:"),
                    start = "2008-11-17", end = "2009-07-15",
                    min = "2008-11-17", max = "2009-07-15",
                    separator = " - ", format = "yyyy-mm-dd",
                    startview = 'month', language = 'en', weekstart = 1
                  ), align="center"
                )
              ),
              conditionalPanel(condition = "input.periodai==2",
                div(class="span1",
                  dateRangeInput('datos2',
                    label = strong("Show only votings in period:"),
                    start = "2009-07-16", end = "2010-01-21",
                    min = "2009-07-16", max = "2010-01-21",
                    separator = " - ", format = "yyyy-mm-dd",
                    startview = 'month', language = 'en', weekstart = 1
                  ), align="center"
                )
              ),
              conditionalPanel(condition = "input.periodai==3",
                div(class="span1",
                  dateRangeInput('datos3',
                    label = strong("Show only votings in period:"),
                    start = "2010-02-11", end = "2010-09-16",
                    min = "2010-02-11", max = "2010-09-16",
                    separator = " - ", format = "yyyy-mm-dd",
                    startview = 'month', language = 'en', weekstart = 1
                  ), align="center"
                )
              ),
              conditionalPanel(condition = "input.periodai==4",
                div(class="span1",
                  dateRangeInput('datos4',
                    label = strong("Show only votings in period:"),
                    start = "2010-09-21", end = "2012-11-14",
                    min = "2010-09-21", max = "2012-11-14",
                    separator = " - ", format = "yyyy-mm-dd",
                    startview = 'month', language = 'en', weekstart = 1
                  ), align="center"
                )
              ),
              conditionalPanel(condition = "input.periodai==5",
                div(class="span1",
                  dateRangeInput('datos5',
                    label = strong("Show only votings in period:"),
                    start = "2012-11-16", end = "2014-03-25",
                    min = "2012-11-16", max = "2014-03-25",
                    separator = " - ", format = "yyyy-mm-dd",
                    startview = 'month', language = 'en', weekstart = 1
                  ), align="center"
                )
              ),
              uiOutput("infokiek"),
              
              HTML("<br>"),
              div(class="span1", p(strong("Select thematic categories:")), align="center"),
              checkboxInput("temos1", label = img(src="ekonomika.png"), value = 1),
              checkboxInput("temos2", label = img(src="kultura.png"), value = 1),
              checkboxInput("temos3", label = img(src="teisetvarka.png"), value = 1),
              checkboxInput("temos4", label = img(src="socialine.png"), value = 1),
              checkboxInput("temos5", label = img(src="gynyba.png"), value = 1),
              checkboxInput("temos6", label = img(src="valdzia.png"), value = 1),
              checkboxInput("temos7", label = img(src="aplinka.png"), value = 1)
            ),
                                            
            tabPanel("Visualization","",
              
              HTML("<br>"),
              radioButtons("spalvos", label = strong("Coloring:"),
                choices = list("By faction" = 1, "Position/opposition" = 2, "Monitoring of members" = 3, "Monitoring of factions" = 4), selected = 1),
              
              conditionalPanel(condition = "input.spalvos==3",
                selectInput("steb_nariai", label = strong("Highlight members:"),
                  choices = list("Name1 Surname1" = 1, "Name2 Surname2" = 2, "Name3 Surname3" = 3, "Name4 Surname4" = 4), selected = NULL, multiple=TRUE)),
              
              conditionalPanel(condition = "input.spalvos==4&&input.periodai==1",
                selectInput("steb_frakcijos1", label = strong("Highlight factions:"),
                  choices = list("Tevynes sajungos - Lietuvos krikscioniu demokratu frakcija (TS-LKDF, Pozicija)" = "TS-LKDF", "Liberalu sajudzio frakcija (LSF, Pozicija)" = "LSF", "Liberalu ir centro sajungos frakcija (LCSF, Pozicija)" = "LCSF", "Tautos prisikelimo partijos frakcija (TPPF, Pozicija)" = "TPPF", "Lietuvos socialdemokratu partijos frakcija (LSDPF, Opozicija)" = "LSDPF", 'Frakcija "Tvarka ir teisingumas" (FTT, Opozicija)' = "FTT", "Darbo partijos frakcija (DPF, Opozicija)" = "DPF", "Misri Seimo nariu grupe (MG, Kiti)" = "MG"), selected = NULL, multiple=TRUE)),
              conditionalPanel(condition = "input.spalvos==4&&input.periodai==2",
                selectInput("steb_frakcijos2", label = strong("Highlight factions:"),
                  choices = list("Tevynes sajungos - Lietuvos krikscioniu demokratu frakcija (TS-LKDF, Pozicija)" = 3, "Liberalu sajudzio frakcija (LSF, Pozicija)" = 5, "Liberalu ir centro sajungos frakcija (LCSF, Pozicija)" = 2, "Azuolo frakcija - Tautos prisikelimo partijos frakcija (AF-TPPF, Pozicija)" = 6, "Lietuvos socialdemokratu partijos frakcija (LSDPF, Opozicija)" = 1, 'Frakcija "Tvarka ir teisingumas" (FTT, Opozicija)' = 4, "Darbo partijos frakcija (DPF, Opozicija)" = 8, 'Tautos prisikelimo partijos frakcija - Frakcija "Viena Lietuva" (TPPF-FVL, Opozicija)' = 9, "Misri Seimo nariu grupe (MG, Kiti)" = 7), selected = NULL, multiple=TRUE)),
              conditionalPanel(condition = "input.spalvos==4&&input.periodai==3",
                selectInput("steb_frakcijos3", label = strong("Highlight factions:"),
                  choices = list("Tevynes sajungos - Lietuvos krikscioniu demokratu frakcija (TS-LKDF, Pozicija)" = 3, "Liberalu sajudzio frakcija (LSF, Pozicija)" = 6, "Liberalu ir centro sajungos frakcija (LCSF, Pozicija)" = 2, "Tautos prisikelimo partijos frakcija (TPPF, Pozicija)" = 7, "Lietuvos socialdemokratu partijos frakcija (LSDPF, Opozicija)" = 1, 'Frakcija "Tvarka ir teisingumas" (FTT, Opozicija)' = 5, "Darbo partijos frakcija (DPF, Opozicija)" = 10, "Krikscioniu partijos frakcija (KPF, Opozicija)" = 4, "Misri Seimo nariu grupe (MG, Kiti)" = 9, "Lietuvos valstieciu liaudininku sajunga (LVLS, Kiti)" = 10), selected = NULL, multiple=TRUE)),
              conditionalPanel(condition = "input.spalvos==4&&input.periodai==4",
                selectInput("steb_frakcijos4", label = strong("Highlight factions:"),
                  choices = list("Tevynes sajungos - Lietuvos krikscioniu demokratu frakcija (TS-LKDF, Pozicija)" = 3, "Liberalu sajudzio frakcija (LSF, Pozicija)" = 7, "Jungtine frakcija - Liberalu ir centro sajungos frakcija (JF-LCSF, Pozicija)" = 2, "Lietuvos socialdemokratu partijos frakcija (LSDPF, Opozicija)" = 1, 'Frakcija "Tvarka ir teisingumas" (FTT, Opozicija)' = 5, "Darbo partijos frakcija (DPF, Opozicija)" = 9, "Krikscioniu partijos frakcija (KPF, Opozicija)" = 4, "Misri Seimo nariu grupe (MG, Kiti)" = 6, "Lietuvos valstieciu liaudininku sajunga (LVLS, Kiti)" = 8), selected = NULL, multiple=TRUE)),
              conditionalPanel(condition = "input.spalvos==4&&input.periodai==5",
                selectInput("steb_frakcijos5", label = strong("Highlight factions:"),
                  choices = list("Lietuvos socialdemokratu partijos frakcija (LSDPF, Pozicija)" = 1, 'Frakcija "Tvarka ir teisingumas" (FTT, Pozicija)' = 4, "Darbo partijos frakcija (DPF, Pozicija)" = 7, "Lietuvos lenku rinkimu akcijos frakcija (LLRAF, Pozicija)" = 6, "Drasos kelio frakcija (DKF, Pozicija)" = 3, "Tevynes sajungos - Lietuvos krikscioniu demokratu frakcija (TS-LKDF, Opozicija)" = 2, "Liberalu sajudzio frakcija (LSF, Opozicija)" = 5, "Misri Seimo nariu grupe (MG, Kiti)" = 8), selected = NULL, multiple=TRUE)),
              
              HTML("<br>"),
              radioButtons("rodymas", label = strong("Show factions:"),
                choices = list("All" = 1, "Selected" = 2), selected = 1),
              
              conditionalPanel(condition = "input.periodai==1&&input.rodymas==2",
                selectInput("frakcijos1", label = strong("Show only factions:"),
                  choices = list("Tevynes sajungos - Lietuvos krikscioniu demokratu frakcija (TS-LKDF, Pozicija)" = 3, "Liberalu sajudzio frakcija (LSF, Pozicija)" = 5, "Liberalu ir centro sajungos frakcija (LCSF, Pozicija)" = 2, "Tautos prisikelimo partijos frakcija (TPPF, Pozicija)" = 6, "Lietuvos socialdemokratu partijos frakcija (LSDPF, Opozicija)" = 1, 'Frakcija "Tvarka ir teisingumas" (FTT, Opozicija)' = 4, "Darbo partijos frakcija (DPF, Opozicija)" = 8, "Misri Seimo nariu grupe (MG, Kiti)" = 7), selected = NULL, multiple=TRUE)),
              conditionalPanel(condition = "input.periodai==2&&input.rodymas==2",
                selectInput("frakcijos2", label = strong("Show only factions:"),
                  choices = list("Tevynes sajungos - Lietuvos krikscioniu demokratu frakcija (TS-LKDF, Pozicija)" = 3, "Liberalu sajudzio frakcija (LSF, Pozicija)" = 5, "Liberalu ir centro sajungos frakcija (LCSF, Pozicija)" = 2, "Azuolo frakcija - Tautos prisikelimo partijos frakcija (AF-TPPF, Pozicija)" = 6, "Lietuvos socialdemokratu partijos frakcija (LSDPF, Opozicija)" = 1, 'Frakcija "Tvarka ir teisingumas" (FTT, Opozicija)' = 4, "Darbo partijos frakcija (DPF, Opozicija)" = 8, 'Tautos prisikelimo partijos frakcija - Frakcija "Viena Lietuva" (TPPF-FVL, Opozicija)' = 9, "Misri Seimo nariu grupe (MG, Kiti)" = 7), selected = NULL, multiple=TRUE)),
              conditionalPanel(condition = "input.periodai==3&&input.rodymas==2",
                selectInput("frakcijos3", label = strong("Show only factions:"),
                  choices = list("Tevynes sajungos - Lietuvos krikscioniu demokratu frakcija (TS-LKDF, Pozicija)" = 3, "Liberalu sajudzio frakcija (LSF, Pozicija)" = 6, "Liberalu ir centro sajungos frakcija (LCSF, Pozicija)" = 2, "Tautos prisikelimo partijos frakcija (TPPF, Pozicija)" = 7, "Lietuvos socialdemokratu partijos frakcija (LSDPF, Opozicija)" = 1, 'Frakcija "Tvarka ir teisingumas" (FTT, Opozicija)' = 5, "Darbo partijos frakcija (DPF, Opozicija)" = 10, "Krikscioniu partijos frakcija (KPF, Opozicija)" = 4, "Misri Seimo nariu grupe (MG, Kiti)" = 9, "Lietuvos valstieciu liaudininku sajunga (LVLS, Kiti)" = 10), selected = NULL, multiple=TRUE)),
              conditionalPanel(condition = "input.periodai==4&&input.rodymas==2",
                selectInput("frakcijos4", label = strong("Show only factions:"),
                  choices = list("Tevynes sajungos - Lietuvos krikscioniu demokratu frakcija (TS-LKDF, Pozicija)" = 3, "Liberalu sajudzio frakcija (LSF, Pozicija)" = 7, "Jungtine frakcija - Liberalu ir centro sajungos frakcija (JF-LCSF, Pozicija)" = 2, "Lietuvos socialdemokratu partijos frakcija (LSDPF, Opozicija)" = 1, 'Frakcija "Tvarka ir teisingumas" (FTT, Opozicija)' = 5, "Darbo partijos frakcija (DPF, Opozicija)" = 9, "Krikscioniu partijos frakcija (KPF, Opozicija)" = 4, "Misri Seimo nariu grupe (MG, Kiti)" = 6, "Lietuvos valstieciu liaudininku sajunga (LVLS, Kiti)" = 8), selected = NULL, multiple=TRUE)),
              conditionalPanel(condition = "input.periodai==5&&input.rodymas==2",
                selectInput("frakcijos5", label = strong("Show only factions:"),
                  choices = list("Lietuvos socialdemokratu partijos frakcija (LSDPF, Pozicija)" = 1, 'Frakcija "Tvarka ir teisingumas" (FTT, Pozicija)' = 4, "Darbo partijos frakcija (DPF, Pozicija)" = 7, "Lietuvos lenku rinkimu akcijos frakcija (LLRAF, Pozicija)" = 6, "Drasos kelio frakcija (DKF, Pozicija)" = 3, "Tevynes sajungos - Lietuvos krikscioniu demokratu frakcija (TS-LKDF, Opozicija)" = 2, "Liberalu sajudzio frakcija (LSF, Opozicija)" = 5, "Misri Seimo nariu grupe (MG, Kiti)" = 8), selected = NULL, multiple=TRUE)),
              
              HTML("<br>"),
              radioButtons("perbegeliai", label = strong('Show faction changers:'),
                choices = list("Same as other members" = 1, "Highlight by hollow points" = 2, "Hide" = 3), selected = 1),
              
              HTML("<br>"),
              sliderInput("taskudydis", label = strong("Point size:"), min = 1, max = 10, value = 4),
              
              HTML("<br>"),
              sliderInput("ryskumas", label = strong("Point opacity:"), min = 1, max = 10, value = 10)
            ),
            
            tabPanel("Advanced","",
              
              HTML("<br>"),
              radioButtons("mdsatstumas", label = strong("MDS distance:"),
                choices = list("Manhattan" = 1, "Euclidean" = 2), selected = 2),
              
              HTML("<br>"),
              checkboxInput("balskod", label = strong("Recode votes"), value = 0),
              
              conditionalPanel(condition = "input.balskod==1",
                numericInput("uz", label = "Aye:", value = 1),
                numericInput("neatvyko", label = "Absent:", value = 0),
                numericInput("nebalsavo", label = "No vote:", value = 0),
                numericInput("susilaike", label = "Abstain:", value = 0),
                numericInput("pries", label = "No:", value = 0),
                numericInput("noinfo", label = "No info:", value = 0)),
              
              HTML("<br>")
            )
          )
        ),
        
        mainPanel(
          uiOutput("plot1_ui"), ggvisOutput("plot1")
        )
      )
    ),
    
    #tabPanel("Test",
    #  dataTableOutput("testoutput")
    #),
  
    tabPanel("Help",
      tabsetPanel("pagalba",
        tabPanel("Bendra informacija","",uiOutput("bendrainfo")),
        tabPanel("Standartine analize","",uiOutput("standanalize")),
        tabPanel("Specialisto kampelis - Politologui","",uiOutput("politologui")),
        tabPanel("Specialisto kampelis - Statistikui","",uiOutput("statistikui")),
        tabPanel("Parlamentaru stebejimas","",uiOutput("parlsteb")),
        tabPanel("Santrumpos","",
          tabsetPanel("santrumpos",
            tabPanel("Nuo 2008-11-17 iki 2009-07-15","",uiOutput("santrumpos1")),
            tabPanel("Nuo 2009-07-16 iki 2010-01-21","",uiOutput("santrumpos2")),
            tabPanel("Nuo 2010-01-22 iki 2010-09-16","",uiOutput("santrumpos3")),
            tabPanel("Nuo 2010-09-17 iki 2012-11-14","",uiOutput("santrumpos4")),
            tabPanel("Nuo 2012-11-15 iki dabar","",uiOutput("santrumpos5"))
          )
        )
      )
    )
  
))