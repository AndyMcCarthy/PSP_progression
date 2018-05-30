library(shiny)
library(shinyBS)
library(plotly)
library(shinyWidgets)
library(shinyjs)
# Define UI for application that draws a histogram


shinyUI(
  
  fluidPage(
    useShinyjs(),
    tags$head(
      tags$style(HTML("
                      @import url('//fonts.googleapis.com/css?family=Josefin+Slab');
                      @import url('//fonts.googleapis.com/css?family=Vollkorn');
                      
                      h4 {
                      font-family: 'Vollkorn';
                      font-weight: 500;
                      line-height: 1.1;
                      color: #000;
                      }
                      h6 {
                      font-family: 'Josefin+Slab';
                      font-weight: 100;
                      line-height: 1;
                      color: #000;
                      font-size: 11pt;
                      }
                      
                      "))
      ),
    
    tags$style(type = "text/css", "
               .irs-bar {width: 70%; height: 25px; background: linear-gradient(to right, white , blue); border-top: 1px solid black; border-bottom: 1px solid black;}
               .irs-bar-edge {background: white; border: 1px solid black; height: 25px; border-radius: 0px; width: 20px;}
               .irs-line {border: 1px solid black; height: 25px; border-radius: 0px;}
               .irs-grid-text {font-family: 'arial'; color: black; font-size: 130%; bottom: 17px; z-index: 1;}
               .irs-grid-pol {display: none;}
               .irs-max {font-family: 'arial'; color: black; background:#ffffff; font-size: 0%;}
               .irs-min {font-family: 'arial'; color: black; background:#ffffff; font-size: 0%;}
               .irs-single {color:black; background:#ffffff; font-size: 100%;}
               .irs-slider {width: 30px; height: 30px; top: 22px;}"),
    
    tags$style(HTML("
                    .tabs-above > .nav > li[class=active] > a {
                    background-color: #000;
                    color: #FFF;
                    }")),
    
    tags$style(type="text/css",
               ".recalculating {opacity: 1.0;}" ),
    tags$style(type="text/css",".pspPlot {padding:0px;}"),
    tags$style(type="text/css",".cbdPlot {padding:0px;}"),
    tags$style(type="text/css",".n3 {margin-left:5px}"),
    tags$head(tags$style( HTML(' .test1 .tab-content {margin-top:-50px;}'))),
    
    sidebarLayout(
      
      sidebarPanel(id = "mysidebar",
        "Subsets and Covariates",
        checkboxInput("useAge", "Include age in analysis (enter age below)", value=FALSE),
        conditionalPanel(
          condition = "input.useAge == true",
          numericInput("age", "Age (50-80):", value= 0)
        ),
        
        checkboxInput("useSex", "Include sex in analysis", value=FALSE),
        conditionalPanel(
          condition = "input.useSex == true",
          radioButtons("sex", "Sex:",
                       c("Female only" = "Female",
                         "Male only" = "Male",
                         "Both" = "N/A"), selected = "N/A")
        ),
        
        checkboxGroupInput("study", "Study to include:",
                           c("4RTNI" = "4RTNI",
                             "TAUROS" = "TAUROS",
                             "DAVENUTIDE" = "DAVENUTIDE",
                             "PROSPERA" = "PROSPERA"), selected = c("4RTNI", "TAUROS", "DAVENUTIDE","PROSPERA")),
        
        
        width = 2
        
      ),
      mainPanel(
      tabsetPanel( id="tabselected",
        tabPanel("Home",
                 fluidRow(
                   column(8, align="centre",style='width:70%; ',
                          h4("Symptomatic Decline in Progressive Supranuclear Palsy and Corticobasal Syndrome: Predictive Models using 4 Clinical Studies and Interactive Visualizations.")
                   )
                 ),
                 fluidRow(
                 column(8, align="centre",style='width:70%; ',
                        h6("This graphic models a large collection of PSP Rating Scale data from completed or ongoing clinical studies to understand better the contribution of symptom domains across the range of disease severity in PSP and CBS.
                           The interactive scale in the bar chart demonstrates the linear progression of domain impairment across disease severity by PSP Rating Scale. On the interactive scale, a patient’s current PSP Rating Scale total score can be selected (bottom slider); this produces the symptom subscores for an average patient at that stage of PSP. Advancing the slider by 11 points on the total rating score (average annual progression) gives a prediction of how a PSP patient may deteriorate over the following 12 months.")
                        )
                 ),
                 fluidRow(
                   column(8, align="centre",style='width:70%; ',
                          h6("A complementary analysis mixed-effects model repeated measures (MMRM) is shown using the observed longitudinal analysis over 12 months was conducted using change from baseline placebo PSP Richardson’s Syndrome patient data from AL-108-231/Davunetide (0, 6, 13, 26, 39 and 52 weeks) and 4RNTI (0, 26 and 52 weeks). Selecting a line allows the user to explore the estimated effect size and associated variability.")
                   )
                 ),
                 fluidRow(
                   column(8, align="centre",style='width:70%;',
                          h4("Source Data")
                          )
                   ),
                 fluidRow(
                   column(8, align="centre",style='width:70%; ',
                          h6("All analyses were conducted on previously collected clinical trial data from 3 completed studies and 1 ongoing study. The 3 completed interventional studies did not demonstrate efficacy greater than placebo, and the drugs are no longer being developed in these indications. However, these studies provide a large systematic collection of data for this rare disease population. Baseline data for all available patients providing a PSP Rating Scale score were included in the baseline modelling from four sources: "),
                          h6("\n1.	AL-108-231 (Clinicaltrials.gov, number NCT01110720) - A trial for davunetide in patients with PSP[6]; data for 304 patients were obtained over 52 weeks. Primary endpoints were the change from baseline in the PSP Rating Scale and the Schwab and England Activities of Daily Living (SEADL) scale."),
                          h6("\n2.	PROSPERA (Clinicaltrials.gov, number NCT01187888) - Evaluating the safety, tolerability, and therapeutic effect of rasagiline on symptom progression in 44 patients with PSP over a year.[12] Primary endpoints included symptom progression as measured by the PSP Rating Scale."),
                          h6("\n3.	TAUROS (Clinicaltrials.gov, number NCT01049399) - Assessing the efficacy, safety, and tolerability of tideglusib, as potential treatment for PSP.[5] The 52-week study enrolled 146 PSP patients with mild-to-moderate disease. The primary endpoint was the change from baseline on the PSP Rating Scale.
                             "),
                          h6("\n4RNTI (Clinicaltrials.gov, numbers NCT01804452 and NCT02966145) – A non-intervention 12-month study to identify the best methods of analysis for tracking PSP and CBS over time; 73 PSP patients and 49 CBS patients are included. At the time of writing, the latter part of this study (4RNTI-2) is still ongoing.
                             Studies followed approximately the same inclusion criteria, requiring a probable diagnosis of PSP, similar age ranges, and a requirement for patients to be able to walk independently or with minimal help. TAUROS, AL-108-231, and 4RNTI did not specify an inclusion range on the PSP Rating Scale, whereas PROSPERA required patients to have a baseline score of less than 40 as they were targeting a milder subset of diagnoses. 4NRTI also included CBS patients with a probable CBD diagnosis. Many thanks to the study investigators for allowing us the use of this valuable data.
                             ")
                          )
                   )
        ),
        
        tabPanel("PSP and CBD using PSP-RS",
                 fluidRow(
                   column(5, offset=1, align="centre",style='width:40%; height: 30px',
                          h4("Progressive supranuclear palsy")
                   ),
                   column(5, align="centre",style='width:30%;height: 30px',
                          h4("Corticobasal degeneration")
                   )
                 ),
                 fluidRow(
                   column(5, align="right",style='padding:0px;',
                          plotOutput("pspPlot", width = "75%", height = "380px", click="plot_hover"),
                          uiOutput("hover_info", style = "color:black; background:#ffffff;")
                          
                          
                   ),
                   
                   column(5, align="left",style='margin-left:0px;',
                          plotOutput("cbdPlot", width = "80%", height = "380px", click="cbdplot_hover"),
                          uiOutput("hover_cbd", style = "color:black; background:#ffffff;")
                          
                          
                   ),
                   column(2, align="right",style='padding:0px;',
                          img(src='Legend.png', align = "right"))
                 ),
                 fluidRow(
                   column(11,offset = 2, align="left", 
                          sliderInput("n", "PSP-RS Score", 0, 100,
                                      value = 0, step = 1, width="60%",animate= animationOptions(interval=200))),
                   
                   column(11,offset =2, align="centre", uiOutput("disease_severity", align="centre", style = "color:black; font-weight: bold; width:100%;")),
                   
                   
                   style = "padding: 5px;",
                   
                   bsTooltip("n", "The Progressive supranuclear palsy Rating Scale: 0 to 24 - Mild disease; 25 to 45 - Moderate disease; 45+ - Severe disease",
                             "right", options = list(container = "body", delay=1000))
                 )
        ),
        
        tabPanel("PSP using CGI",
                 
                 fluidRow(
                   column(5,offset = 2, align="centre",style='width:40%; height: 30px',
                          h4("Progressive supranuclear palsy")
                   )
                 ),
                 
                 
                 fluidRow(
                   
                   column(5,offset = 2, align="centre",style='padding:0px;',
                          plotOutput("cgiPlot",  click="cgiplot_hover"),
                          uiOutput("hover_cgi", style = "color:black; background:#ffffff;")
                          
                          
                   ),
                   column(2,offset=2, align="right",style='padding:0px;',
                          img(src='Legend.png', align = "right")),
                   
                   fluidRow(
                     
                     column(7,offset = 3, align="centre",style='padding:0px;',
                            sliderInput("n2", "Clinical Global impression severity score", 0, 7,
                                        value = 0, step = 1, width="65%",animate= animationOptions(interval=200))),
                     
                     column(7,offset =3, align="centre", uiOutput("cgi_severity", align="centre", style = "color:black; font-weight: bold; width:100%;")),
                     
                     
                     bsTooltip("n2", "The Clinical Global impression severity score: 0 to 2 - Mild disease; 2 to 4 - Moderate disease; 4+ - Severe disease",
                               "right", options = list(container = "body", delay=1000))
                   )
                 )
                 
        ),
        
        tabPanel("PSP using SEADL",
                 
                 fluidRow(
                   column(5,offset = 2, align="centre",style='width:40%; height: 30px',
                          h4("Progressive supranuclear palsy")
                   )
                 ),
                 
                 
                 fluidRow(
                   
                   
                   #width = "120%", height = "450px",
                   column(5,offset = 2, align="centre",style='padding:0px;',
                          plotOutput("seadlPlot",  click="seadlplot_hover"),
                          uiOutput("hover_seadl", style = "color:black; background:#ffffff;")
                   ),
                   
                   column(2, offset=2, align="right",style='padding:0px;',
                          img(src='Legend.png', align = "right")),
                   
                   fluidRow(
                     
                     column(7,offset = 3, align="centre",style='padding:0px;',
                            #sliderInput("n3", "Schwab & England Activity daily Living scale", 0, 100,
                            #            value = 0, step = 1, width="65%",animate= animationOptions(interval=200)),
                            sliderTextInput(
                              inputId = "n3",
                              label = "Schwab & England Activity daily Living scale",
                              animate= animationOptions(interval=200),
                              choices = c(100:0),
                              grid = FALSE,
                              width="65%"
                            )#,
                            #verbatimTextOutput(outputId = "result")
                            ),
                     
                     column(7,offset =3, align="centre", uiOutput("seadl_severity", align="centre", style = "color:black; font-weight: bold; width:100%;")),
                     
                     bsTooltip("n3", "The Schwab & England Activity daily Living scale. 0 to 24 - Severe disease; 25 to 44 - Moderate disease; 45+ - Mild disease",
                               "right", options = list(container = "body", delay=1000))
                   )
                 )
                 
        ),
        
        tabPanel("MMRM", "Longitudinal Model of PSP Rating Scale Subscores and Total Score of Placebo PSP-Richardson’s Syndrome Patients (AL-108-231 and 4RNTI)",
                 plotlyOutput("mmrmPlot")       
        ),
        
        tabPanel("PMOM", "Proportional Odds Models – Probability of Start of Mild or Severe Impairment for Each PSP Rating Scale Domain Subscore as PSP Rating Scale Total Score Worsens",
                 plotlyOutput("proportionaloddsPlot"),
                 selectInput('POMSeverity', 'Options', c("Mild/No impairment", "Severe impairment" ), selectize=TRUE)
        )
        
      )
      )
      
      
      
    )
    )
  
  
  
  
    )





