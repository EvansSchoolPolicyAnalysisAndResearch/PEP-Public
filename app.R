#options(shiny.error=browser,
#        shiny.trace=F) #For debugging 
options(dplyr.summarise.inform = FALSE,
        shiny.maxRequestSize=50*1024^2)
#options(shiny.useragg = TRUE)
options(sass.cache=FALSE)
#options(shiny.reactlog=TRUE)

#Moved library calls to startup because R is sourced first.

lapply(list.files("Functions", full.names=T), FUN=source) #To do: make the name of this folder more deSCRIPTive



thematic_shiny(
  font = "auto",
  sequential = colorRampPalette(colors = c("white", "#440154FF"))(12),
  qualitative = c("#440154FF",  "#21908CFF", "#3B528BFF", "#5DC863FF", "#FDE725FF")
)


ui <- navbarPage(title="Policy Explorer Platform (PEP)", theme=bslib::bs_theme(version="5", preset='pulse',  #bg = "white", fg = "#3B528B", info="#474481", primary = "#440154FF",
                                                              base_font = bslib::font_google("Open Sans"),
                                                              heading_font=bslib::font_google("Open Sans")), collapsible=T,
                                                              windowTitle="Policy Explorer Platform",
                 header=
                   tags$style(HTML(
                     '
                               .selectize-input.items.full.has-options.has-items {font-size: 1.0em}
                               .shiny-input-select {font-size: 1.0em}
                               .selectize-input {font-size: 1.0em}
                               .radio-group-buttons {font-size: 1.0em}
                               .btn.btn-default.shiny-download-link {--bs-btn-line-height: 0.8; font-size:1.0em}
                               .btn.btn-default.action-button {--bs-btn-line-height: 1.0; font-size:1.0em}
                                '
                   )),
                 tabPanel("Policy Context", icon=icon("signs-post"),
                          contextUI("context") #Assuming this is going to be done in knitr for now

                 ),
                 tabPanel("Identifying Feasible Options: Stakeholders & Decision Criteria", icon=icon("landmark-dome"),
                          tabsetPanel(
                           tabPanel("Policy Instruments by Goal",
                                      policymatUI("policymat")),  #Separating these so they can be moved around (you can also put them in multiple places, just use different ids)
                          tabPanel("Stakeholder Mapping",
                                   stakeholderUI("stakeholdertab"))
                          )
                 ),
                 tabPanel("Gather Evidence & Assess Data", icon=icon("magnifying-glass-chart"),
                          tabsetPanel(
                            tabPanel("Data",
                          evidenceUI("evidence", goalNames, adm_levels, indicator_list), #IDs can be arbitrary so long as the servers use the same ones.
                            ),
                          tabPanel("Literature Evidence",
                                   litUI("evidence_tab"))
                          )
                          #for eventual UI data support
                          #if(interactive()) {
                          # hr(),
                          #  accordion(open=F, accordion_panel("Add Data", addDataUI("addData")))
                          #}
                 ),
                 
                 tabPanel("Interpreting Data Relationships", icon=icon("chart-line"),
                          comparisonsUI("comps", goalNames, year_list, adm_levels)
                 ),
                 
                 
                 tabPanel("Additional Sources for Evaluating Options", icon=icon("database"),
                          secsourcesUI("secsources", ext_sources)
                 ),
                 
                 
                 tabPanel("User Guide and App Diagnostics", icon=icon("stethoscope"),
                          diagnosticsUI("diagnostics")
                 )
                 
)

server <- function(input, output, session) {
  contextServer("context")
  evidenceServer("evidence", globals, shps)
  policymatServ("policymat", pathwaysDT)
  stakeholderServer("stakeholdertab")
  comparisonsServer("comps", globals, territory_names)
  litServer("evidence_tab", ref_sources)
  diagnosticsServer("diagnostics", dataset_list=NULL, indic_inventory, instrument_list=NULL, indicator_list, pathway_link, policy_path) #Null vars are no longer in use and should be pulled
  secsourcesServer("secsources", ext_sources) #Need to do name change on the back end. 
  #if(interactive()) addDataServer("addData")
}

shinyApp(ui = ui, server = server)
