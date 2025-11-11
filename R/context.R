contextUI <- function(id) {
  if(file.exists("www/context.html")) {
    tags$iframe(src="context.html",
                #width = "100%",
                style = "width:100%; height: 80vh; overflow: auto;",
                #frameBorder = "0",
                scrolling="auto"
                )
    #includeHTML("www/context.html")
  } else {
    tags$p(style="color: red;", 'Please create "context.html" in the "www" subfolder to add content to this tab.')
  }
}



contextServer <- function(id) {
  moduleServer(id, function(input, output, session){
    #Server content isn't necessary here, but advanced users may want to add some functionality. 
  })
}
