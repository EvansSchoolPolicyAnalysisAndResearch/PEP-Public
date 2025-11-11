contextUI <- function(id) {
  if(file.exists("www/context.html")) {
    tags$iframe(src = "context.html", width="100%", style="height:70vh; border:none;")
    } else {
    tags$p(style="color: red;", 'Please create "context.html" in the "www" subfolder to add content to this tab.')
  }
}



contextServer <- function(id) {
  moduleServer(id, function(input, output, session){
    #(no content needed here, but some could be added)
  })
}
