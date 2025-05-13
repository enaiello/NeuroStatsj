SmartInfo <- R6::R6Class(
    "SmartInfo",
    class = TRUE,
    cloneable = FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
    public = list(
        interface = "jamovi",
        caller        = NULL,
        infotab       = NULL,
        extrainfotab  = NULL,
        infotag       = NULL,
        extratag      = NULL,
        infovec       = NULL,
        extravec      = NULL,
        linkvec       = NULL,
        initialize = function(jmvobj) {
            self$infotab       <- jmvobj$results$info
            self$extrainfotab  <-jmvobj$results$extrainfo
            self$caller        <- jmvobj$options$.caller

            if (".interface" %in% names(jmvobj$results$options)) {
                self$interface <- jmvobj$results$options$.interface
            }
        },
        info = function() {
          
     
         text <- paste(
             "<style>",
             ".accordion {",
             "  background-color: #3498db;",
             "  color: white;",
             "  cursor: pointer;",
             "  padding: 8px 15px;",
             "  width: 100%;",
             "  border: none;",
             "  text-align: left;",
             "  outline: none;",
             "  font-size: 16px;",
             "  transition: 0.4s;",
             "  display: flex;",
             "  align-items: center;",
             "  position: relative;",
             "  border-top-left-radius: 8px;",
             "  border-top-right-radius: 8px;",
             "}",
             ".accordion svg {",
             "  margin-right: 15px;",
             "  transition: fill 0.4s;",
             "}",
             ".accordion svg .circle {",
             "  fill: white;",
             "}",
             ".accordion svg .horizontal,",
             ".accordion svg .vertical {",
             "  fill: #3498db;",
             "  transition: transform 0.8s ease-in-out;",
             "  transform-origin: center;",
             "}",
             ".accordion.active svg .vertical {",
             "  transform: scaleY(0);",
             "}",
             ".panel {",
             "  padding: 0 15px;",
             "  display: none;",
             "  background-color: white;",
             "  overflow: hidden;",
             "}",
             "</style>",
             "<script>",
             'var acc = document.getElementsByClassName("accordion");',
             "for (var i = 0; i < acc.length; i++) {",
             '  acc[i].addEventListener("click", function() {',
             '    this.classList.toggle("active");',
             "    var panel = this.nextElementSibling;",
             '    if (panel.style.display === "block") {',
             '      panel.style.display = "none";',
             "    } else {",
             '      panel.style.display = "block";',
             "    }",
             "  });",
             "}",
             "</script>",
             '<button class="accordion">',
             '  <svg width="20" height="18" viewBox="0 0 24 24">',
             '    <circle class="circle" cx="12" cy="12" r="11" />',
             '    <rect class="horizontal" x="5" y="11" width="15" height="3" />',
             '    <rect class="vertical" x="11" y="5" width="3" height="15" />',
             "  </svg>",
             '  <span style="font-size: 14px;">Info</span>',
             "</button>",
             '<div class="panel">',
             "{addinfo}",
             "{addinfo2}",
             "{help}",
             "</div>"
         )
         if (is.null(self$infotag)) self$infotag<-self$caller
       
    addinfo   <- self$infovec[[self$infotag]]
    addinfo2  <- self$extravec[[self$extratag]]
    help      <-  private$.link_help() 
    txt<-jmvcore::format(text, addinfo = addinfo, addinfo2 = addinfo2, help = help)
    self$infotab$setContent(txt)
    self$infotab$setVisible(TRUE)
    }
    ), ## end of public
    private= list(
      
      .link_help = function() {
        
          text <- NULL
          link <- self$linkvec[[self$infotag]]

    
          if (is.something(link)) {
            text <- "<p style='display: flex; align-items: center;'> " %+%
                    "<span style=' display:inline-block; text-align: center;" %+%
                    "width:16px; height:16px; border: 3px solid green; border-radius: 50%;padding:3px; padding-bottom:3px; margin-right:8px;" %+%
                    "font-weight: bolder'>" %+%
                    "i</span><span> Help can be found <a href='" %+% link %+% "' target='_blank'> Pamlj web manual.</a> <span></p>"
    }
    return(text)
    }

    ) # end of private
) # end of class
      
      
