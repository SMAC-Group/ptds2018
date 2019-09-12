library(ptdspkg)

ui <- shinyUI(
  fluidPage(
    title = "R Markdown Playground",
    sidebarLayout(
      sidebarPanel(
        checkboxInput("header", label = "Headers", value = FALSE),
        checkboxInput("lists", label = "Bullet Points", value = FALSE),
        checkboxInput("bolditalic", label = "Emphasis", value = FALSE),
        checkboxInput("p", label = "Paragraph", value = FALSE),
        checkboxInput("table", label = "Tables", value = FALSE),
        checkboxInput("quote", label = "Blockquotes", value = FALSE),
        checkboxInput("link", label = "Links", value = FALSE),
        checkboxInput("pic", label = "Pictures", value = FALSE),
        checkboxInput("code", label = "Code", value = FALSE),
        checkboxInput("emo", label = "Emoji", value = FALSE),
        checkboxInput("gif", label = "Giphy", value = FALSE),
        checkboxInput("video", label = "Video", value = FALSE),
        checkboxInput("plot", label = "Plot", value = FALSE),

        conditionalPanel(
          condition = "input.plot",
          selectInput("align", label = "Figure position:",
                      choices = list("Default" = "default", "Left" = "left", "Right" = "right", "Center" = "center"),
                      selected = "default"),

          numericInput("height", label = "Figure height", value = 7, 1, 12),
          numericInput("width", label = "Figure width", value = 7, 1, 12),
          checkboxInput("caption", label = "Add caption", value = FALSE),

          conditionalPanel(
            condition = "input.caption",
            textInput("cap_text", "Caption text", value = "**Figure:** This is a figure")
          )
      ),

      checkboxInput("ref", label = "Reference", value = FALSE),

      checkboxInput("theme", label = "Change theme", value = FALSE),

      conditionalPanel(
        condition = "input.theme",
        selectInput("theme_select", label = "Select theme",
                    choices = list("default", "cerulean", "journal", "flatly", "readable",
                                   "spacelab", "united", "cosmo", "lumen", "paper", "sandstone",
                                   "simplex", "yeti"),
                    selected = "default")
      ),

      checkboxInput("high", label = "Change syntax highlighting", value = FALSE),

      conditionalPanel(
        condition = "input.high",
        selectInput("high_select", label = "Select style:",
                    choices = list("default", "tango", "pygments", "kate",
                                   "monochrome", "espresso", "zenburn", "haddock", "textmate"),
                    selected = "default")
      ),


      checkboxInput("font", label = "Change font", value = FALSE),

      conditionalPanel(
        condition = "input.font",
        selectInput("font_select", label = "Select font:",
                    choices = list("Agency FB", "Antiqua", "Architect", "Arial",
                                   "BankFuturistic", "BankGothic", "Blackletter",
                                   "Blagovest", "Calibri", "Comic Sans MS",
                                   "Courier", "Cursive", "Decorative", "Fantasy",
                                   "Fraktur", "Frosty", "Garamond", "Georgia",
                                   "Helvetica", "Impact", "Minion", "Modern",
                                   "Monospace", "Open Sans", "Palatino",
                                   "Roman", "Sans-serif", "Serif", "Script",
                                   "Swiss", "Times", "Times New Roman",
                                   "Tw Cen MT", "Verdana"),
                    selected = "Sans-serif")
      ),

      checkboxInput("font_size", label = "Change font size", value = FALSE),

      conditionalPanel(
        condition = "input.font_size",
        sliderInput("font_size_select", "Select size:", 12,
                    min = 8, max = 16)
      )

    ),

      mainPanel(
        tabsetPanel(id = "tabs",
                    tabPanel("Raw code", verbatimTextOutput(outputId = "raw")),
                    tabPanel("Compiled html", uiOutput('html')),
                    tabPanel("BibTex file", verbatimTextOutput(outputId = "bib"))
        )
      )
    )
  )
)
server <- function(input, output) {

  output$raw <- renderPrint({
    make_files(input, rmd = FALSE)
  })

  output$bib <- renderPrint({
    cat("@article{harrar2013taste,\n")
    cat("  title={The taste of cutlery: how the taste of food is affected by \n")
    cat("  the weight, size, shape, and colour of the cutlery used to eat it},\n")
    cat("  author={Harrar, Vanessa and Spence, Charles},\n")
    cat("  journal={Flavour},\n")
    cat("  volume={2},\n")
    cat("  number={1},\n")
    cat("  pages={21},\n")
    cat("  year={2013},\n")
    cat("  publisher={BioMed Central}\n")
    cat("}\n\n\n")

    cat("@article{harrar2011there,\n")
    cat("  title={There's more to taste in a coloured bowl},\n")
    cat("  author={Harrar, Vanessa and Piqueras-Fiszman, Betina and \n")
    cat("  Spence, Charles},\n")
    cat("  journal={Perception},\n")
    cat("  volume={40},\n")
    cat("  number={7},\n")
    cat("  pages={880--882},\n")
    cat("  year={2011},\n")
    cat("  publisher={SAGE Publications Sage UK: London, England}\n")
    cat("}\n")
  })

  output$html <- renderUI({
    make_files(input, rmd = TRUE)
    rmarkdown::render("test.Rmd")
    includeHTML("test.html")
  })
}

shinyApp(ui, server)
