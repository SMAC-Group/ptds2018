#' @title Internal function for RMarkdown app
#'
#' @description Create RMD file need for app or print content.
#' @param input A \code{data frame} coming fronm shiny app.
#' @param rmd A \code{boolean} to indicate if the output should be a rmd file or
#' simply print output.
#' @export
make_files = function(input, rmd = FALSE){
  if (rmd){
    file.create("test.txt")
    sink("test.txt")
  }

  cat("---\n")
  cat("title: 'RMarkdown Playground'\n")
  if (input$ref){
    cat("bibliography: biblio.bib\n")
  }
  if ((input$theme && input$theme_select != "default") || (input$high && input$high_select != "default")){
    cat("output:\n")
    cat("   html_document:\n")

    if (input$theme && input$theme_select != "default"){
      cat("      theme: ")
      cat(input$theme_select)
      cat("\n")
    }

    if (input$high && input$high_select != "default"){
      cat("      highlight: ")
      cat(input$high_select)
      cat("\n")
    }

  }else{
    cat("output: html_document\n")
  }
  cat("---\n\n")

  if (input$font || input$font_size){
    cat("<style type='text/css'>\n")

    cat("  body, td {\n")

    if (input$font){
      cat("    font-family: ")
      cat(input$font_select)
      cat(";\n")
    }

    if (input$font_size){
      cat("    font-size: ")
      cat(input$font_size_select)
      cat("px;\n")
    }
    cat("  }\n")

    cat("</style>\n\n\n")
  }


  cat("Experiment with the Rmd Code below and test output.\n\n")

  if (input$ref){
    cat("Here are some examples: @harrar2013taste ; [see also @harrar2011there].\n\n")
  }

  if(input$header){
    cat("# Header Type 1\n## Header Type 2 \n### Header Type 3\n\n")
  }

  if(input$lists){
    cat("* an asterisk starts an unordered list\n")
    cat("* and this is another item in the list\n")
    cat("+ or you can also use the + character\n")
    cat("- or the - character\n\n")

    cat("To start an ordered list, write this:\n\n")

    cat("1. this starts a list *with* numbers\n")
    cat("2. this will show as number '2.'\n")
    cat("33. a wrong number here still shows as number '3.'\n")
    cat("#. the '#' continues the list and shows as number '4.'\n")
    cat("9. any number or '#' will keep the list going.\n")
    cat("    * just indent by 4 spaces (or tab) to make a sub-list\n")
    cat("        1. keep indenting for more sub lists\n")
    cat("    * here i'm back to the second level\n\n")
  }

  if(input$bolditalic){
    cat("Use * or _ to emphasize things:\n\n")
    cat("*this is in italic*  and _so is this_\n\n")
    cat("**this is in bold**  and __so is this__\n\n")
    cat("***this is bold and italic***  and ___so is this___\n\n")
  }

  if(input$p){
    cat("**Paragraph together (on one line):**\n\n")
    cat("When I first brought my cat home from the humane society she was a mangy, pitiful animal. It cost a lot to adopt her: forty dollars. And then I had to buy litter, a litterbox, food, and dishes for her to eat out of. Two days after she came home with me she got taken to the pound by the animal warden. There's a leash law for cats in Fort Collins. If they're not in your yard they have to be on a leash. Anyway, my cat is my best friend. I'm glad I got her. She sleeps under the covers with me when it's cold. Sometimes she meows a lot in the middle of the night and wakes me up, though. (unfocused)\n\n")

    cat("**Paragraph (multiple lines without breaks):**\n\n")

    cat("When I first brought my cat home from the humane society she was a mangy, pitiful animal.\n")
    cat("It cost a lot to adopt her: forty dollars. And then I had to buy litter, a litterbox,\n")
    cat("food, and dishes for her to eat out of. Two days after she came home with me she got taken\n")
    cat("to the pound by the animal warden. There's a leash law for cats in Fort Collins. If \n")
    cat("they're not in your yard they have to be on a leash. Anyway, my cat is my best friend. I'm\n")
    cat("glad I got her. She sleeps under the covers with me when it's cold. Sometimes she meows a \n")
    cat("lot in the middle of the night and wakes me up, though. (unfocused)\n\n")

    cat("**Paragraph (multiple lines with breaks):**\n\n")
    cat("When I first brought my cat home from the humane society she was a mangy, pitiful animal.\n\n")
    cat("It cost a lot to adopt her: forty dollars. And then I had to buy litter, a litterbox,\n\n")
    cat("food, and dishes for her to eat out of. Two days after she came home with me she got taken\n\n")
    cat("to the pound by the animal warden. There's a leash law for cats in Fort Collins. If \n\n")
    cat("they're not in your yard they have to be on a leash. Anyway, my cat is my best friend. I'm\n\n")
    cat("glad I got her. She sleeps under the covers with me when it's cold. Sometimes she meows a \n\n")
    cat("lot in the middle of the night and wakes me up, though. (unfocused)\n\n")

    cat("**Notice that the paragraph on one line and multiple lines without breaks are identical**\n\n")
  }

  if(input$table){
    cat("+---------------+---------------+--------------------+\n")
    cat("| Fruit         | Price         | Advantages         |\n")
    cat("+===============+===============+====================+\n")
    cat("| *Bananas*     | $1.34         | - built-in wrapper |\n")
    cat("|               |               | - bright color     |\n")
    cat("+---------------+---------------+--------------------+\n")
    cat("| Oranges       | $2.10         | - cures scurvy     |\n")
    cat("|               |               | - **tasty**        |\n")
    cat("+---------------+---------------+--------------------+\n\n")
  }

  if(input$quote){
    cat("> Use the > character in front of a line, *just like in email*.\n> Use it if you're quoting a person, a song or whatever.\n >>Add the >> if you want to quote within a quote.\n\n")
  }


  if(input$emo){
    cat("Here are some nice emoji: `r emo::ji('cold_sweat')` `r emo::ji('flushed')` `r emo::ji('scream')`\n\n")
  }


  if(input$gif){
    cat("Here is a big Giphy: \n\n")
    cat("![Some useful legend](https://media.giphy.com/media/3o7TKSha51ATTx9KzC/giphy.gif) \n\n")
    cat("and a smaller one... <img src='https://media.giphy.com/media/5nkIn9AEfUQ6JtXL43/giphy.gif' width='200' height='200' />\n")
  }


  if(input$video){
    cat("Here is a useful video: \n\n")
    cat("<iframe width='560' height='315' src='https://www.youtube.com/embed/WpE_xMRiCLE' frameborder='0' allowfullscreen></iframe>\n\n")
  }




  if(input$link){
    cat("**Links Full:** <https://google.com/>\n\n**Links Custom:** [RMarkdown Cheat Sheet](https://www.rstudio.com/wp-content/uploads/2016/03/rmarkdown-cheatsheet-2.0.pdf)\n\n")
  }

  if(input$pic){
    cat("**Picture (with caption):**\n\n")
    #cat("![Science Department Banner](http://science.psu.edu/psu_eberly_blue.png)\n\n")
    cat("![HEC Lausanne](http://www.unil.ch/logo/files/live/sites/logo/files/web/pdf/lo_unil_hec06_bleu.pdf)\n\n")
    cat("**Picture (without caption):**\n\n")
    cat("![](http://kefalosandassociates.com/wp-content/uploads/2015/07/facebook-banner.png)\n\n")
  }

  if(input$code){
    cat("Inline Code highlighting with `mean(x)`. \n\n")
    cat("```{r calculate_mean, cache = TRUE}\n")
    cat("n = 25\nset.seed(42)\nx = runif(n) # Generates 25 random numbers\n\nmean(x)\n```\n\n")

    cat("If you do not want code to run just use:\n")
    cat("```r\nmean(x)\n```\n\n")
    cat("You can also embed a calculation within the text to show that `r mean(x)` is the mean")
  }

  if(input$plot){
    cat("We will talk more about plots later, but here is an example of changing plot figure options.\n\n")

    #cat("```{r simple_plot}\n")
    #cat("plot(1:10, main = 'Default Plot')\n")
    #cat("```\n\n")


    cat("```{r")
    first_opt = TRUE
    if (input$height != 7){
      if (!first_opt){
        cat(", ")
      }else{
        cat(" ")
      }
      first_opt = FALSE
      cat("fig.height = ")
      cat(input$height)
    }

    if (input$width != 7){
      if (!first_opt){
        cat(", ")
      }else{
        cat(" ")
      }
      first_opt = FALSE
      cat("fig.width = ")
      cat(input$width)
    }

    if (input$align != "default"){
      if (!first_opt){
        cat(", ")
      }else{
        cat(" ")
      }
      first_opt = FALSE
      cat("fig.align = '")
      cat(input$align)
      cat("'")
    }


    if (input$caption){
      if (!first_opt){
        cat(", ")
      }else{
        cat(" ")
      }
      first_opt = FALSE
      cat("fig.cap = '")
      cat(input$cap_text)
      cat("'")
    }

    cat("}\n")

    cat("plot(1:10)\n")
    cat("```\n\n")

    #cat("```{r fig.width = 4, fig.align = 'right', fig.cap = 'Aligned to the right!'}\n")
    #cat("plot(1:10, main = 'Changed Alignment')\n")
    #cat("```\n\n")
  }

  # Last section...
  if (input$ref){
    cat("\n\n# References\n")
  }

  if(rmd){
    sink()
  }

  if (rmd){
    file.rename('test.txt', 'test.Rmd')
  }
}

