## Built by Edouardo Honig

library(shiny)

# if (interactive()) {

load_wordle_data <- function(file_name) {
  unname(unlist(read.delim(file_name, header = FALSE, sep = ",")))
}

# load og wordle sols
os <- load_wordle_data("wordle_old_sols.txt")
# load og wordle choices
oc <- load_wordle_data("wordle_old_choices.txt")
# # load nyt wordle (02/18/2022) sols
# ns <- load_wordle_data("wordle_nyt_sols.txt")
# # load nyt wordle choices
# nc <- load_wordle_data("wordle_nyt_choices.txt")

word_list <- c(os, oc)

# validate guess
validate_guess <- function(word, choices = word_list) {
  nchar(word) == 5 && !grepl("[^A-Za-z]", word) && tolower(word) %in% word_list
}
  
# get guess
get_chrs <- function(word) {
  strsplit(toupper(word), "")[[1]]
}

# check validity of word input as character vector of single-strings
check_wordle <- function(guess, soln, colors_wordle = list(
  gray = "#3b3b3c",
  green = "#528d4e",
  gold = "#b49e3b")
) {
  pos <- guess == soln
  soln[pos] <- colors_wordle$green                # green
  chr <- sapply(unique(soln), function(i) which(i == guess))
  chr <- chr[lengths(chr) != 0]
  if (length(chr) != 0) {                         # gold
    chr_freq <- sapply(unique(soln), function(i) sum(i == soln))
    set_gold <- sapply(names(chr), function(i) chr[[i]][chr_freq[[i]]])
    soln[set_gold] <- colors_wordle$gold          
  }
  soln[nchar(soln) == 1] <- colors_wordle$gray    # gray
  soln
}

# plot a guess
plot_guess <- function(guess, soln, round) {
  round <- 7 - round
  points(1:5, rep(round, 5), pch = 15, col = check_wordle(guess, soln), cex = 4)
  points(1:5, rep(round, 5), pch = pch_mapping[guess], col = white, cex = 2)
}

# letter mapping for pch for plot
pch_mapping <- 65:90
names(pch_mapping) <- LETTERS

# color mapping for plot
white <- "#fefeff"
black <- "#121312"
gray <- "#3b3b3c"
green <- "#528d4e"
gold <- "#b49e3b"

jscode_enter <- '
$(function() {
  var $els = $("[data-proxy-click]");
  $.each(
    $els,
    function(idx, el) {
      var $el = $(el);
      var $proxy = $("#" + $el.data("proxyClick"));
      $el.keydown(function (e) {
        if (e.keyCode == 13) {
          $proxy.click();
        }
      });
    }
  );
});
'

ui <- fluidPage(
  tags$style('body {
                             background-color: #121312;
              }'),
  tags$head(tags$script(HTML(jscode_enter))),
  titlePanel(""),
  fluidRow(
    align = "center",
    column(12,
           tagAppendAttributes(
             span(textInput("guess", "Enter your guess"), style = "color:#fefeff"),
             `data-proxy-click` = "guessButton"),
    column(12,
      actionButton("guessButton", "Enter Guess", class = "btn-success"),
        span(textOutput("message"), style = "color:#fefeff"),
    column(12, 
           plotOutput("plot")
        )
      )
    )
  )
)

server <- function(input, output) {
  soln <- get_chrs(os[abs(19047 - as.numeric(Sys.Date())) %% length(os) + 1])
  guesses <- character(6)
  guess_num <- 0L
  
  gameOver <- reactiveVal(FALSE)
  
  guessEvent <- eventReactive(input$guessButton, {
    # validate(
    #   need(validate_guess(input$guess), "Please enter a valid input"),
    #   need(guess_num < 6, "Only allowed six guesses")
    # )
    if (gameOver()) {
      guess_num <<- 0L
      guesses <<- character(6)
      soln <<- get_chrs(sample(os, 1))
      gameOver(FALSE)
      renderText({"New woRdle generated"})
    } else 
      if (!validate_guess(input$guess)) {
      renderText({"Invalid input"})
    } else if (guess_num > 5) {
      guess_num <<- 0L
      guesses <<- character(6)
      soln <<- get_chrs(sample(os, 1))
      gameOver(FALSE)
      renderText({paste0("The woRdle was: ", paste(soln, collapse = ""), "; new woRdle generated")})
    } else {
      guess_num <<- guess_num + 1L
      guesses[guess_num] <<- input$guess
      if (all(get_chrs(input$guess) == soln)) {
        gameOver(TRUE)
        renderText({paste("Congratulations! You found the woRdle")})
      } else if (guess_num != 5) {
        renderText({paste(6L - guess_num, "guesses remaining")})
      } else {
        renderText({paste("1 guess remaining")})
      }
    }
  })
  output$plot <- renderPlot(bg = black, {
    output$message <<- if (input$guessButton != 0) guessEvent() else renderText({"Welcome to woRdle"})
    par(fin = c(3.5, 4), mar = c(0, 2, 1, 2), col.main = white)
    plot(rep(1:5, 6), rep(6:1, each = 5), pch = 0, cex = 3.8,
         main = "woRdle", col = gray,
         axes = FALSE, xlab = "", ylab = "", xlim = c(0, 6), ylim = c(0.5, 6.5))
    for (i in seq_len(guess_num)) {
      plot_guess(get_chrs(guesses[i]), soln, i)
    }
  })
}

shinyApp(ui, server)

# }

