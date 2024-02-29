library(shiny)
library(eutheme)
library(googlesheets4)
library(shinyvalidate)
library(shinyWidgets)
library(shinyRadioMatrix)
library(rbcb)
library(DIGCLASS)
library(contactdata)
library(shinyjs)
library(shiny.fluent)

source("income.R")

# Normalize country names to match the income data
cnt_list <- contactdata::list_countries()
cnt_list <- gsub("US", "United States", cnt_list)
cnt_list <- gsub("UK", "United Kingdom", cnt_list)
cnt_list <- gsub("Turkey", "Turkiye", cnt_list)
cnt_list <- gsub("Gambia", "Gambia, The", cnt_list)
cnt_list <- gsub("Trinidad & Tobago", "Trinidad and Tobago", cnt_list)
cnt_list <- gsub("South Korea", "Korea, Republic of", cnt_list)
cnt_list <- gsub("Bosnia", "Bosnia and Herzegovina", cnt_list)
cnt_list <- gsub("Bahamas", "Bahamas, The", cnt_list)
cnt_list <- gsub("Congo - Kinshasa|Congo - Brazzaville", "Congo, Democratic Republic of the", cnt_list)
cnt_list <- gsub("Côte d’Ivoire", "Cote d'Ivoire", cnt_list)
cnt_list <- gsub("St. Vincent & Grenadines", "Saint Vincent and the Grenadines", cnt_list)
cnt_list <- gsub("St. Lucia", "Saint Lucia", cnt_list)
cnt_list <- gsub("São Tomé & Príncipe", "Sao Tome and Principe", cnt_list)
cnt_list <- cnt_list[!cnt_list %in% c("Palestine", "North Korea")]


check_radio <- function(x) {
  # First check: Ensure no NULL elements and match the length with row_ids_matrix
  null_elements <- Filter(function(x) !is.null(x), x)
  if (length(null_elements) != length(row_ids_matrix)) {
    return("All row options are required")
  }

  # Additional check for the "No one" condition
  for (list_element in x) {
    tmp_el <- unlist(list_element)
    # Check if "No one" is in the list and it's not the only element
    if ("No one" %in% tmp_el && length(tmp_el) > 1) {
      return("You cannot specify 'No one' with other options")
    }
  }

  # If all checks passed
  NULL
}

funFact <- function(image, text, reverse = FALSE) {
  if (!reverse) {
    container <- div(
      class = "fact-container",
      img(src = image, class = "fact-image"),
      div(class = "fact-text", text)
    )
  } else {
    container <- div(
      class = "fact-container",
      div(class = "fact-text", text),
      img(src = image, class = "fact-image")
    )
  }

  div(
    div(class = "catchy-title", "Mind-Blowing Facts!"),
    container
  )
}

# Define a function for creating a small header
headerSection <- function(image, text) {
  container <- div(
    class = "economic-header-container",
    img(src = image, class = "economic-header-image"),
    div(class = "economic-header-text", text)
  )

  div(class = "economic-header", container)
}

generateMultipleCheckDataFrame <- function(row_ids_matrix, col_ids_matrix, input_data) {
  # Generate column names
  cols_multiple_check_df <- sapply(row_ids_matrix, function(q) {
    sapply(col_ids_matrix, function(r) {
      gsub("[:/() ]", "", paste(tolower(gsub(" ", "_", q)), tolower(gsub(" ", "_", r)), sep = "_"))
    })
  }, simplify = "vector")

  # Initialize the data frame
  multiple_check_df <- setNames(
    data.frame(matrix(0, nrow = 1, ncol = length(cols_multiple_check_df))),
    cols_multiple_check_df
  )

  # Populate the data frame based on input_data
  for (question in names(input_data)) {
    for (response in unlist(input_data[[question]])) {
      col_name <- gsub("[:/() ]", "", paste(tolower(gsub(" ", "_", question)), tolower(gsub(" ", "_", response)), sep = "_"))

      if (col_name %in% names(multiple_check_df)) {
        multiple_check_df[1, col_name] <- 1
      }
    }
  }

  return(multiple_check_df)
}

fancyRadioButtons <- function(inputId, label, choices, selected, width = "100%") {
  radioGroupButtons(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    individual = TRUE,
    checkIcon = list(
      yes = tags$i(
        class = "fa fa-circle",
        style = "color: steelblue"
      ),
      no = tags$i(
        class = "fa fa-circle-o",
        style = "color: steelblue"
      )
    ),
    width = width
  )
}


row_ids_matrix <- c("You live with:", "You regularly share/pool resources with:", "You sometimes receive income from:")

col_ids_matrix <- c("No one", "Partner", "Children under 16", "Child(ren) 16 or older", "Other relatives (fi. parents)")

ui <- fluidEuTheme(
  tags$head(
    tags$style(
      HTML(
        "#div_id .selectize-control.single .selectize-input:after{
          content: none;
        }

      .fact-container {
        display: flex;
        align-items: center;
        margin-bottom: 20px;
      }
      .fact-text {
        font-size: 16px;
        color: #333;
        margin-left: 20px;
      }
      .fact-image {
        width: 100px; /* Adjust the image size as needed */
        height: auto;
      }

      .scroll-container {
        overflow-x: auto; /* Enable horizontal scrolling */
        -webkit-overflow-scrolling: touch; /* Smooth scrolling on touch devices */
      }

      .catchy-title {
        font-size: 24px; /* Large font size for emphasis */
        color: #D35400; /* Vibrant orange color for attention */
        background-color: #FDE3A7; /* Soft background to make the text pop */
        text-align: center; /* Center the title for prominence */
        padding: 10px; /* Add some padding around the text */
        border-radius: 8px; /* Rounded corners for a modern look */
        margin-top: 20px; /* Space at the top for breathing room */
        margin-bottom: 20px; /* Space below to separate from content */
        font-family: 'Arial', sans-serif; /* Modern, readable font */
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1); /* Subtle shadow for depth */
      }

      .economic-header {
        text-align: center;
        margin-bottom: 20px;
      }

      .economic-header-container {
        display: inline-flex;
        align-items: center;
        background-color: #f0f0f0; /* Light grey background */
        padding: 10px;
        border-radius: 5px;
      }

      .economic-header-image {
        width: 30px; /* Smaller image size */
        height: auto;
        margin-right: 10px;
      }

      .economic-header-text {
        font-size: 18px; /* Adjusted text size for subheading */
      }
      .social-classes-container {
        display: flex; /* Use flex layout */
        justify-content: center; /* Center items horizontally */
        gap: 20px; /* Adjust the space between class containers */
        align-items: center; /* Align items vertically */
        margin-bottom: 20px; /* Space below the container */
      }

      .class-container {
        display: flex;
        flex-direction: column;
        align-items: center;
        text-align: center; /* Ensure text alignment is centered for labels */
      }

      .class-icon {
        width: 60px; /* Icon width */
        height: 60px; /* Icon height */
        /* 'display: block;' is removed since flexbox handles the alignment */
      }

      .class-label {
        margin-top: 5px; /* Space between icon and label */
      }

      /* Custom styles for the progress bar and income profile text */
      .progress-custom {
        height: 20px;
        background-color: #eee;
        border-radius: 10px;
        margin-top: 20px;
      }

      .progress-bar-custom {
        background-color: #4CAF50;
        transition: width 0.6s ease;
        border-radius: 10px;
      }

      .income-profile-text {
        text-align: center;
        margin-top: 10px;
      }
      /* Responsive font sizes for blockquote */
      blockquote {
        font-size: 1.5em;
      }

      /* Adjust font size for smaller devices */
      @media (max-width: 768px) {
        blockquote {
          font-size: 1.2em; /* Slightly smaller font size for tablets and mobile phones */
        }
      }

      /* Further adjustments for very small screens */
      @media (max-width: 480px) {
        blockquote {
          font-size: 1em;
        }
      }"
      )
    )
  ),
  title = "Social Mobility Survey",
  useShinyjs(),
  br(),
  conditionalPanel(
    condition = "input.start_survey == 0 || typeof(input.start_survey) === 'undefined'",
    titlePanel("Shape the Future of Social Mobility in the European Union"),
    subtitle = div(
      style = "margin-bottom: 20px;",
      "Your Voice Matters: Participate in Our Survey to Help Us Understand and Improve Social Mobility Across EU Countries."
    ),
    img(
      src = "custom_css/images/eu-social-mobility.jpeg",
      style = "width: 100%; max-width: 600px; height: auto; margin-bottom: 20px;"
    ),
    p("We invite you to share your experiences and views on social mobility within the European Union. Your participation will provide invaluable insights for shaping policies that aim to improve opportunities for all."),
    actionButton("start_survey", "Begin Survey Now", class = "btn-primary"),
    hr(),
    h4("Frequently Asked Questions"),
    # Example FAQ - expand as needed
    p("How long will the survey take?", strong("Typically, the survey takes about 10 minutes to complete.")),
    p("Is my survey response confidential?", strong("Yes, your responses are completely anonymous and will be used solely for research purposes.")),
    tags$footer(
      style = "margin-top: 40px;",
      p("This survey is conducted by the European Union. Your feedback is critical for us to understand and enhance social mobility across member states.")
    )
  ),
  hidden(
    div(
      id = "first_page",
      funFact(
        image = "custom_css/images/test.gif",
        text = "Did you know that in Denmark over 80% of low-income kids climb to a higher income bracket, showcasing high social mobility?"
      ),
      hr(),
      br(),
      selectInput(
        "education",
        "What is the highest level of education that you have completed?",
        c(
          "Choose Education" = "",
          "Early childhood education",
          "Primary education",
          "Lower secondary education",
          "Upper secondary education",
          "Post-secondary non-tertiary education",
          "Short-cycle tertiary education",
          "Bachelor’s or equivalent level",
          "Master’s or equivalent level",
          "Doctoral or equivalent level"
        ),
        width = "100%"
      ),
      selectInput(
        "working",
        "Are you currently working?",
        c(
          "Choose Status" = "",
          "Yes",
          "No, I am a full-time student",
          "No, I am on a temporary sick leave",
          "No, I am permanently disabled",
          "No, I am unemployed",
          "No, I am retired",
          "No, I don’t want/need to work"
        ),
        width = "100%"
      ),
      conditionalPanel(
        condition = "input.working != 'Yes' & input.working != ''",
        fancyRadioButtons(
          "workedBefore",
          "Have you worked before?",
          c("Yes", "No"),
          selected = character(0),
          width = "100%"
        )
      ),
      div(
        id = "income_section",
        conditionalPanel(
          condition = "input.working == 'Yes' || input.workedBefore == 'Yes'",
          headerSection(
            image = "custom_css/images/economic_capital.png",
            text = "Economic Capital"
          ),
          uiOutput("dynamicOccupationQuestion"),
          uiOutput("dynamicEmploymentTypeQuestion"),
          conditionalPanel(
            condition = "input.employmentType == 'I am self-employed and I have employees' || input.employmentType == 'I was self-employed and I had employees' ",
            uiOutput("dynamicNumberEmployees"),
            uiOutput("numberEmployeeError")
          ),
          funFact(
            image = "custom_css/images/income_ineq.png",
            text = "Did you know that scientists have confirmed that most social inequality in the world today is due to income inequality?",
            reverse = TRUE
          ),
          uiOutput("dynamicWageQuestion"),
          textInput(
            "careerInterruptions",
            "Approximately, how many career interruptions have you experienced?",
            width = "100%"
          ),
          conditionalPanel(
            condition = "input.careerInterruptions > 0",
            textInput(
              "childrenInterruptions",
              "How many of the total interruptions were to take care of children or other relatives?",
              width = "100%"
            ),
            textInput(
              "unemploymentInterruptions",
              "How many of the total interruptions were due to unemployment?",
              width = "100%"
            ),
            uiOutput("interruptionError")
          )
        )
      ),
      actionButton("go_second_page", "Next Page", class = "btn-primary"),
    )
  ),
  hidden(
    div(
      id = "second_page",
      funFact(
        image = "custom_css/images/income.gif",
        text = "Did you know that only half of the U.S. kids born in the 1980s earn more than their parents, down from 90% in 1940?",
        reverse = TRUE
      ),
      hr(),
      br(),
      fancyRadioButtons(
        "propertySavingsYesNo",
        "Do you have any property or savings?",
        choices = c("Yes", "No"),
        selected = character(0),
        width = "100%"
      ),
      conditionalPanel(
        condition = "input.propertySavingsYesNo == 'Yes'",
        checkboxGroupInput(
          "detailedPropertySavings",
          "If yes, which ones?",
          choices = list(
            "I own the house where I live",
            "I own additional property",
            "I have savings from my own income",
            "I have savings from inheritances or donations"
          ),
          width = "100%"
        )
      ),
      div(
        class = "scroll-container", # Add this container around your widget
        radioMatrixInput(
          "share_input",
          rowIDsName = "Which of these fits best your current situation?",
          rowIDs = row_ids_matrix,
          rowLLabels = c("", "", ""),
          choices = col_ids_matrix,
          labelsWidth = list("1px", "15px")
        )
      ),
      actionButton("back_first_page", "Back"),
      actionButton("go_third_page", "Next Page", class = "btn-primary")
    )
  ),
  hidden(
    div(
      id = "third_page",
      funFact(
        image = "custom_css/images/books.gif",
        text = "Did you know that Vietnamese students beat wealthier countries in global educational tests, showcasing a huge jump in social mobility?",
        reverse = TRUE
      ),
      hr(),
      br(),
      headerSection(
        image = "custom_css/images/demographic_capital.png",
        text = "Demographic Details"
      ),
      selectInput(
        "motherEducation",
        "Highest education of your mother:",
        c(
          "Choose Education" = "",
          "Not applicable",
          "Early childhood education",
          "Primary education",
          "Lower secondary education",
          "Upper secondary education",
          "Post-secondary non-tertiary education",
          "Short-cycle tertiary education",
          "Bachelor’s or equivalent level",
          "Master’s or equivalent level",
          "Doctoral or equivalent level"
        ),
        width = "100%"
      ),
      conditionalPanel(
        condition = "input.motherEducation !== 'Not applicable' && input.motherEducation !== ''",
        uiOutput("motherOccupationInput"),
      ),
      selectInput(
        "fatherEducation",
        "Highest education of your father:",
        c(
          "Choose Education" = "",
          "Not applicable",
          "Early childhood education",
          "Primary education",
          "Lower secondary education",
          "Upper secondary education",
          "Post-secondary non-tertiary education",
          "Short-cycle tertiary education",
          "Bachelor’s or equivalent level",
          "Master’s or equivalent level",
          "Doctoral or equivalent level"
        ),
        width = "100%"
      ),
      conditionalPanel(
        condition = "input.fatherEducation !== 'Not applicable' && input.fatherEducation !== ''",
        uiOutput("fatherOccupationInput"),
      ),
      numericInput(
        "age",
        "How old are you?",
        value = NA,
        min = 0,
        max = 120,
        width = "100%"
      ),
      fancyRadioButtons(
        "gender",
        "What is your gender?",
        c("Female", "Male", "Other", "Prefer not to tell"),
        selected = character(0),
        width = "100%"
      ),
      selectInput(
        "countryBirth",
        "In which country were you born?",
        c("Choose country" = "", cnt_list),
        selectize = TRUE,
        width = "100%"
      ),
      selectInput(
        "countryResidence",
        "In which country do you currently live?",
        c("Choose country" = "", cnt_list),
        selectize = TRUE,
        width = "100%"
      ),
      selectInput(
        "originInfo",
        "The information you just provided reflects:",
        c("Choose" = "", "My own details", "The details belonging to someone I know", "Fictitious details"),
        width = "100%"
      ),
      br(),
      actionButton("back_second_page", "Back"),
      actionButton("submit_button", "Submit", class = "btn-primary")
    )
  )
)

server <- function(input, output, session) {
  observeEvent(input$start_survey, {
    isco08 <- unique(gsub("\\'", "", DIGCLASS::all_labels$isco08[[2]]))

    output$dynamicOccupationQuestion <- renderUI({
      req(input$working)
      if (input$working == "Yes") {
        tags$div(
          id = "div_id",
          selectInput(
            "occupation",
            "What is your current occupation? (Type it instead of scrolling)",
            choices = c("Write down your occupation" = "", isco08),
            selectize = TRUE,
            width = "100%"
          )
        )
      } else if (isTruthy(input$workedBefore) && input$workedBefore == "Yes") {
        tags$div(
          id = "div_id",
          selectInput(
            "occupation",
            "What was your last occupation? (Type it instead of scrolling)",
            choices = c("Write down your occupation" = "", isco08),
            selectize = TRUE,
            width = "100%"
          )
        )
      }
    })

    output$dynamicEmploymentTypeQuestion <- renderUI({
      req(input$working)
      if (input$working == "Yes") {
        selectInput(
          "employmentType",
          "Do you work on your own or for a company (current job)?",
          c(
            "Employment Type" = "",
            "I am self-employed and I don’t have any employees",
            "I am self-employed and I have employees",
            "I am an employee, but I also do some activities on my own",
            "I am an employee"
          ),
          width = "100%"
        )
      } else if (isTruthy(input$workedBefore) && input$workedBefore == "Yes") {
        selectInput(
          "employmentType",
          "Did you work on your own or for a company (last job)?",
          c(
            "Employment Type" = "",
            "I was self-employed and I didn’t have any employees",
            "I was self-employed and I had employees",
            "I was an employee, but I also did some activities on my own",
            "I was an employee"
          ),
          width = "100%"
        )
      }
    })

    output$dynamicNumberEmployees <- renderUI({
      req(input$employmentType)
      if (input$employmentType == "I am self-employed and I have employees") {
        numericInput(
          "numEmployees",
          "How many employees do you have (current job)?",
          value = numeric(0),
          min = 1,
          width = "100%"
        )
      } else if (input$employmentType == "I was self-employed and I had employees") {
        numericInput(
          "numEmployees",
          "How many employees did you have (last job)?",
          value = numeric(0),
          min = 1,
          width = "100%"
        )
      }
    })

    observe({
      if (!is.null(input$comfortableIncome)) {
        print("passed")
        if (input$comfortableIncome == "Yes") {
          updateSelectInput(session, "incomeBrackets", selected = character(0))
          print("incomebrackets")
          print(input$incomeBrackets)
        } else {
          updateTextInput(session, "monthlyWage", value = "")
          print("monthlywage")
          print(input$monthlyWage)
        }
      }
    })

    output$dynamicWageQuestion <- renderUI({
      req(input$working)

      # New question for comfort in sharing exact income
      comfortableWithExactIncome <- fancyRadioButtons(
        "comfortableIncome",
        "Would you feel comfortable sharing your exact monthly income?",
        choices = c("Yes", "No"),
        selected = character(0),
        width = "100%"
      )

      if (input$working == "Yes") {
        income_title <- "What is your monthly wage net of taxes in euros (current job)?"
      } else {
        income_title <- "What was your monthly wage net of taxes in euros (last job)?"
      }

      income_brackets <- c(
        "Select income bracket" = "",
        "Less than €1,000",
        "€1,000 to €1,999",
        "€2,000 to €2,999",
        "€3,000 to €3,999",
        "€4,000 to €4,999",
        "€5,000 to €5,999",
        "€6,000 to €6,999",
        "€7,000 to €7,999",
        "€8,000 to €8,999",
        "€9,000 to €9,999",
        "€10,000 and above"
      )

      # Define income brackets
      incomeBrackets <- selectInput(
        "incomeBrackets",
        income_title,
        choices = income_brackets,
        selected = NULL,
        width = "100%"
      )

      if (input$working == "Yes" || (isTruthy(input$workedBefore) && input$workedBefore == "Yes")) {
        fluidRow(
          column(12, comfortableWithExactIncome),
          conditionalPanel(
            condition = "input.comfortableIncome == 'Yes'",
            column(
              12,
              textInput(
                "monthlyWage",
                income_title,
                placeholder = "1500",
                width = "100%"
              )
            )
          ),
          conditionalPanel(
            condition = "input.comfortableIncome == 'No'",
            column(12, incomeBrackets)
          )
        )
      }
    })

    output$motherOccupationInput <- renderUI({
      tags$div(
        id = "div_id",
        selectInput(
          "motherOccupation",
          "Mother's occupation when you were 15:",
          choices = c("Write down occupation" = "", isco08),
          selectize = TRUE,
          width = "100%"
        )
      )
    })

    output$fatherOccupationInput <- renderUI({
      tags$div(
        id = "div_id",
        selectInput(
          "fatherOccupation",
          "Father's occupation when you were 15:",
          choices = c("Write down occupation" = "", isco08),
          selectize = TRUE,
          width = "100%"
        )
      )
    })

    observe({
      req(input$careerInterruptions)
      req(input$childrenInterruptions)
      req(input$unemploymentInterruptions)

      totalInterruptions <- as.numeric(input$careerInterruptions)
      sumInterruptions <- as.numeric(input$childrenInterruptions) + as.numeric(input$unemploymentInterruptions)

      if (totalInterruptions > 0 && totalInterruptions != sumInterruptions) {
        errorMessage <- "The sum of interruptions for children and unemployment does not match the total reported."
        styledMessage <- paste0('<span style="color: red; font-weight: bold;">', errorMessage, "</span>")
      } else {
        styledMessage <- ""
      }

      output$interruptionError <- renderUI({
        HTML(styledMessage)
      })
    })

    observe({
      req(input$numEmployees)
      if (input$numEmployees < 1) {
        errorMessage <- "You need to specify 1 or more employees"
        styledMessage <- paste0('<span style="color: red; font-weight: bold;">', errorMessage, "</span>")
      } else {
        styledMessage <- ""
      }

      output$numberEmployeeError <- renderUI({
        HTML(styledMessage)
      })
    })
  })

  observeEvent(input$start_survey, {
    show("first_page")
  })

  observeEvent(input$go_second_page, {
    iv_two <- InputValidator$new()

    working <- ifelse(is.null(input$working), "", input$working)
    workedBefore <- ifelse(is.null(input$workedBefore), "", input$workedBefore)
    comfortableIncome <- ifelse(is.null(input$comfortableIncome), "", input$comfortableIncome)
    employmentType <- ifelse(is.null(input$employmentType), "", input$employmentType)

    # Add validation rules
    iv_two$add_rule("education", sv_required("This field is required"))
    iv_two$add_rule("working", sv_required("This field is required"))

    if (working != "Yes" & working != "") {
      iv_two$add_rule("workedBefore", sv_required("This field is required"))
    }

    if (working == "Yes" | workedBefore == "Yes") {
      iv_two$add_rule("occupation", sv_required("This field is required"))
      iv_two$add_rule("employmentType", sv_required("This field is required"))
      iv_two$add_rule("comfortableIncome", sv_required("This field is required"))

      if (comfortableIncome == "Yes") {
        iv_two$add_rule("monthlyWage", sv_required("This field is required"))
      } else {
        iv_two$add_rule("incomeBrackets", sv_required("This field is required"))
      }

      iv_two$add_rule("careerInterruptions", sv_required("This field is required"))

      if (employmentType == "I am self-employed and I have employees" | employmentType == "I was self-employed and I had employees") {
        iv_two$add_rule("numEmployees", sv_required("This field is required"))
      }
    }

    if (input$careerInterruptions != "" && as.numeric(input$careerInterruptions) > 0) {
      iv_two$add_rule("unemploymentInterruptions", sv_required("This field is required"))
      iv_two$add_rule("childrenInterruptions", sv_required("This field is required"))
    }

    iv_two$enable()

    totalInterruptions <- ifelse(input$careerInterruptions == "", 0, as.numeric(input$careerInterruptions))
    childrenInterruptions <- ifelse(input$childrenInterruptions == "", 0, as.numeric(input$childrenInterruptions))
    unemploymentInterruptions <- ifelse(input$unemploymentInterruptions == "", 0, as.numeric(input$unemploymentInterruptions))
    sumInterruptions <- childrenInterruptions + unemploymentInterruptions

    if (iv_two$is_valid() & totalInterruptions == sumInterruptions) {
      hide("first_page")
      show("second_page")
    }
  })

  observeEvent(input$go_third_page, {
    iv_three <- InputValidator$new()
    propertySavingsYesNo <- ifelse(is.null(input$propertySavingsYesNo), "", input$propertySavingsYesNo)


    iv_three$add_rule("propertySavingsYesNo", sv_required("This field is required"))
    iv_three$add_rule("share_input", check_radio)

    if (propertySavingsYesNo == "Yes") {
      iv_three$add_rule("detailedPropertySavings", sv_required("This field is required"))
    }
    iv_three$enable()

    if (iv_three$is_valid()) {
      hide("second_page")
      show("third_page")
    }
  })

  observeEvent(input$back_first_page, {
    hide("second_page")
    show("first_page")
  })

  observeEvent(input$back_second_page, {
    hide("third_page")
    show("second_page")
  })

  observe({
  })

  self_employed <- reactive({
    if (input$working == "Yes") {
      req(input$employmentType)
      if (input$employmentType %in% c("I am self-employed and I don’t have any employees", "I am self-employed and I have employees", "I was self-employed and I didn’t have any employees", "I was self-employed and I had employees")) {
        1
      } else {
        0
      }
    } else {
      0
    }
  })

  n_employees <- reactive({
    req(input$employmentType)
    if (input$employmentType %in% c("I am self-employed and I have employees", "I was self-employed and I had employees")) {
      # Ensure the input$numEmployees is not NULL before using it
      if (!is.null(input$numEmployees) && length(input$numEmployees) != 0) {
        as.numeric(input$numEmployees)
      } else {
        0 # Default value if numEmployees is NULL
      }
    } else {
      0 # Default value if not self-employed with employees
    }
  })

  observeEvent(input$submit_button, {
    # Initialize shinyvalidate
    submit_iv <- InputValidator$new()

    # Conditional validation for dynamic inputs
    motherEducation <- ifelse(is.null(input$motherEducation), "", input$motherEducation)
    fatherEducation <- ifelse(is.null(input$fatherEducation), "", input$fatherEducation)

    if (!motherEducation %in% c("", "Not applicable")) {
      submit_iv$add_rule("motherOccupation", sv_required("This field is required"))
    }

    if (!fatherEducation %in% c("", "Not applicable")) {
      submit_iv$add_rule("fatherOccupation", sv_required("This field is required"))
    }

    submit_iv$add_rule("age", sv_required("This field is required"))
    submit_iv$add_rule("gender", sv_required("This field is required"))
    submit_iv$add_rule("countryBirth", sv_required("This field is required"))
    submit_iv$add_rule("countryResidence", sv_required("This field is required"))
    submit_iv$add_rule("fatherEducation", sv_required("This field is required"))
    submit_iv$add_rule("motherEducation", sv_required("This field is required"))
    submit_iv$enable()

    multiple_check_df <-
      generateMultipleCheckDataFrame(
        row_ids_matrix,
        col_ids_matrix,
        input$share_input
      )

    handleNA <- function(inputField) {
      ifelse(length(inputField) == 0, NA, inputField)
    }

    if (submit_iv$is_valid()) {
      responsesDF <- reactive({
        res <- data.frame(
          Education = handleNA(input$education),
          Working = handleNA(input$working),
          WorkedBefore = handleNA(input$workedBefore),
          Occupation = handleNA(input$occupation),
          EmploymentType = handleNA(input$employmentType),
          NumberOfEmployees = handleNA(input$numEmployees),
          Currency = handleNA(input$currency),
          ComfortableIncome = handleNA(input$comfortableIncome),
          MonthlyWage = handleNA(input$monthlyWage),
          IncomeBrackets = handleNA(input$incomeBrackets),
          CareerInterruptions = handleNA(input$careerInterruptions),
          ChildrenInterruptions = handleNA(input$childrenInterruptions),
          UnemploymentInterruptions = handleNA(input$unemploymentInterruptions),
          PropertySavingsYesNo = handleNA(paste(input$propertySavingsYesNo, collapse = ", ")),
          detailedPropertySavings = handleNA(paste(input$detailedPropertySavings, collapse = ", ")),
          multiple_check_df,
          motherEducation = handleNA(input$motherEducation),
          fatherEducation = handleNA(input$fatherEducation),
          motherOccupation = handleNA(input$motherOccupation),
          fatherOccupation = handleNA(input$fatherOccupation),
          Age = handleNA(input$age),
          Gender = handleNA(input$gender),
          CountryOfBirth = handleNA(input$countryBirth),
          CountryOfResidence = handleNA(input$countryResidence),
          originInfo = handleNA(input$originInfo),
          stringsAsFactors = FALSE
        )

        names(res) <- gsub(":|\\\\", "", tolower(names(res)))
        res
      })

      gs4_auth(path = "./service-account.json")
      sheet_append(
        "https://docs.google.com/spreadsheets/d/1JyvLgsdpJ2WruGIAPqBWFMXxskeF38wjEPs77P4MDoo/edit#gid=0",
        responsesDF()
      )

      income <- NA
      if (!is.null(input$incomeBrackets) && input$incomeBrackets != "") {
        print("brackets not null")
        # Remove the currency symbol and commas
        s_clean <- gsub("[€,]", "", input$incomeBrackets)

        # Check if the string contains "to"
        if (grepl(" to ", s_clean)) {
          # Split the string based on "to" and convert to numbers
          range_parts <- strsplit(s_clean, " to ")[[1]]
          start_num <- as.numeric(range_parts[1])
          end_num <- as.numeric(range_parts[2])
          # Calculate the middle number
          income <- (start_num + end_num) / 2
        } else {
          # For single-value strings, remove non-numeric characters except the decimal point, then convert to numeric
          income <- as.numeric(gsub("[^0-9.]", "", s_clean))
        }
      } else if (!is.null(input$monthlyWage)) {
        print("wage not null")
        cleanedInput <- gsub("[^0-9.]", "", input$monthlyWage)
        income <- as.numeric(cleanedInput)
      }

      income_profile <- process_income_data(input$countryResidence, income)

      occ <- paste0("'", input$occupation, "'")
      isco08_digit <- all_labels$isco08$ISCO08[all_labels$isco08$`ISCO08-label-E` == occ]
      occupation_one_digit <- isco08_swap(isco08_digit, from = 4, to = 1)
      isco08_class <- all_labels$isco08$`ISCO08-label-E`[all_labels$isco08$ISCO08 == occupation_one_digit]

      egp_class <- isco88_to_egp(
        isco08_to_isco88(occupation_one_digit),
        self_employed(),
        n_employees(),
        n_classes = 5,
        label = TRUE
      )

      oesch_class <- isco08_to_oesch(
        occupation_one_digit,
        self_employed(),
        n_employees(),
        n_classes = 5,
        label = TRUE
      )

      oesch_class <- ifelse(is.na(oesch_class), "Couldn't be determined", oesch_class)
      egp_class <- ifelse(is.na(egp_class), "Couldn't be determined", egp_class)
      # Remove roman numerals
      egp_class <- gsub("^.*?\\s", "", egp_class)

      print("classes")
      print(occupation_one_digit)
      print(self_employed())
      print(n_employees())
      print(isco08_class)
      print(egp_class)
      print(oesch_class)
      print(income_profile)

      descr_08 <- read_csv("isco_08_descriptions.csv")
      descr_class <- descr_08$description[descr_08$category == isco08_class]

      if (income_profile$found) {
        income_block <- div(
          headerSection(
            image = "custom_css/images/economic_capital.png",
            text = "Income profile"
          ),
          progressBar(id = "pb", value = income_profile$position * 10, status = "success", size = "s"),
          tags$p(paste("You earn more than", income_profile$below, "% of the population and less than", income_profile$above, "% of the population of", input$countryResidence))
        )
      } else {
        income_block <- div(
          tags$p("We are not able to locate income related information for you country of residence. If you'd like to test this functionality, please choose another country of residence, even as a test.")
        )
      }

      # Show a modal dialog after submission
      showModal(
        modalDialog(
          title = "Social Classes and Income Profile",
          tags$div(
            tags$p("Your response has been submitted successfully! 😊 Below you will find your social class according to three standard class schemas:"),
            tags$ul(
              tags$li(tags$strong("ISCO-2008 classification")),
              tags$li(tags$strong("Oesch classification")),
              tags$li(tags$strong("EGP classification"))
            ),
          ),
          br(),
          tags$div(
            class = "social-classes-container",
            tags$div(
              class = "class-container",
              tags$img(src = "custom_css/images/isco_08.png", class = "class-icon"),
              tags$div(tags$strong(isco08_class), class = "class-label")
            ),
            tags$div(
              class = "class-container",
              tags$img(src = "custom_css/images/oesch.png", class = "class-icon"),
              tags$div(tags$strong(oesch_class), class = "class-label")
            ),
            tags$div(
              class = "class-container",
              tags$img(src = "custom_css/images/egp.png", class = "class-icon"),
              tags$div(tags$strong(egp_class), class = "class-label")
            )
          ),
          br(),
          tags$blockquote(descr_class),
          br(),
          income_block,
          easyClose = TRUE,
          footer = modalButton("Close")
        )
      )
    }
  })
}

shinyApp(ui = ui, server = server)
