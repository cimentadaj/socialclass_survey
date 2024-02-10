library(shiny)
library(eutheme)
library(googlesheets4)
library(shinyvalidate)
library(shinyRadioMatrix)
library(rbcb)
library(DIGCLASS)
library(contactdata)
library(shinyjs)

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


row_ids_matrix <- c("You live with:", "You regularly share/pool resources with:", "You sometimes receive income from:")

col_ids_matrix <- c("No one", "Partner", "Children under 16", "Child(ren) 16 or older", "Other relatives (fi. parents)")

ui <- fluidEuTheme(
  tags$head(
    tags$style(
      HTML(
        "        #div_id .selectize-control.single .selectize-input:after{
          content: none;
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
      em(p("Page 1/3")),
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
        radioButtons(
          "workedBefore",
          "Have you worked before?",
          c("Yes", "No"),
          selected = character(0),
          width = "100%"
        )
      ),
      conditionalPanel(
        condition = "input.working == 'Yes' || input.workedBefore == 'Yes'",
        uiOutput("dynamicOccupationQuestion"),
        uiOutput("dynamicEmploymentTypeQuestion"),
        conditionalPanel(
          condition = "input.employmentType == 'I am self-employed and I have employees' || input.employmentType == 'I was self-employed and I had employees' ",
          uiOutput("dynamicNumberEmployees"),
          uiOutput("numberEmployeeError")
        ),
        uiOutput("dynamicWageQuestion"),
        numericInput(
          "careerInterruptions",
          "Approximately, how many career interruptions have you experienced?",
          value = 0,
          min = 0,
          width = "100%"
        ),
        conditionalPanel(
          condition = "input.careerInterruptions > 0",
          numericInput(
            "childrenInterruptions",
            "How many of the total interruptions were to take care of children or other relatives?",
            value = 0,
            min = 0,
            width = "100%"
          ),
          numericInput(
            "unemploymentInterruptions",
            "How many of the total interruptions were due to unemployment?",
            value = 0,
            min = 0,
            width = "100%"
          ),
          uiOutput("interruptionError")
        )
      ),
      actionButton("go_second_page", "Next Page")
    )
  ),
  hidden(
    div(
      id = "second_page",
      em(p("Page 2/3")),
      br(),
      radioButtons(
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
      radioMatrixInput(
        "share_input",
        rowIDsName = "Which of these fits best your current situation?",
        rowIDs = row_ids_matrix,
        rowLLabels = c("", "", ""),
        choices = col_ids_matrix,
        labelsWidth = list("1px", "15px")
      ),
      actionButton("back_first_page", "Back"),
      actionButton("go_third_page", "Next Page")
    )
  ),
  hidden(
    div(
      id = "third_page",
      em(p("Page 3/3")),
      br(),
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
      uiOutput("motherOccupationInput"),
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
      uiOutput("fatherOccupationInput"),
      numericInput(
        "age",
        "How old are you?",
        value = NA,
        min = 0,
        max = 120,
        width = "100%"
      ),
      radioButtons(
        "gender",
        "What is your gender?",
        c("Female", "Male", "Other"),
        selected = character(0),
        width = "100%"
      ),
      selectInput(
        "countryBirth",
        "In which country were you born?",
        c("Choose country" = "", contactdata::list_countries()),
        selectize = TRUE,
        width = "100%"
      ),
      selectInput(
        "countryResidence",
        "In which country do you currently live?",
        c("Choose country" = "", contactdata::list_countries()),
        selectize = TRUE,
        width = "100%"
      ),
      br(),
      actionButton("back_second_page", "Back"),
      actionButton("submit_button", "Submit")
    )
  )
)

server <- function(input, output) {
  observeEvent(input$start_survey, {
    isco08 <- unique(gsub("\\'", "", DIGCLASS::all_labels$isco08[[2]]))

    output$dynamicOccupationQuestion <- renderUI({
      req(input$working)
      if (input$working == "Yes") {
        tags$div(
          id = "div_id",
          selectInput(
            "occupation",
            "What was your last occupation?",
            choices = c("Write down your occupation" = "", isco08),
            selectize = TRUE,
            width = "100%"
          )
        )
      } else if (input$workedBefore == "Yes") {
        tags$div(
          id = "div_id",
          selectInput(
            "occupation",
            "What was your last occupation?",
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
      } else if (input$workedBefore == "Yes") {
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
          value = 1,
          min = 1,
          width = "100%"
        )
      } else if (input$employmentType == "I was self-employed and I had employees") {
        numericInput(
          "numEmployees",
          "How many employees did you have (last job)?",
          value = 1,
          min = 1,
          width = "100%"
        )
      }
    })

    output$dynamicWageQuestion <- renderUI({
      req(input$working)

      # New question for comfort in sharing exact income
      comfortableWithExactIncome <- radioButtons(
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

      if (input$working == "Yes" || input$workedBefore == "Yes") {
        fluidRow(
          column(12, comfortableWithExactIncome),
          conditionalPanel(
            condition = "input.comfortableIncome == 'Yes'",
            column(
              12,
              textInput(
                "monthlyWage",
                income_title,
                placeholder = "€1,500",
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
      if (!is.null(input$motherEducation) && input$motherEducation != "" &&
        input$motherEducation != "Not applicable") {
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
      } else {
        div()
      }
    })

    output$fatherOccupationInput <- renderUI({
      if (!is.null(input$fatherEducation) && input$fatherEducation != "" &&
        input$fatherEducation != "Not applicable") {
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
      } else {
        div()
      }
    })

    observe({
      req(input$careerInterruptions)
      req(input$childrenInterruptions)
      req(input$unemploymentInterruptions)

      totalInterruptions <- input$careerInterruptions
      sumInterruptions <- input$childrenInterruptions + input$unemploymentInterruptions

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

    if (input$careerInterruptions > 0) {
      iv_two$add_rule("unemploymentInterruptions", sv_required("This field is required"))
      iv_two$add_rule("childrenInterruptions", sv_required("This field is required"))
    }

    iv_two$enable()

    req(input$careerInterruptions)
    req(input$childrenInterruptions)
    req(input$unemploymentInterruptions)

    totalInterruptions <- input$careerInterruptions
    sumInterruptions <- input$childrenInterruptions + input$unemploymentInterruptions

    if (iv_two$is_valid() & totalInterruptions == sumInterruptions) {
      hide("first_page")
      show("second_page")
    }
  })

  observeEvent(input$go_third_page, {
    iv_three <- InputValidator$new()
    propertySavingsYesNo <- ifelse(is.null(input$propertySavingsYesNo), "", input$propertySavingsYesNo)

    check_radio <- function(x) {
      null_elements <- Filter(function(x) !is.null(x), x)
      if (length(null_elements) == length(row_ids_matrix)) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }

    iv_three$add_rule("propertySavingsYesNo", sv_required("This field is required"))
    iv_three$add_rule("share_input", sv_required("This field is required", test = check_radio))

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

      # Show a modal dialog after submission
      showModal(
        modalDialog(
          title = "Thank you!",
          "Your response has been submitted successfully! 😊",
          easyClose = TRUE,
          footer = NULL
        )
      )
    }
  })
}

shinyApp(ui = ui, server = server)
