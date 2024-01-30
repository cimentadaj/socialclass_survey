library(shiny)
library(eutheme)
library(googlesheets4)
library(shinyvalidate)
library(shinyRadioMatrix)
library(rbcb)
library(DIGCLASS)
library(contactdata)

ui <- fluidEuTheme(
  br(),
  selectInput(
    "education",
    "1. What is the highest level of education that you have completed?",
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
    "2. Are you currently working?",
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
      "3. Have you worked before?",
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
      "7. Approximately, how many career interruptions have you experienced?",
      value = 0,
      min = 0,
      width = "100%"
    ),
    conditionalPanel(
      condition = "input.careerInterruptions > 0",
      numericInput(
        "childrenInterruptions",
        "7.1 How many of the total interruptions were to take care of children or other relatives?",
        value = 0,
        min = 0,
        width = "100%"
      ),
      numericInput(
        "unemploymentInterruptions",
        "7.2 How many of the total interruptions were due to unemployment?",
        value = 0,
        min = 0,
        width = "100%"
      ),
      uiOutput("interruptionError")
    )
  ),
  checkboxGroupInput(
    "propertySavings",
    "8. Do you have any property or savings?",
    choices = list(
      "None",
      "I own the house where I live",
      "I own additional property",
      "I have savings from my own income",
      "I have savings from inheritances or donations"
    ),
    width = "100%"
  ),
  radioMatrixInput(
    "share_input",
    rowIDsName = "9. Which of these fits best your current situation?",
    rowIDs = c("You live with:", "You regularly share/pool resources with:", "You sporadically receive income from:"),
    rowLLabels = c("", "", ""),
    choices = c("No one", "Partner", "Children under 16", "Child(ren) 16 or older", "Other relatives (fi. parents)"),
    labelsWidth = list("1px", "15px")
  ),
  selectInput(
    "motherEducation",
    "10. Highest education of your mother:",
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
    "10. Highest education of your father:",
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
    "11. How old are you?",
    value = NA,
    min = 0,
    max = 120,
    width = "100%"
  ),
  radioButtons(
    "gender",
    "12. What is your gender?",
    c("Female", "Male", "Other"),
    selected = character(0),
    width = "100%"
  ),
  selectInput(
    "countryBirth",
    "13. In which country were you born?",
    c("Choose country" = "", contactdata::list_countries()),
    selectize = TRUE,
    width = "100%"
  ),
  selectInput(
    "countryResidence",
    "14. In which country do you currently live?",
    c("Choose country" = "", contactdata::list_countries()),
    selectize = TRUE,
    width = "100%"
  ),
  br(),
  actionButton("submit_button", "Submit")
)

server <- function(input, output) {
  isco08 <- unique(gsub("\\'", "", DIGCLASS::all_labels$isco08[[2]]))

  output$dynamicOccupationQuestion <- renderUI({
    req(input$working)
    if (input$working == "Yes") {
      selectInput(
        "occupation",
        "4. What is your current occupation?",
        choices = c("Write down your occupation" = "", isco08),
        selectize = TRUE,
        width = "100%"
      )
    } else if (input$workedBefore == "Yes") {
      selectInput(
        "occupation",
        "4. What was your last occupation?",
        choices = c("Write down your occupation" = "", isco08),
        selectize = TRUE,
        width = "100%"
      )
    }
  })

  output$dynamicEmploymentTypeQuestion <- renderUI({
    req(input$working)
    if (input$working == "Yes") {
      selectInput(
        "employmentType",
        "5. Do you work on your own or for a company (current job)?",
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
      selectInput("employmentType", "5. Did you work on your own or for a company (last job)?",
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
        "5.1 How many employees do you have (current job)?",
        value = 1,
        min = 1,
        width = "100%"
      )
    } else if (input$employmentType == "I was self-employed and I had employees") {
      numericInput(
        "numEmployees",
        "5.1 How many employees did you have (last job)?",
        value = 1,
        min = 1,
        width = "100%"
      )
    }
  })

  output$dynamicWageQuestion <- renderUI({
    req(input$working)
    if (input$working == "Yes") {
      fluidRow(
        column(
          3,
          selectInput(
            "currency",
            "Currency:",
            unique(rbcb::list_currencies()$symbol),
            selected = "EUR"
          )
        ),
        column(
          9,
          textInput(
            "monthlyWage",
            "6. What is your monthly wage net of taxes (current job)?",
            placeholder = "1,500",
            width = "100%"
          )
        )
      )
    } else if (input$workedBefore == "Yes") {
      fluidRow(
        column(
          3,
          selectInput(
            "currency",
            "Currency:",
            unique(rbcb::list_currencies()$symbol),
            selected = "EUR"
          )
        ),
        column(
          9,
          textInput(
            "monthlyWage",
            "6. What was your monthly wage net of taxes (last job)?",
            placeholder = "1,500",
            width = "100%"
          )
        )
      )
    }
  })

  output$motherOccupationInput <- renderUI({
    if (!is.null(input$motherEducation) && input$motherEducation != "" &&
      input$motherEducation != "Not applicable") {
      selectInput(
        "motherOccupation",
        "10. Mother's occupation when you were 15:",
        choices = c("Write down occupation" = "", isco08),
        selectize = TRUE,
        width = "100%"
      )
    } else {
      div()
    }
  })

  output$fatherOccupationInput <- renderUI({
    if (!is.null(input$fatherEducation) && input$fatherEducation != "" &&
      input$fatherEducation != "Not applicable") {
      selectInput(
        "fatherOccupation",
        "10. Father's occupation when you were 15:",
        choices = c("Write down occupation" = "", isco08),
        selectize = TRUE,
        width = "100%"
      )
    } else {
      div()
    }
  })

  observe({
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

  observeEvent(input$submit_button, {
    # Initialize shinyvalidate
    iv <- InputValidator$new()

    # Add validation rules
    iv$add_rule("education", sv_required("This field is required"))
    iv$add_rule("working", sv_required("This field is required"))

    # Conditional validation for dynamic inputs
    working <- ifelse(is.null(input$working), "", input$working)
    workedBefore <- ifelse(is.null(input$workedBefore), "", input$workedBefore)
    employmentType <- ifelse(is.null(input$employmentType), "", input$employmentType)
    motherEducation <- ifelse(is.null(input$motherEducation), "", input$motherEducation)
    fatherEducation <- ifelse(is.null(input$fatherEducation), "", input$fatherEducation)

    if (working != "Yes" & working != "") {
      iv$add_rule("workedBefore", sv_required("This field is required"))
    }

    if (working == "Yes" | workedBefore == "Yes") {
      iv$add_rule("occupation", sv_required("This field is required"))
      iv$add_rule("employmentType", sv_required("This field is required"))
      iv$add_rule("monthlyWage", sv_required("This field is required"))
      iv$add_rule("careerInterruptions", sv_required("This field is required"))

      if (employmentType == "I am self-employed and I have employees" | employmentType == "I was self-employed and I had employees") {
        iv$add_rule("numEmployees", sv_required("This field is required"))
      }
    }

    if (!motherEducation %in% c("", "Not applicable")) {
      iv$add_rule("motherOccupation", sv_required("This field is required"))
    }

    if (!fatherEducation %in% c("", "Not applicable")) {
      iv$add_rule("fatherOccupation", sv_required("This field is required"))
    }

    if (input$careerInterruptions > 0) {
      iv$add_rule("unemploymentInterruptions", sv_required("This field is required"))
      iv$add_rule("childrenInterruptions", sv_required("This field is required"))
    }

    check_radio <- function(x) {
      is_error <- try(as.data.frame(x), silent = TRUE)
      if (inherits(is_error, "try-error")) {
        return(FALSE)
      } else if (nrow(as.data.frame(x)) != 1) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    }

    iv$add_rule("propertySavings", sv_required("This field is required"))
    iv$add_rule("age", sv_required("This field is required"))
    iv$add_rule("gender", sv_required("This field is required"))
    iv$add_rule("countryBirth", sv_required("This field is required"))
    iv$add_rule("countryResidence", sv_required("This field is required"))
    iv$add_rule("share_input", sv_required("This field is required", test = check_radio))
    iv$add_rule("fatherEducation", sv_required("This field is required"))
    iv$add_rule("motherEducation", sv_required("This field is required"))
    iv$enable()

    if (iv$is_valid()) {
      responsesDF <- reactive({
        res <-
          data.frame(
            Education = ifelse(length(input$education) == 0, NA, input$education),
            Working = ifelse(length(input$working) == 0, NA, input$working),
            WorkedBefore = ifelse(length(input$workedBefore) == 0, NA, input$workedBefore),
            Occupation = ifelse(length(input$occupation) == 0, NA, input$occupation),
            EmploymentType = ifelse(length(input$employmentType) == 0, NA, input$employmentType),
            NumberOfEmployees = ifelse(length(input$numEmployees) == 0, NA, input$numEmployees),
            Currency = ifelse(length(input$currency) == 0, NA, input$currency),
            MonthlyWage = ifelse(length(input$monthlyWage) == 0, NA, input$monthlyWage),
            CareerInterruptions = ifelse(length(input$careerInterruptions) == 0, NA, input$careerInterruptions),
            ChildrenInterruptions = ifelse(length(input$childrenInterruptions) == 0, NA, input$childrenInterruptions),
            UnemploymentInterruptions = ifelse(length(input$unemploymentInterruptions) == 0, NA, input$unemploymentInterruptions),
            PropertySavings = ifelse(length(input$propertySavings) == 0, NA, paste(input$propertySavings, collapse = ", ")),
            as.data.frame(input$share_input), # Here we add the radio matrix
            motherEducation = ifelse(length(input$motherEducation) == 0, NA, input$motherEducation),
            fatherEducation = ifelse(length(input$fatherEducation) == 0, NA, input$fatherEducation),
            motherOccupation = ifelse(length(input$motherOccupation) == 0, NA, input$motherOccupation),
            fatherOccupation = ifelse(length(input$fatherOccupation) == 0, NA, input$fatherOccupation),
            Age = ifelse(length(input$age) == 0, NA, input$age),
            Gender = ifelse(length(input$gender) == 0, NA, input$gender),
            CountryOfBirth = ifelse(length(input$countryBirth) == 0, NA, input$countryBirth),
            CountryOfResidence = ifelse(length(input$countryResidence) == 0, NA, input$countryResidence),
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
