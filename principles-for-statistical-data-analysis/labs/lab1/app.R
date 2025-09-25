# Title: Shiny app for PrinStat Student Survey in Lab 1
# Author: Koen Plevoets
# Date last modified: 26/09/2024

library(shiny)
library(googlesheets4)

# ID of Google Spreadsheet for PrinStat Student Survey:
sheet_id <- paste("https://docs.google.com/spreadsheets/d/",
                  "1KOb_GbNNC2zpteGwWeAcySiIeUu3Rd5wjq873WzO65o",
                  "/", sep ="")

# Functions for Shiny app:
ui <- fluidPage(
  titlePanel("PrinStat Student Survey"),
  textOutput("Intro"),
  selectInput("Gender", label = "Gender", choices = c("", "female", "male")),
  selectInput("Nationality", label = "Nationality",
              choices = c("", "Afghanistan", "Albania", "Algeria", "Andorra",
                          "Angola", "Antigua & Barbuda", "Argentina", "Armenia",
                          "Australia", "Austria", "Azerbaijan", "Bahames, The",
                          "Bahrain", "Bangladesh", "Barbados", "Belarus",
                          "Belgium", "Belize", "Benin", "Bhutan", "Bolivia",
                          "Bosnia & Herzegovina", "Botwana", "Brazil", "Brunei",
                          "Bulgaria", "Burkina Faso", "Burundi", "Cabo Verde",
                          "Cambodia", "Cameroon", "Canada",
                          "Central African Republic", "Chad", "Chile", "China",
                          "Colombia", "Comoros",
                          "Congo, Democratic Republic of the",
                          "Congo, Republic of the Congo", "Costa Rica",
                          "Cote d'Ivoire", "Croatia", "Cuba", "Cyprus",
                          "Czech Republic", "Denmark", "Djibouti", "Domenica",
                          "Dominican Republic", "Ecuador", "Egypt",
                          "El Salvador", "Equatorial Guinea", "Eritrea",
                          "Estonia", "Ethiopia", "Fiji", "Finland",
                          "France", "Gabon", "Gambia, The", "Georgia",
                          "Germany", "Ghana", "Greece", "Grenada", "Guatemala",
                          "Guinea", "Guinea-Bissau", "Guyana", "Haiti",
                          "Honduras", "Hungary", "Iceland", "India",
                          "Indonesia", "Iran", "Iraq", "Ireland", "Israel",
                          "Italy", "Jamaica", "Japan", "Jordan", "Kazakhstan",
                          "Kenya", "Kiribati", "Kosovo", "Kuwait", "Kyrgyzstan",
                          "Laos", "Latvia", "Lebanon", "Lesotho", "Liberia",
                          "Libya", "Liechtenstein", "Lithuania", "Luxembourg",
                          "Macedonia", "Madagascar", "Malawi", "Malaysia",
                          "Maldives", "Mali", "Malta", "Marshall Islands",
                          "Mauretania", "Mauritius", "Mexico",
                          "Micronesia, Federated States of", "Moldova",
                          "Monaco", "Mongolia", "Montenegro", "Morocco",
                          "Mozambique", "Myanmar", "Namibia", "Nauru", "Nepal",
                          "Netherlands, The", "New Zealand", "Nicaragua",
                          "Niger", "Nigeria", "North Korea", "Norway", "Oman",
                          "Pakistan", "Palau", "Palestine", "Panama",
                          "Papua New Guinea", "Paraguay", "Peru", "Philippines",
                          "Poland", "Portugal", "Qatar",
                          "Romania", "Russia", "Rwanda", "Saint Kitts & Nevis",
                          "Saint Lucia", "Saint Vincent & The Grenadines",
                          "Samoa", "San Marino", "Sao Tome & Principe",
                          "Saudi Arabia", "Senegal", "Serbia", "Seychelles",
                          "Sierra Leone", "Singapore", "Slovakia", "Slovenia",
                          "Solomon Islands", "Somalia", "South Africa",
                          "South Korea", "South Sudan", "Spain", "Sri Lanka",
                          "Sudan", "Suriname", "Swaziland", "Sweden",
                          "Switzerland", "Syria", "Tajikistan", "Tanzania",
                          "Thailand", "Timor-Leste", "Togo", "Tonga",
                          "Trinidad & Tobago", "Tunisia", "Turkey",
                          "Turkmenistan", "Tuvalu", "Uganda", "Ukraine",
                          "United Arab Emirates", "United Kingdom",
                          "United States of America", "Uruguay", "Uzbekistan",
                          "Vanuatu", "Vatican City", "Venezuela", "Vietnam",
                          "Yemen", "Zambia", "Zimbabwe")),
  dateInput("Date_of_Birth", label = "Date of Birth", format = "yyyy-mm-dd"),
  selectInput("Academic_background", label = "Academic background",
              choices = c("Arts and literature",
                          "Astronomy / Astrophysics / Space Science",
                          "Biological Sciences", "Business Administration",
                          "Chemistry", "Computer and Information Science",
                          "Design", "Earth Sciences", "Economics",
                          "Education", "Electrical and Electronic Engineering",
                          "Engineering", "Environmental Sciences",
                          "Humanities", "Law", "Linguistics",
                          "Management Science / Operations Research",
                          "Materials Science", "Mathematics", "Medicine",
                          "Philosophy", "Physics", "Psychology",
                          "Social Sciences", "Sports and Recreation",
                          "Statistics / Data Science")),
  radioButtons("R_course",
               label = paste("Did you attend any refresher course on R prior",
                             "to starting this program?"),
               choices = c("yes", "no")),
  selectInput("R_skills",
              label = "How would you describe your skill level in R?",
              choices = c("None", "Low", "Intermediate", "Advanced")),
  numericInput("Distance_to_class",
               label = paste("What's the distance (in km) between your",
                             "location and the university campus (S9, Sterre)?",
                             "You can use Google Maps to verify it."),
               value = 0.0),
  numericInput("Time_to_get_here",
               label = paste("How long (in minutes) did it approximately take",
                             "you to get here from your previous location?"),
               value = 0.0),
  radioButtons("Means_of_transportation",
               label = "What is your usual means of transportation in Ghent?",
               choices = c("public transportation (bus and/or train)",
                           "car / motorbike", "bike")),
  radioButtons("Where",
               label = "Where did you here about the MaStat program?",
               choices = c("Friend/Colleague", "UGent Website",
                           "FLAMES website", "Other")),
  radioButtons("Music_preference",
               label = "What is your favorite music?",
               choices = c("Classical", "House/Techno", "Rap/slow jam",
                           "Zouk/Kizomba", "I don't like music")),
  actionButton("Submit", "Submit"),
  hr()
)

server <- function(input, output, session) {
  output$Intro <- renderText({
    paste("Please fill out the questions below.",
          "Feel free to leave open any question you don't feel comfortable",
          "with answering.")
  })
  entry <- reactive({
    data.frame(Time = format(Sys.time(), format = "%d/%m/%Y %H:%M:%OS"),
               Gender = input$Gender,
               Nationality = input$Nationality,
               `Date of Birth` = format(as.Date(input$Date_of_Birth),
                                        format = "%d/%m/%Y"),
               `Àcademic background` = input$Academic_background,
               `R course` = input$R_course,
               `R skills` = input$R_skills,
               `Distance to class` = input$Distance_to_class,
               `Time to get here` = input$Time_to_get_here,
               `Means of transportation` = input$Means_of_transportation,
               `Where did you hear about the MaStat program?` = input$Where,
               `Music preference` = input$Music_preference)
  })
  observeEvent(input$Submit, {
    googlesheets4::gs4_deauth()
    googlesheets4::gs4_auth(cache = FALSE)
    googlesheets4::sheet_append(sheet_id, entry())
  })
}

shinyApp(ui = ui, server = server)
