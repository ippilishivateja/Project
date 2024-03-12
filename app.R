library(shiny)
library(ggplot2)
library(scales)
library(readxl)
library(ggimage)
library(RColorBrewer)

ui <- navbarPage(
  title = "Social Media Analysis",
  
  tabPanel(
    title = "About",
    fluidPage(
      titlePanel("Social Media Usage Analysis"),
      
      HTML("<p>Social media platforms are online tools that enable users to share and exchange information, ideas, and content within virtual communities. These platforms come in various types, each serving different purposes and catering to diverse user needs.
Social networking sites like Facebook and LinkedIn facilitate connections with friends, family, and professionals, offering opportunities for networking and sharing updates. However, they also raise concerns about privacy and the spread of misinformation.
Microblogging platforms such as Twitter and Reddit allow users to share thoughts and links concisely, fostering real-time discussions and news updates. Yet, their limited character count and the prevalence of trolling pose challenges.
Photo and video sharing apps like Instagram and Snapchat enable users to express themselves creatively through visual content but also contribute to issues like cyberbullying and body image concerns.
Messaging and chat apps like WhatsApp and Messenger provide instant communication with individuals and groups, emphasizing privacy and security. However, they can be prone to misuse and distraction from face-to-face interactions.
Blogging platforms such as WordPress and Medium offer spaces for long-form content creation and community engagement but require effort to build an audience and combat plagiarism.
Overall, while social media enhances communication, connectivity, and entertainment, users must navigate privacy concerns, misinformation, and the potential negative impacts on mental health. Striking a balance and using these platforms responsibly is crucial in leveraging their benefits while minimizing risks.</p>"),
      tags$img(src = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcRfS43sWeyAHiB3P1wDNDH95agVhydlve5vOA&usqp=CAU', 
               style = 'display:block; margin:auto;', 
               height ='300px', 
               width = '300px'),
      
      HTML("<p3 style='font-size: 20px;'>Below is the bad graph related to the Social Media: </p3>"),
      tags$img(src = 'https://static01.nyt.com/images/2023/11/29/learning/TeenSocialMediaGraph/TeenSocialMediaGraph-videoSixteenByNineJumbo1600.png', 
               style = 'display:block; margin:auto;', 
               height ='300px', 
               width = '600px'),
      HTML("<P>The title is not descriptive enough, making it difficult to understand the purpose of the graph.</P>"),
      HTML("<p>The graph is bad because it is difficult to read and understand. The colors are too similar, the text is too small. It would be better to use a different type of graph, such as a bar chart.</p>"),
      HTML("<P>The y-axis categories are not aligned, making it difficult to compare the data.</P>"),
      HTML("<P>The legend is not clear, making it difficult to understand what each color represents.</P>"),
      HTML("<P>The data is not labeled, making it difficult to understand what the numbers represent.</P>"),
      HTML("<p>Graph Citation: https://www.nytimes.com/2023/06/17/upshot/social-media-teen-mental-health.html</P>"),      
      HTML("<P>Dataset Citation:  </P>")
      )
  ),
  
  tabPanel(
    title = "Analysis",
    fluidPage(
      titlePanel("Social Media Usage Analysis"),
      tags$img(src = 'https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcQ4t4igL_o7dR1SMmgcCXK8y21EP33jQvQmRg&usqp=CAU', 
               style = 'display:block; margin:auto;', 
               height ='200px', 
               width = '800px'),
      sidebarLayout(
        sidebarPanel(
          selectInput("option", "Select an option:",
                      choices = c("Usage by gender",
                                  "Usage by Region",
                                  "Usage by Age",
                                  "Usage by Country",
                                  "Most Used App")),
          actionButton("go_button", "GO"),
          
          HTML("<p><strong>Watch the video <a href='https://youtu.be/Px1sEmlSApA'>here</a>.</strong></p>"),
        ),
        
        mainPanel(
          plotOutput("plot_male"),
          plotOutput("plot_female"),
          plotOutput("plot")
        )
      )
    )
  ),
  
  tabPanel(
    title = "Backend Code",
    verbatimTextOutput("backend_code")
  )
)

server <- function(input, output, session) {
  
  Gender <- reactive({
    read_excel("Social_Media_Dataset.xlsx")
  })
  Region <- reactive({
    read_excel("Region_Dataset.xlsx")
  })
  Age <- reactive({
    read_excel("AgeGroup_Dataset.xlsx")
  })
  Country <- reactive({
    read_excel("Country_Dataset.xlsx")
  })
  Usage <- reactive({
    read_excel("Users_Dataset.xlsx")
  })
  
  observeEvent(input$go_button, {
    
    if (input$option == "Usage by gender") {
      output$plot_male <- renderPlot({
        ggplot(Gender(), aes(x = Platform, y = `Male User (%)`, fill = "Male")) +
          geom_bar(stat = "identity") +
          labs(title = "Social Media Usage by Male Gender",
               x = "Platform",
               y = "Male User Percentage",
               fill = "Gender") +
          scale_fill_manual(values = c("Male" = "skyblue")) +
          theme(legend.position = "none",
        axis.text = element_text(size = 14)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
      
      output$plot_female <- renderPlot({
        ggplot(Gender(), aes(x = Platform, y = `Female User (%)`, fill = "Female")) +
          geom_bar(stat = "identity") +
          labs(title = "Social Media Usage by Female Gender",
               x = "Platform",
               y = "Female User Percentage",
               fill = "Gender") +
          scale_fill_manual(values = c("Female" = "salmon")) +
          theme(legend.position = "none",
                axis.text = element_text(size = 14)) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))
      })
    }
    
    if (input$option == "Usage by Region") {
      output$plot_male <- renderPlot({
        ggplot(Region(), aes(x = Region, y = Male)) +
          geom_bar(stat = "identity", fill = "lightgreen") +
          labs(title = "Male Social Media Usage by Region",
               x = "Region",
               y = "Percentage") +
          theme(axis.text.x = element_text(size = 12),  # Increase x-axis label size
                axis.text.y = element_text(size = 12))  # Increase y-axis label size
      })
      
      output$plot_female <- renderPlot({
        ggplot(Region(), aes(x = Region, y = Female)) +
          geom_bar(stat = "identity", fill = "yellow") +
          labs(title = "Female Social Media Usage by Region",
               x = "Region",
               y = "Percentage") +
          theme(axis.text.x = element_text(size = 12),  # Increase x-axis label size
                axis.text.y = element_text(size = 12))  # Increase y-axis label size
      })
    }
    
    if (input$option == "Usage by Age") {
  output$plot_male <- renderPlot({
    ggplot(Age(), aes(x = AgeGroup, y = `Percentage (%)`*100, fill = AgeGroup)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste0(`Percentage (%)`*100, "%"), 
                    y = `Percentage (%)`), 
                vjust = -0.5, 
                size = 4, 
                color = "black") +
      labs(title = "Social Media Usage by Age Group",
           x = "Age Group",
           y = "Percentage",
           fill = "Age Group") +
      theme_minimal() +
      scale_x_discrete(labels = function(x) paste0(x, "%"), expand = expansion(mult = c(0.1, 0))) +
      scale_y_continuous(labels = function(x) paste0(round(x), "%"), limits = c(0, 100), expand = expansion(mult = c(0, 0.1))) +
      theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 12), 
            axis.text.x = element_text(size = 12), 
            plot.title = element_text(hjust = 0.5, size = 14),  
            legend.position = "none")
  })
  output$plot_female <- NULL
}
    
    if (input$option == "Usage by Country") {
      output$plot_male <- renderPlot({
        ggplot(Country(), aes(x = `Percentage (%)`*100, y = reorder(Country, -`Percentage (%)`), fill = Country)) +
          geom_bar(stat = "identity", alpha = 0.8) +  
          scale_fill_brewer(palette = "Paired") +  
          labs(title = "Social Media Usage by Country",
               x = "Percentage",
               y = "Country") +
          theme_minimal() +
          theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 12), 
                axis.text.x = element_text(size = 12), 
                plot.title = element_text(hjust = 0.5, size = 14),  
                legend.position = "none")  
      })
      output$plot_female <- NULL
    }
    
    if (input$option == "Most Used App") {
      output$plot_male <- renderPlot({
        
        my_palette <- c("#FF2400", "#1877F2", "#0088FF", "#833AB4", "#FF4B55",
                        "#BD081C", "#2CA4E0", "#E6162D", "#FFFC00", "#17becf",
                        "#000000", "#25D366", "#09B83E", "#1DA1F2", "#FF0000")
        
        ggplot(Usage(), aes(x = reorder(Social_Media_Network, -Users), y = Users, fill = Social_Media_Network)) +
          geom_bar(stat = "identity") +
          scale_fill_manual(values = my_palette) +  
          labs(title = "Most Used Social Media Apps",
               x = "Social Media Network",
               y = "Users (in millions)")+
          theme_minimal()+
        theme(axis.text.y = element_text(angle = 0, hjust = 1, size = 12), 
              axis.text.x = element_text(size = 12,angle = 45, hjust = 1), 
              plot.title = element_text(hjust = 0.5, size = 14), 
              legend.position = "none")
      })
      output$plot_female <- NULL
    }
  })
  output$backend_code <- renderPrint({
    cat("library(shiny)\n")
    cat("library(ggplot2)\n")
    cat("library(readxl)\n")
    cat("library(ggimage)\n")
    cat("library(RColorBrewer)\n\n")
    
    cat("ui <- navbarPage(\n")
    cat("  title = 'Social Media Analysis',\n")
    cat("\n")
    cat("  tabPanel(\n")
    cat("    title = 'About',\n")
    cat("    fluidPage(\n")
    cat("      titlePanel('Social Media Usage Analysis'),\n")
    cat("      HTML(\"<p>Social media encompasses online platforms and websites designed for users to create, share, and interact with various forms of content, fostering communication, networking, and community building. Users utilize social media for diverse purposes, including communication with friends, family, and colleagues; sharing personal updates, photos, videos, and articles; networking for personal and professional connections; consuming news, information, and entertainment content; marketing and promoting businesses, products, and services; engaging in online communities based on shared interests or beliefs; and accessing customer service from businesses. Social media platforms serve as hubs for communication, information dissemination, and digital engagement, playing a pivotal role in modern society's communication landscape and facilitating global connectivity on an unprecedented scale.</p>\")\n")
    cat("    )\n")
    cat("  ),\n")
    cat("\n")
    cat("  tabPanel(\n")
    cat("    title = 'Analysis',\n")
    cat("    fluidPage(\n")
    cat("      titlePanel('Social Media Usage Analysis'),\n")
    cat("      sidebarLayout(\n")
    cat("        sidebarPanel(\n")
    cat("          selectInput('option', 'Select an option:',\n")
    cat("                      choices = c('Usage by gender',\n")
    cat("                                  'Usage by Region',\n")
    cat("                                  'Usage by Age',\n")
    cat("                                  'Usage by Country',\n")
    cat("                                  'Most Used App')),\n")
    cat("          actionButton('go_button', 'GO')\n")
    cat("        ),\n")
    cat("        mainPanel(\n")
    cat("          plotOutput('plot_male'),\n")
    cat("          plotOutput('plot_female'),\n")
    cat("          plotOutput('plot')\n")
    cat("        )\n")
    cat("      )\n")
    cat("    )\n")
    cat("  ),\n")
    cat("\n")
    cat("  tabPanel(\n")
    cat("    title = 'Backend Code',\n")
    cat("    verbatimTextOutput('backend_code')\n")
    cat("  )\n")
    cat(")\n")
    
    cat("server <- function(input, output, session) {\n\n")
    cat("  Gender <- reactive({\n")
    cat("    read_excel('C:\\Users\\User\\OneDrive\\Desktop\\STAT 515\\Mid_Project\\Social Media Dataset.xlsx')\n")
    cat("  })\n\n")
    cat("  Region <- reactive({\n")
    cat("    read_excel('C:\\Users\\User\\OneDrive\\Desktop\\STAT 515\\Mid_Project\\Region Dataset.xlsx')\n")
    cat("  })\n\n")
    cat("  Age <- reactive({\n")
    cat("    read_excel('C:\\Users\\User\\OneDrive\\Desktop\\STAT 515\\Mid_Project\\AgeGroup Dataset.xlsx')\n")
    cat("  })\n\n")
    cat("  Country <- reactive({\n")
    cat("    read_excel('C:\\Users\\User\\OneDrive\\Desktop\\STAT 515\\Mid_Project\\Country Dataset.xlsx')\n")
    cat("  })\n\n")
    cat("  Usage <- reactive({\n")
    cat("    read_excel('C:\\Users\\User\\OneDrive\\Desktop\\STAT 515\\Mid_Project\\Users Dataset.xlsx')\n")
    cat("  })\n\n")
    
    cat("  observeEvent(input$go_button, {\n\n")
    cat("    if (input$option == 'Usage by gender') {\n")
    cat("      output$plot_male <- renderPlot({\n")
    cat("        ggplot(Gender(), aes(x = Platform, y = `Male User (%)`, fill = 'Male')) +\n")
    cat("          geom_bar(stat = 'identity') +\n")
    cat("          labs(title = 'Social Media Usage by Male Gender',\n")
    cat("               x = 'Platform',\n")
    cat("               y = 'Male User Percentage',\n")
    cat("               fill = 'Gender') +\n")
    cat("          scale_fill_manual(values = c('Male' = 'skyblue')) +\n")
    cat("          theme(legend.position = 'none')\n")
    cat("      })\n\n")
    
    cat("      output$plot_female <- renderPlot({\n")
    cat("        ggplot(Gender(), aes(x = Platform, y = `Female User (%)`, fill = 'Female')) +\n")
    cat("          geom_bar(stat = 'identity') +\n")
    cat("          labs(title = 'Social Media Usage by Female Gender',\n")
    cat("               x = 'Platform',\n")
    cat("               y = 'Female User Percentage',\n")
    cat("               fill = 'Gender') +\n")
    cat("          scale_fill_manual(values = c('Female' = 'salmon')) +\n")
    cat("          theme(legend.position = 'none')\n")
    cat("      })\n")
    cat("    }\n\n")
    
    cat("    if (input$option == 'Usage by Region') {\n")
    cat("      output$plot_male <- renderPlot({\n")
    cat("        ggplot(Region(), aes(x = Region, y = Male)) +\n")
    cat("          geom_bar(stat = 'identity', fill = 'lightgreen') +\n")
    cat("          labs(title = 'Male Social Media Usage by Region',\n")
    cat("               x = 'Region',\n")
    cat("               y = 'Percentage')\n")
    cat("      })\n\n")
    
    cat("      output$plot_female <- renderPlot({\n")
    cat("        ggplot(Region(), aes(x = Region, y = Female)) +\n")
    cat("          geom_bar(stat = 'identity', fill = 'yellow') +\n")
    cat("          labs(title = 'Female Social Media Usage by Region',\n")
    cat("               x = 'Region',\n")
    cat("               y = 'Percentage')\n")
    cat("      })\n")
    cat("    }\n\n")
    cat("    if (input$option == 'Usage by Age') {\n")
    cat("      output$plot_male <- renderPlot({\n")
    cat("        ggplot(Age(), aes(x = 'Age', y = `Percentage (%)`, fill = AgeGroup)) +\n")
    cat("          geom_bar(stat = 'identity') +\n")
    cat("          coord_polar('y', start = 0) +\n")
    cat("          geom_text(aes(label = paste0(`Percentage (%)`, '%'), x = 'Age', y = `Percentage (%)`), vjust = 5, hjust = 5, size = 6) +\n")
    cat("          labs(title = 'Social Media Usage by Age Group',\n")
    cat("               fill = 'Age Group',\n")
    cat("               y = NULL) +\n")
    cat("          theme_void() +\n")
    cat("          theme(legend.position = 'right')\n")
    cat("      })\n")
    cat("      output$plot_female <- NULL\n")
    cat("    }\n\n")
    
    cat("    if (input$option == 'Usage by Country') {\n")
    cat("      output$plot_male <- renderPlot({\n")
    cat("        ggplot(Country(), aes(x = 'Percentage (%)', y = reorder(Country, -`Percentage (%)`), fill = Country)) +\n")
    cat("          geom_bar(stat = 'identity', alpha = 0.8) +\n")
    cat("          scale_fill_brewer(palette = 'Paired') +\n")
    cat("          labs(title = 'Social Media Usage by Country',\n")
    cat("               x = 'Percentage',\n")
    cat("               y = 'Country') +\n")
    cat("          theme_minimal() +\n")
    cat("          theme(axis.text.y = element_text(angle = 0, hjust = 1),\n")
    cat("                plot.title = element_text(hjust = 0.5),\n")
    cat("                legend.position = 'none')\n")
    cat("      })\n")
    cat("      output$plot_female <- NULL\n")
    cat("    }\n\n")
    
    cat("    if (input$option == 'Most Used App') {\n")
    cat("      output$plot_male <- renderPlot({\n")
    cat("        my_palette <- c('#FF2400', '#1877F2', '#0088FF', '#833AB4', '#FF4B55',\n")
    cat("                        '#BD081C', '#2CA4E0', '#E6162D', '#FFFC00', '#17becf',\n")
    cat("                        '#000000', '#25D366', '#09B83E', '#1DA1F2', '#FF0000')\n\n")
    cat("        ggplot(Usage(), aes(x = reorder(Social_Media_Network, -Users), y = Users, fill = Social_Media_Network)) +\n")
    cat("          geom_bar(stat = 'identity') +\n")
    cat("          scale_fill_manual(values = my_palette) +\n")
    cat("          labs(title = 'Most Used Social Media Apps',\n")
    cat("               x = 'Social Media Network',\n")
    cat("               y = 'Users (in millions)')\n")
    cat("      })\n")
    cat("      output$plot_female <- NULL\n")
    cat("    }\n")
    cat("  })\n")
    cat("}\n")
    cat("shinyApp(ui = ui, server = server)")
  })
}
shinyApp(ui = ui, server = server)
