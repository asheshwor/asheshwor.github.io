# R script to generate academic profile HTML from Markdown content

# Install and load necessary packages
if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}
library(stringr)

# --- Markdown Content ---
# This string contains the academic profile content in Markdown format.
# You can modify this content directly within this R script.
markdown_content <- paste0(c(
  "# Profile",
  "## Your Full Name",
  "[Your Academic Title]",
  "[Your Affiliation/Department]",
  "[Your Tagline or Brief Bio: e.g., Researcher focusing on diverse interdisciplinary topics / Expert in X & Y]",
  "",
  "## Social Media",
  "- Twitter: https://twitter.com/[YourTwitterHandle]",
  "- GitHub: https://github.com/[YourGitHubHandle]",
  "- LinkedIn: https://linkedin.com/in/[YourLinkedInHandle]",
  "- Threads: https://www.threads.net/@yourthreadsusername",
  "- Instagram: https://www.instagram.com/yourinstagramusername/",
  "",
  "## Contact Information",
  "- Email: your.email@example.com",
  "- Location: [Your City, Your Country]",
  "",
  "# Main Content",
  "",
  "## Research Areas",
  "- [Research Area 1: e.g., Artificial Intelligence and Machine Learning]",
  "- [Research Area 2: e.g., Sustainable Urban Development]",
  "- [Research Area 3: e.g., Cognitive Neuroscience]",
  "- [Research Area 4: e.g., Environmental Policy and Governance]",
  "- [Research Area 5: e.g., Human-Computer Interaction]",
  "",
  "## Current Projects",
  "- ### [Project Title 1]",
  "  [Brief description of Project 1]",
  "  [Start Date] - Present",
  "  - [Key activity/goal 1]",
  "  - [Key activity/goal 2]",
  "- ### [Project Title 2]",
  "  [Brief description of Project 2]",
  "  [Start Date] - [End Date (if applicable)]",
  "",
  "## Employment",
  "- ### [Current Position Title]",
  "  [Current Institution/Organization]",
  "  [Start Date] - Present",
  "  - [Key Responsibility/Achievement 1]",
  "  - [Key Responsibility/Achievement 2]",
  "- ### [Previous Position Title]",
  "  [Previous Institution/Organization]",
  "  [Start Date] - [End Date]",
  "  - [Key Responsibility/Achievement 1]",
  "  - [Key Responsibility/Achievement 2]",
  "",
  "## Additional Experience",
  "- ### [Additional Role/Activity 1]",
  "  [Organization/Context]",
  "  [Dates]",
  "- ### [Additional Role/Activity 2]",
  "  [Organization/Context]",
  "  [Dates]",
  "- ### [Additional Role/Activity 3]",
  "  [Organization/Context]",
  "  [Dates]",
  "",
  "## Media Features",
  "- ### [Media Outlet Name 1]",
  "  [Article/Feature Title 1]",
  "  [Date of Feature]",
  "  Link: [Link to media feature 1]",
  "- ### [Media Outlet Name 2]",
  "  [Article/Feature Title 2]",
  "  [Date of Feature]",
  "  Link: [Link to media feature 2]",
  "",
  "## Publications and Conference Presentations",
  "- [Author(s) Name(s)]. ([Year]). [Publication Title]. *[Journal/Conference Name]* ([Month Year]). DOI: [Link to DOI or paper]",
  "- [Author(s) Name(s)]. ([Year]). [Another Publication Title]. *[Journal/Conference Name]*. DOI: [Link to DOI or paper]",
  "- [Author(s) Name(s)]. ([Year]). [Conference Presentation Title]. Paper presented at the \\\"[Conference Name]\\\", [Date]. [Location].",
  "  - Slides: [Link to Slides]",
  "  - Conference Link: [Link to Conference]",
  "",
  "## Non-academic Publications",
  "- ### [Article Title 1]",
  "  [Platform/Outlet 1]",
  "  [Date]",
  "  Link: [Link to non-academic publication 1]",
  "- ### [Article Title 2]",
  "  [Platform/Outlet 2]",
  "  [Date]",
  "  Link: [Link to non-academic publication 2]",
  "",
  "## Hobbies and Interests",
  "- [Hobby/Interest 1: e.g., Hiking and Photography]",
  "- [Hobby/Interest 2: e.g., Reading Science Fiction]",
  "- [Hobby/Interest 3: e.g., Playing Chess]"
), collapse = "\n")

# --- Markdown Parsing Function ---
split_markdown_into_sections <- function(md_content) {
  sections <- list()
  # Split by top-level headers (# )
  main_sections <- str_split(md_content, "# ", simplify = TRUE)[-1] # Remove empty first element if present
  
  for (section_text in main_sections) {
    lines <- str_split(section_text, "\n")[[1]]
    title <- str_trim(lines[1])
    content <- paste(str_trim(lines[-1]), collapse = "\n")
    sections[[title]] <- content
  }
  return(sections)
}

# --- HTML Rendering Functions ---

render_profile_section <- function(profile_data) {
  profile_lines <- str_split(profile_data, "\n")[[1]]
  profile_lines <- profile_lines[str_trim(profile_lines) != ""]
  
  name <- str_replace(profile_lines[1], "## ", "")
  title <- profile_lines[2]
  affiliation <- profile_lines[3]
  tagline <- profile_lines[4]
  
  sprintf('
    <h1 id="profile-name" class="text-3xl sm:text-4xl font-bold text-gray-800 mb-2 text-center lg:text-left">%s</h1>
    <h2 id="profile-title" class="text-lg sm:text-xl font-medium text-indigo-700 mb-1 text-center lg:text-left">%s</h2>
    <p id="profile-affiliation" class="text-sm text-gray-600 mb-4 text-center lg:text-left">%s</p>
    <p id="profile-tagline" class="text-base text-gray-700 leading-relaxed mb-6 text-center lg:text-left">%s</p>
  ', name, title, affiliation, tagline)
}

render_social_media_links <- function(social_media_data) {
  links_html <- ""
  social_media_lines <- str_split(social_media_data, "\n")[[1]]
  social_media_lines <- social_media_lines[str_trim(social_media_lines) != ""]
  
  for (line in social_media_lines) {
    match <- str_match(line, "^- (Twitter|GitHub|LinkedIn|Threads|Instagram): (.*)$")
    if (!is.na(match[1,1])) {
      platform <- match[1,2]
      url <- match[1,3]
      icon_class <- ""
      text_color <- ""
      bg_color <- ""
      
      if (platform == 'Twitter') { icon_class <- 'fab fa-twitter'; text_color <- 'text-blue-500'; bg_color <- 'bg-blue-50' }
      else if (platform == 'GitHub') { icon_class <- 'fab fa-github'; text_color <- 'text-gray-700'; bg_color <- 'bg-gray-100' }
      else if (platform == 'LinkedIn') { icon_class <- 'fab fa-linkedin'; text_color <- 'text-blue-700'; bg_color <- 'bg-blue-100' }
      else if (platform == 'Threads') { icon_class <- 'fab fa-threads'; text_color <- 'text-gray-800'; bg_color <- 'bg-gray-100' }
      else if (platform == 'Instagram') { icon_class <- 'fab fa-instagram'; text_color <- 'text-pink-600'; bg_color <- 'bg-pink-50' }
      
      links_html <- paste0(links_html, sprintf('
        <a href="%s" target="_blank" class="flex items-center space-x-2 %s hover:%s transition-colors duration-200 p-2 rounded-md %s hover:%s">
            <i class="%s text-xl"></i>
            <span class="font-medium">%s</span>
        </a>
      ', url, text_color, str_replace_all(text_color, c('-500'='-700', '-600'='-800', '-700'='-900', '-800'='-black')), bg_color, str_replace_all(bg_color, c('-50'='-100', '-100'='-200')), icon_class, platform))
    }
  }
  return(links_html)
}

render_contact_info <- function(contact_data) {
  contact_lines <- str_split(contact_data, "\n")[[1]]
  contact_lines <- contact_lines[str_trim(contact_lines) != ""]
  
  email_match <- str_match(contact_lines[1], "Email: (.*)$")
  location_match <- str_match(contact_lines[2], "Location: (.*)$")
  
  email_html <- ""
  if (!is.na(email_match[1,1])) {
    email <- email_match[1,2]
    email_html <- sprintf('
      <p id="contact-email" class="flex items-center justify-center lg:justify-start text-gray-700 mb-2">
          <i class="fas fa-envelope mr-2 text-indigo-500"></i>
          <a href="mailto:%s" class="hover:underline">%s</a>
      </p>
    ', email, email)
  }
  
  location_html <- ""
  if (!is.na(location_match[1,1])) {
    location <- location_match[1,2]
    location_html <- sprintf('
      <p id="contact-location" class="flex items-center justify-center lg:justify-start text-gray-700">
          <i class="fas fa-map-marker-alt mr-2 text-indigo-500"></i>
          %s
      </p>
    ', location)
  }
  return(paste0(email_html, location_html))
}

render_list_section <- function(content) {
  ul_html <- '<ul class="list-disc pl-5 text-gray-700 leading-relaxed space-y-2">'
  lines <- str_split(content, "\n")[[1]]
  lines <- lines[str_trim(lines) != ""]
  for (line in lines) {
    if (str_starts(line, "- ")) {
      ul_html <- paste0(ul_html, sprintf('<li>%s</li>', str_sub(line, 3)))
    }
  }
  ul_html <- paste0(ul_html, '</ul>')
  return(ul_html)
}

render_complex_section <- function(content, parse_links = FALSE) {
  html_output <- ""
  items <- str_split(content, "### ", simplify = TRUE)[-1] # Split by ### and remove empty first element
  
  for (item in items) {
    lines <- str_split(item, "\n")[[1]]
    lines <- lines[str_trim(lines) != ""]
    title <- lines[1]
    
    div_html <- sprintf('<div><h3 class="text-lg font-medium text-gray-800">%s</h3>', title)
    current_list_html <- ""
    in_list <- FALSE
    
    for (i in 2:length(lines)) {
      line <- lines[i]
      if (str_starts(line, "- ")) {
        if (!in_list) {
          current_list_html <- paste0(current_list_html, '<ul class="list-disc pl-5 text-gray-700 text-sm mt-2">')
          in_list <- TRUE
        }
        current_list_html <- paste0(current_list_html, sprintf('<li>%s</li>', str_sub(line, 3)))
      } else if (str_starts(line, "Link: ") && parse_links) {
        if (in_list) {
          current_list_html <- paste0(current_list_html, '</ul>')
          div_html <- paste0(div_html, current_list_html)
          current_list_html <- ""
          in_list <- FALSE
        }
        link_text <- str_sub(line, 7)
        link_match <- str_match(link_text, "^\\[(.*?)\\]\\((.*?)\\)$")
        if (!is.na(link_match[1,1])) {
          link_element <- sprintf('<a href="%s" target="_blank" class="text-indigo-600 hover:underline text-sm">%s</a>', link_match[1,3], 'Read more')
          div_html <- paste0(div_html, link_element)
        } else {
          div_html <- paste0(div_html, sprintf('<p class="text-gray-600">%s</p>', str_trim(line)))
        }
      } else {
        if (in_list) {
          current_list_html <- paste0(current_list_html, '</ul>')
          div_html <- paste0(div_html, current_list_html)
          current_list_html <- ""
          in_list <- FALSE
        }
        p_class <- if (str_detect(line, "Date|Present")) 'text-sm text-gray-500' else 'text-gray-600'
        div_html <- paste0(div_html, sprintf('<p class="%s">%s</p>', p_class, str_trim(line)))
      }
    }
    if (in_list) { # Close list if still open at end of item
      current_list_html <- paste0(current_list_html, '</ul>')
      div_html <- paste0(div_html, current_list_html)
    }
    div_html <- paste0(div_html, '</div>')
    html_output <- paste0(html_output, div_html)
  }
  return(html_output)
}

render_publications_section <- function(content) {
  html_output <- ""
  items <- str_split(content, "- ", simplify = TRUE)[-1] # Split by - and remove empty first element
  
  for (item in items) {
    div_html <- '<div>'
    p_html <- '<p class="text-gray-700 leading-relaxed">'
    
    # Handle italic for journal/conference name
    formatted_item <- str_replace_all(item, "\\*\\[(.*?)\\]\\*", '<em class="font-medium">\\1</em>')
    
    # Handle DOI links
    formatted_item <- str_replace_all(formatted_item, "DOI: \\[(.*?)\\]\\((.*?)\\)", 'DOI: <a href="\\2" target="_blank" class="text-indigo-600 hover:underline ml-1">\\1</a>')
    
    # Handle Slides/Conference Links for presentations
    slides_match <- str_match(formatted_item, "Slides: \\[(.*?)\\]")
    conference_match <- str_match(formatted_item, "Conference Link: \\[(.*?)\\]")
    
    if (!is.na(slides_match[1,1]) || !is.na(conference_match[1,1])) {
      presentation_text <- formatted_item
      if (!is.na(slides_match[1,1])) {
        presentation_text <- str_split(presentation_text, "Slides:", simplify = TRUE)[1]
      }
      if (!is.na(conference_match[1,1])) {
        presentation_text <- str_split(presentation_text, "Conference Link:", simplify = TRUE)[1]
      }
      p_html <- paste0(p_html, str_trim(presentation_text), '</p>')
      div_html <- paste0(div_html, p_html)
      
      button_container_html <- '<div class="flex gap-2 mt-2">'
      if (!is.na(slides_match[1,1])) {
        slide_link <- slides_match[1,2]
        button_container_html <- paste0(button_container_html, sprintf('
          <a href="%s" target="_blank" class="inline-block bg-indigo-100 text-indigo-700 px-3 py-1 rounded-full text-sm font-medium hover:bg-indigo-200 transition-colors duration-200">Slides</a>
        ', slide_link))
      }
      if (!is.na(conference_match[1,1])) {
        conf_link <- conference_match[1,2]
        button_container_html <- paste0(button_container_html, sprintf('
          <a href="%s" target="_blank" class="inline-block bg-indigo-100 text-indigo-700 px-3 py-1 rounded-full text-sm font-medium hover:bg-indigo-200 transition-colors duration-200">Conference Link</a>
        ', conf_link))
      }
      button_container_html <- paste0(button_container_html, '</div>')
      div_html <- paste0(div_html, button_container_html)
      
    } else {
      p_html <- paste0(p_html, formatted_item, '</p>')
      div_html <- paste0(div_html, p_html)
    }
    div_html <- paste0(div_html, '</div>')
    html_output <- paste0(html_output, div_html)
  }
  return(html_output)
}

# --- Main HTML Generation Logic ---

sections <- split_markdown_into_sections(markdown_content)

profile_html <- render_profile_section(sections[['Profile']])
social_media_html <- render_social_media_links(sections[['Social Media']])
contact_info_html <- render_contact_info(sections[['Contact Information']])

research_areas_content_html <- render_list_section(sections[['Research Areas']])
current_projects_content_html <- render_complex_section(sections[['Current Projects']])
employment_content_html <- render_complex_section(sections[['Employment']])
additional_experience_content_html <- render_complex_section(sections[['Additional Experience']])
media_features_content_html <- render_complex_section(sections[['Media Features']], parse_links = TRUE)
publications_content_html <- render_publications_section(sections[['Publications and Conference Presentations']])
non_academic_publications_content_html <- render_complex_section(sections[['Non-academic Publications']], parse_links = TRUE)
hobbies_content_html <- render_list_section(sections[['Hobbies and Interests']])


# Assemble the full HTML content
full_html_content <- paste0(c(
  '<!DOCTYPE html>',
  '<html lang="en">',
  '<head>',
  '    <meta charset="UTF-8">',
  '    <meta name="viewport" content="width=device-width, initial-scale=1.0">',
  '    <title>Academic Profile - [Your Name]</title>',
  '    <script src="https://cdn.tailwindcss.com"></script>',
  '    <link href="https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap" rel="stylesheet">',
  '    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0-beta3/css/all.min.css">',
  '    <style>',
  '        /* Base styles for light mode */',
  '        body {',
  '            font-family: \'Inter\', sans-serif;',
  '            background-color: #f8fafc; /* Light gray background */',
  '            color: #334155; /* Dark slate gray text */',
  '            cursor: crosshair; /* Change cursor to crosshairs on page hover */',
  '            transition: background-color 0.3s ease, color 0.3s ease; /* Smooth transition for theme changes */',
  '        }',
  '        .bg-white { background-color: #ffffff; transition: background-color 0.3s ease; }',
  '        .text-gray-800 { color: #1f2937; transition: color 0.3s ease; }',
  '        .text-gray-700 { color: #374151; transition: color 0.3s ease; }',
  '        .text-gray-600 { color: #4b5563; transition: color 0.3s ease; }',
  '        .text-gray-500 { color: #6b7280; transition: color 0.3s ease; }',
  '        .border-gray-200 { border-color: #e5e7eb; transition: border-color 0.3s ease; }',
  '        .bg-gray-50 { background-color: #f9fafb; transition: background-color 0.3s ease; }',
  '        .hover\\:bg-gray-100:hover { background-color: #f3f4f6; transition: background-color 0.3s ease; }',
  '        .shadow-lg { box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.1), 0 4px 6px -2px rgba(0, 0, 0, 0.05); transition: box-shadow 0.3s ease; }',
  '',
  '        /* Dark mode styles */',
  '        body.dark {',
  '            background-color: #1a202c; /* Dark background */',
  '            color: #e2e8f0; /* Light text */',
  '        }',
  '        body.dark .bg-white { background-color: #2d3748; }',
  '        body.dark .text-gray-800 { color: #f7fafc; }',
  '        body.dark .text-gray-700 { color: #e2e8f0; }',
  '        body.dark .text-gray-600 { color: #cbd5e0; }',
  '        body.dark .text-gray-500 { color: #a0aec0; }',
  '        body.dark .border-gray-200 { border-color: #4a5568; }',
  '        body.dark .bg-gray-50 { background-color: #2c313d; }',
  '        body.dark .hover\\:bg-gray-100:hover { background-color: #3a4452; }',
  '        body.dark .shadow-lg { box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.3), 0 4px 6px -2px rgba(0, 0, 0, 0.15); }',
  '',
  '        /* Custom scrollbar for a cleaner look */',
  '        ::-webkit-scrollbar {',
  '            width: 8px;',
  '        }',
  '        ::-webkit-scrollbar-track {',
  '            background: #f1f5f9;',
  '        }',
  '        body.dark ::-webkit-scrollbar-track {',
  '            background: #2d3748;',
  '        }',
  '        ::-webkit-scrollbar-thumb {',
  '            background: #cbd5e1;',
  '            border-radius: 4px;',
  '        }',
  '        body.dark ::-webkit-scrollbar-thumb {',
  '            background: #4a5568;',
  '        }',
  '        ::-webkit-scrollbar-thumb:hover {',
  '            background: #94a3b8;',
  '        }',
  '        body.dark ::-webkit-scrollbar-thumb:hover {',
  '            background: #6b7280;',
  '        }',
  '',
  '        /* Hide elements during print for a cleaner PDF */',
  '        @media print {',
  '            .no-print {',
  '                display: none !important;',
  '            }',
  '            body {',
  '                cursor: default; /* Revert cursor for printing */',
  '            }',
  '        }',
  '    </style>',
  '</head>',
  '<body class="antialiased">',
  '    <div class="container mx-auto p-4 sm:p-8 lg:p-12 max-w-7xl">',
  '        <div class="flex flex-col lg:flex-row gap-8 lg:gap-12">',
  '            <aside class="lg:w-1/4 p-6 bg-white rounded-xl shadow-lg flex flex-col items-center lg:items-start h-fit lg:sticky lg:top-8">',
  profile_html,
  '                <div id="social-media-links" class="flex flex-wrap justify-center lg:justify-start gap-4 mb-8">',
  social_media_html,
  '                </div>',
  '',
  '                <button onclick="window.print()" class="w-full flex items-center justify-center space-x-2 bg-indigo-600 text-white py-2 px-4 rounded-md hover:bg-indigo-700 transition-colors duration-200 mb-4 shadow-md no-print">',
  '                    <i class="fas fa-file-pdf text-xl"></i>',
  '                    <span class="font-medium">Save as PDF</span>',
  '                </button>',
  '',
  '                <button id="theme-toggle" class="w-full flex items-center justify-center space-x-2 bg-gray-200 text-gray-800 py-2 px-4 rounded-md hover:bg-gray-300 transition-colors duration-200 mb-8 shadow-md no-print">',
  '                    <i class="fas fa-moon text-xl"></i>',
  '                    <span class="font-medium">Toggle Dark Mode</span>',
  '                </button>',
  '',
  '                <div class="mt-auto pt-6 border-t border-gray-200 w-full text-center lg:text-left">',
  '                    <h3 class="text-lg font-semibold text-gray-800 mb-3">Contact:</h3>',
  contact_info_html,
  '                </div>',
  '            </aside>',
  '',
  '            <main class="lg:w-3/4 flex-grow">',
  '                <section class="bg-white p-6 rounded-xl shadow-lg mb-8">',
  '                    <div class="py-4 mb-4 border-b border-gray-200 bg-gray-50 -mx-6 px-6 rounded-t-xl hover:bg-gray-100 transition-colors duration-200">',
  '                        <h2 class="text-2xl font-semibold text-gray-800 flex items-center">',
  '                            <i class="fas fa-flask mr-3 text-indigo-500"></i> Research Areas',
  '                        </h2>',
  '                    </div>',
  '                    <div id="research-areas-content" class="content-area">',
  research_areas_content_html,
  '</div>',
  '                </section>',
  '',
  '                <section class="bg-white p-6 rounded-xl shadow-lg mb-8">',
  '                    <div class="py-4 mb-4 border-b border-gray-200 bg-gray-50 -mx-6 px-6 rounded-t-xl hover:bg-gray-100 transition-colors duration-200">',
  '                        <h2 class="text-2xl font-semibold text-gray-800 flex items-center">',
  '                            <i class="fas fa-project-diagram mr-3 text-indigo-500"></i> Current Projects',
  '                        </h2>',
  '                    </div>',
  '                    <div id="current-projects-content" class="content-area space-y-4">',
  current_projects_content_html,
  '</div>',
  '                </section>',
  '',
  '                <section class="bg-white p-6 rounded-xl shadow-lg mb-8">',
  '                    <div class="py-4 mb-4 border-b border-gray-200 bg-gray-50 -mx-6 px-6 rounded-t-xl hover:bg-gray-100 transition-colors duration-200">',
  '                        <h2 class="text-2xl font-semibold text-gray-800 flex items-center">',
  '                            <i class="fas fa-briefcase mr-3 text-indigo-500"></i> Employment',
  '                        </h2>',
  '                    </div>',
  '                    <div id="employment-content" class="content-area space-y-4">',
  employment_content_html,
  '</div>',
  '                </section>',
  '',
  '                <section class="bg-white p-6 rounded-xl shadow-lg mb-8">',
  '                    <div class="py-4 mb-4 border-b border-gray-200 bg-gray-50 -mx-6 px-6 rounded-t-xl hover:bg-gray-100 transition-colors duration-200">',
  '                        <h2 class="text-2xl font-semibold text-gray-800 flex items-center">',
  '                            <i class="fas fa-star mr-3 text-indigo-500"></i> Additional Experience',
  '                        </h2>',
  '                    </div>',
  '                    <div id="additional-experience-content" class="content-area space-y-4">',
  additional_experience_content_html,
  '</div>',
  '                </section>',
  '',
  '                <section class="bg-white p-6 rounded-xl shadow-lg mb-8">',
  '                    <div class="py-4 mb-4 border-b border-gray-200 bg-gray-50 -mx-6 px-6 rounded-t-xl hover:bg-gray-100 transition-colors duration-200">',
  '                        <h2 class="text-2xl font-semibold text-gray-800 flex items-center">',
  '                            <i class="fas fa-microphone-alt mr-3 text-indigo-500"></i> Media Features',
  '                        </h2>',
  '                    </div>',
  '                    <div id="media-features-content" class="content-area space-y-4">',
  media_features_content_html,
  '</div>',
  '                </section>',
  '',
  '                <section class="bg-white p-6 rounded-xl shadow-lg mb-8">',
  '                    <div class="py-4 mb-4 border-b border-gray-200 bg-gray-50 -mx-6 px-6 rounded-t-xl hover:bg-gray-100 transition-colors duration-200">',
  '                        <h2 class="text-2xl font-semibold text-gray-800 flex items-center">',
  '                            <i class="fas fa-book-open mr-3 text-indigo-500"></i> Publications and Conference Presentations',
  '                        </h2>',
  '                    </div>',
  '                    <div id="publications-content" class="content-area space-y-6">',
  publications_content_html,
  '</div>',
  '                </section>',
  '',
  '                <section class="bg-white p-6 rounded-xl shadow-lg mb-8">',
  '                    <div class="py-4 mb-4 border-b border-gray-200 bg-gray-50 -mx-6 px-6 rounded-t-xl hover:bg-gray-100 transition-colors duration-200">',
  '                        <h2 class="text-2xl font-semibold text-gray-800 flex items-center">',
  '                            <i class="fas fa-newspaper mr-3 text-indigo-500"></i> Non-academic Publications',
  '                        </h2>',
  '                    </div>',
  '                    <div id="non-academic-publications-content" class="content-area space-y-4">',
  non_academic_publications_content_html,
  '</div>',
  '                </section>',
  '',
  '                <section class="bg-white p-6 rounded-xl shadow-lg mb-8">',
  '                    <div class="py-4 mb-4 border-b border-gray-200 bg-gray-50 -mx-6 px-6 rounded-t-xl hover:bg-gray-100 transition-colors duration-200">',
  '                        <h2 class="text-2xl font-semibold text-gray-800 flex items-center">',
  '                            <i class="fas fa-heart mr-3 text-indigo-500"></i> Hobbies and Interests',
  '                        </h2>',
  '                    </div>',
  '                    <div id="hobbies-content" class="content-area">',
  hobbies_content_html,
  '</div>',
  '                </section>',
  '            </main>',
  '        </div>',
  '    </div>',
  '    <script>',
  '        document.addEventListener(\'DOMContentLoaded\', () => {',
  '            const savedTheme = localStorage.getItem(\'theme\');',
  '            if (savedTheme === \'dark\') {',
  '                document.body.classList.add(\'dark\');',
  '                updateThemeToggleButton(\'light\');',
  '            } else {',
  '                updateThemeToggleButton(\'dark\');',
  '            }',
  '            document.getElementById(\'theme-toggle\').addEventListener(\'click\', toggleTheme);',
  '        });',
  '',
  '        function toggleTheme() {',
  '            const body = document.body;',
  '            const themeToggleBtn = document.getElementById(\'theme-toggle\');',
  '            if (body.classList.contains(\'dark\')) {',
  '                body.classList.remove(\'dark\');',
  '                localStorage.setItem(\'theme\', \'light\');',
  '                updateThemeToggleButton(\'dark\');',
  '            } else {',
  '                body.classList.add(\'dark\');',
  '                localStorage.setItem(\'theme\', \'dark\');',
  '                updateThemeToggleButton(\'light\');',
  '            }',
  '        }',
  '',
  '        function updateThemeToggleButton(mode) {',
  '            const themeToggleBtn = document.getElementById(\'theme-toggle\');',
  '            const icon = themeToggleBtn.querySelector(\'i\');',
  '            const text = themeToggleBtn.querySelector(\'span\');',
  '            if (mode === \'dark\') {',
  '                icon.className = \'fas fa-moon text-xl\';',
  '                text.textContent = \'Toggle Dark Mode\';',
  '                themeToggleBtn.classList.remove(\'bg-gray-800\', \'text-white\', \'hover:bg-gray-700\');',
  '                themeToggleBtn.classList.add(\'bg-gray-200\', \'text-gray-800\', \'hover:bg-gray-300\');',
  '            } else {',
  '                icon.className = \'fas fa-sun text-xl\';',
  '                text.textContent = \'Toggle Light Mode\';',
  '                themeToggleBtn.classList.remove(\'bg-gray-200\', \'text-gray-800\', \'hover:bg-gray-300\');',
  '                themeToggleBtn.classList.add(\'bg-gray-800\', \'text-white\', \'hover:bg-gray-700\');',
  '            }',
  '        }',
  '    </script>',
  '</body>',
  '</html>'
), collapse = "\n")

# Save the HTML content to a file
output_file <- "academic_profile.html"
writeLines(full_html_content, output_file)

cat(sprintf("HTML file generated successfully: %s\n", output_file))
