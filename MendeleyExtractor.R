library(RSQLite) # Database
library(dplyr) # This really should be loaded by default always...
library(tidyr) # 'spread' function to change table from 'long' to 'wide' format 
library(readr)
library(magrittr)
library(purrr)

# Uncomment the following line with your Mendeley path.
# See here for more information: http://support.mendeley.com/customer/en/portal/articles/227951-how-do-i-locate-mendeley-desktop-database-files-on-my-computer-

mendeley.path = "C:/Users/braym/AppData/Local/Mendeley Ltd./Mendeley Desktop/braymath@umich.edu@www.mendeley.com.sqlite"

# Connect to the database
mendeley.connection = dbConnect(RSQLite::SQLite(),mendeley.path)

# Some of the tables available in the Mendeley database
head(dbListTables(mendeley.connection),n=10)

# The variables available in the 'Documents' table
dbListFields(mendeley.connection,"Documents")


extract.table <- function(con,query){
  
  res <- dbSendQuery(con,query) # Send query
  
  table <- dbFetch(res) # Fetch table
  
  dbClearResult(res) # Free resources
  
  return(table)
  
}

### Folder

dbListFields(mendeley.connection,"Folders")

folders <- extract.table(mendeley.connection, "SELECT id, name FROM Folders")

dbListFields(mendeley.connection,"DocumentFolders")

document.folders <- extract.table(mendeley.connection, "SELECT folderId, documentId FROM DocumentFolders")

relevant.folder.name <- "Networks"

# Extract interal ID for folder of interest
relevant.folder.id <- folders %>%
  filter(name == relevant.folder.name) %>%
  .$id %>%
  extract(1)

# Extract internal IDs for all papers belonging to the folder of interest, using the folder ID
relevant.papers <- document.folders %>%
  filter(folderId == relevant.folder.id) %>%
  .$documentId

head(relevant.papers)


### Documents

# Collect title and citation key for all relevant documents
relevant.documents <- extract.table(mendeley.connection,"SELECT id, title, citationKey FROM Documents") %>% 
  filter(id %in% relevant.papers) %>%
  rename(documentId = id)


### Authors

dbListFields(mendeley.connection,"DocumentContributors")

# Collect and concatenate authors for all relevant documents
authors <- extract.table(mendeley.connection,"SELECT * FROM DocumentContributors")

head(authors)

unique(authors$contribution)

# Collect and concatenate authors for all relevant documents
relevant.authors <- authors %>%
  filter(contribution == "DocumentAuthor",
         documentId %in% relevant.papers) %>%
  mutate(fullName = paste(lastName,firstNames,sep=", ")) %>% # Concatenate first and last name
  select(documentId,fullName) %>%
  group_by(documentId) %>%
  summarize(authorsNames = paste(fullName,collapse="; ")) # Concatenate all authors of a document together

head(relevant.authors)

### Tags

dbListFields(mendeley.connection,"DocumentTags")

# Collect and concatenate tags for all relevant documents
relevant.tags <- extract.table(mendeley.connection, "SELECT * FROM DocumentTags") %>%
  filter(documentId %in% relevant.papers) %>%
  group_by(documentId) %>%
  summarize(tagList = paste(tag,collapse="; "))

head(relevant.tags)

### Notes

dbListFields(mendeley.connection,"FileNotes")

# Collect notes
relevant.notes <- extract.table(mendeley.connection,"SELECT documentId, note FROM FileNotes") %>%
  filter(documentId %in% relevant.papers) %>%
  rowwise() %>%
  mutate(type = tolower(trimws(unlist(strsplit(note,split=":")))[1]), # Extract for each document whether it is a 'Goal' or a 'Key'
         note = paste(unlist(strsplit(note,split=":"))[-1],collapse=":")) %>% # Extract the actual note
  ungroup() %>%
  spread(type,note)

head(relevant.notes)


### Final Dataset

# Join the datasets together
relevant.files <- relevant.documents %>%
  left_join(relevant.authors, by="documentId") %>%
  left_join(relevant.tags, by="documentId") %>%
  left_join(relevant.notes, by="documentId")

head(relevant.files)

write_csv(relevant.files, "MendeleyNetworkFiles.csv")

#####################################

relevant.files <- read_csv("MendeleyNetworkFiles.csv")

tags <- relevant.files$tagList %>%
  strsplit(";") %>%
  unlist %>%
  trimws %>%
  tolower %>%
  unique

relevant.files$tagList <- tolower(relevant.files$tagList)

tags <- tags[c(1,2,3,4,5,6,9,11,12,15,32)]

retreive.papers <- function(tag){

  papers <- relevant.files %>%
    filter(grepl(tagList,pattern=tag,fixed=T)) %>%
    mutate(tag = tag) %>%
    select(tag, title)
  
  return(papers)
}

papers <- map_df(tags, retreive.papers) %>%
  mutate(weight=1,
         tag = case_when(tag == "multilayer networks" ~ "multilayer network",
                         TRUE ~ tag))


papers[papers$tag == "multilayer networks",]$tag <- "multilayer network"



library(googleVis)

plot(gvisSankey(papers, from="tag", to="title", weight="weight",
                options=list(height=800, width=850,
                             sankey="{
                             link:{color:{fill: 'lightgray', fillOpacity: 0.7}},
                             node:{nodePadding: 5, label:{fontSize: 12}, interactivity: true, width: 20},
                             }")
)
                )
