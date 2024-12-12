# Charger la bibliothèque nécessaire
library(tidyverse)

# Remplacer par le chemin du fichier CSV
file_path <- "C:/Users/Alexa/OneDrive/Bureau/contributions-pro-fr.csv"  # Chemin vers votre fichier

# Charger le fichier CSV en spécifiant le séparateur ";"
df <- read_delim(file_path, delim = ";", locale = locale(encoding = "latin1"), col_names = TRUE)

# Filtrer les données pour l'année 2023 et les entités politiques spécifiées
entities_2023 <- c("Coalition avenir Québec - L'équipe François Legault",
                   "Parti libéral du Québec/Quebec Liberal Party",
                   "Québec solidaire",
                   "Parti québécois")

df_2023_filtered <- df %>%
  filter(`Année financière` == 2023, `Entité politique` %in% entities_2023)

# Créer une nouvelle colonne pour séparer les dons à Montréal et les autres dons
df_2023_filtered <- df_2023_filtered %>%
  mutate(
    Montreal = if_else(`Municipalité` == "Montréal", 1, 0),
    Non_Montreal = if_else(`Municipalité` != "Montréal", 1, 0)
  )

# Cumuler les dons pour chaque entité politique et chaque catégorie
donations_combined <- df_2023_filtered %>%
  group_by(`Entité politique`) %>%
  summarise(
    Dons_a_Montréal = sum(Montreal),
    Dons_autres_que_Montréal = sum(Non_Montreal)
  )

# Créer un graphique avec ggplot2
donations_combined %>%
  pivot_longer(cols = starts_with("Dons"), names_to = "Type_don", values_to = "Nombre_de_dons") %>%
  ggplot(aes(x = `Entité politique`, y = Nombre_de_dons, fill = Type_don)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Nombre_de_dons), position = position_dodge(width = 0.9), vjust = -0.5) +  # Afficher les valeurs au-dessus des barres
  labs(
    title = "Cumul des dons par entité politique en 2023",
    x = "Entité politique",
    y = "Nombre de dons",
    fill = "Type de don"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels pour les entités politiques

