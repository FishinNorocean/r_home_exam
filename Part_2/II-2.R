# Question 2

# setup
rm(list = ls())
library(ggplot2)
library(dplyr)
library(tools)

# (1)

# load and pre-process data
df_source <- read.csv('data/pokedex.csv')
df_source$type <- as.factor(df_source$type)
df_source$is_legendary <- as.factor(df_source$is_legendary)
df_source <- tibble(df_source)

# first 6 rows
first_6_df_source <- head(df_source)
write.csv(first_6_df_source, file = 'output/Q2_1_first_6_rows.csv', row.names = FALSE)

# proportion calculation
legen_prop <- mean(df_source$is_legendary == 1)
nlege_prop <- mean(df_source$is_legendary == 0)
print(paste('Within our dataset,',legen_prop, 'of pokemons are legendary, while', nlege_prop, 'are not.'))


# (2)
# (2) (i)
height_weight_plot <- ggplot(df_source, aes(weight_kg, height_m)) +
  geom_point(aes(color = is_legendary)) +
  geom_text(aes(label = ifelse(weight_kg > 600 | height_m > 7.5, name, "")), 
            vjust = -0.5) +
  scale_color_manual(name = "Legendary or not", 
                     values=c("lightblue", "gold"), 
                     labels=c("Ordinary","Legendary")) +
  labs(title="Height against Weight among all pokemons", 
       x="Weight, in kilograms", 
       y="Height, in meters") +
  xlim(0, 1200)
ggsave(height_weight_plot, 
       filename = "output/Q2_2_1_Height_weight_plot.png", 
       height = 6, width = 9)

# (2) (ii)
  
type_legen <- group_by(df_source, type) %>%
  summarise(proportion=mean(is_legendary==1))
type_legen_bar <- ggplot(type_legen, aes(type, proportion)) +
  geom_bar(stat="identity", fill = 'lightblue') +
  theme(aspect.ratio = 2/3, axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(x = "Type", y = "proportion", title = "Type and Lengendary Proportion")
ggsave(type_legen_bar, filename = "output/Q2_2_2_type_legen_barplot.png")

# (2) (iii)
get_box_plot <- function(index){
  plt <- ggplot(df_source, 
              aes(is_legendary, .data[[index]], fill = is_legendary)) +
  geom_boxplot() +
  labs(x = 'Legendary or not', y = index,
       title = paste(toTitleCase(index), "Comparison Between Legendary and Ordinary")) +
  scale_x_discrete(labels = c("0" = "Ordinary", "1" = "Legendary")) +
  scale_fill_manual(values=c("0"="lightblue","1"="gold")) +
  guides(fill = FALSE)
  return(plt)
}
objs <- c("attack","sp_attack","defense","sp_defense","hp","speed")
for (index in objs) {
  plt <- get_box_plot(index)
  ggsave(plt, filename = paste('output/Q2_2_3/Q2_2_3', 
                               toTitleCase(index), '_Boxplot.png',sep = ''))
}

