library(ggplot2)
library(dplyr)

# Definir o diretório de trabalho (coloque o caminho que voce usa no seu PC)
setwd("C:/Users/vitor/OneDrive/Documentos/Materias_UNB/CE1/ProjetoFinal_CE1")

#### Lendo os dados
rawWages <- read.csv("raw_wages.csv")
wagesClaned <- read.csv("wages_cleaned.csv")

#### TOPICO 1
salarios <- wagesClaned[,"Salary"]
participacaoSelecao <- wagesClaned[,"Caps"]

# Calcula correlacao de Person usando funcao do R
correlacaoPearson <- cor(salarios, participacaoSelecao, method = "pearson")

data <- data.frame(Salario = salarios, ParticipacaoSelecao = participacaoSelecao)

# Calcular a média salarial para cada valor de Caps
media_salarial_por_caps <- data %>%
  group_by(ParticipacaoSelecao) %>%
  summarise(media_salarial = mean(Salario, na.rm = TRUE))

# Criar o gráfico de linha contínua da média salarial em função das participações na seleção
ggplot(media_salarial_por_caps, aes(x = ParticipacaoSelecao, y = media_salarial)) +
  geom_line(color = "blue") +  # Adiciona uma linha contínua azul
  labs(title = "Média Salarial em Função das Participações na Seleção",
       x = "Participações na Seleção (Caps)",
       y = "Média Salarial") +
  theme_minimal()


#### TOPICO 2
# Calcular as estatísticas resumo por idade
summary_stats <- wagesClaned %>%
  group_by(Age) %>%
  summarise(
    mean_salary = mean(Salary, na.rm = TRUE),
    sd_salary = sd(Salary, na.rm = TRUE)
  )

# Adicionar uma coluna para a área do desvio padrão para usar na legenda
summary_stats$fill_group <- "Desvio Padrão"

# Criar o gráfico com legendas
ggplot(summary_stats, aes(x = Age, y = mean_salary)) +
  geom_line(aes(color = "Média Salarial"), size = 1) +  # Linha da média salarial
  geom_ribbon(aes(ymin = mean_salary - sd_salary, ymax = mean_salary + sd_salary, fill = fill_group), alpha = 0.3) +  # Área do desvio padrão
  geom_smooth(aes(color = "Regressão Linear"), method = "lm", linetype = "dashed", size = 1) +  # Linha de regressão linear
  labs(title = "Média Salarial por Idade com Desvio Padrão",
       x = "Idade (anos)",
       y = "Média Salarial",
       fill = "Legenda",
       color = "Legenda",
       caption = "Fonte: Dataset de Salários de Jogadores") +
  scale_y_continuous(limits = c(0, 1.1 * max(summary_stats$mean_salary, na.rm = TRUE))) +  # Ajustar a escala do eixo y para uma melhor visualização
  scale_color_manual(values = c("Média Salarial" = "darkgreen", "Regressão Linear" = "blue")) +
  scale_fill_manual(values = c("Desvio Padrão" = "gray")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        legend.position = "bottom")

#### TOPICO 3
# Adicionar a coluna Is_top_5_League à tabela rawWages
rawWages$Is_top_5_League <- wagesClaned$Is_top_5_League

# Criar uma coluna que categoriza as ligas em Top 5 Ligas e Outras Ligas
rawWages$League_Category <- ifelse(rawWages$Is_top_5_League, "Top 5 Ligas", "Outras Ligas")

# Remover caracteres não numéricos e converter para numérico
rawWages$Salary <- as.numeric(gsub("[^0-9]", "", rawWages$Salary))

# Dividir os dados em duas categorias
salarios_top5 <- rawWages$Salary[rawWages$League_Category == "Top 5 Ligas"]
salarios_outras <- rawWages$Salary[rawWages$League_Category == "Outras Ligas"]

teste_t <- t.test(salarios_top5, salarios_outras)

# Imprimir os resultados do teste t
print(teste_t)
# Criar o boxplot
ggplot(rawWages, aes(x = League_Category, y = Salary, fill = League_Category)) +
  geom_boxplot(outlier.size = 2, size = 1, width = 0.5) +  # Ajustar o tamanho das linhas das caixas
  labs(title = "Comparação de Salários: Top 5 Ligas vs Outras Ligas",
       x = "Categoria da Liga",
       y = "Salário") +
  scale_fill_manual(values = c("Top 5 Ligas" = "blue", "Outras Ligas" = "gray")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        legend.position = "none",
        panel.grid.major = element_blank(),  # Remover linhas de grade principais
        panel.grid.minor = element_blank(),  # Remover linhas de grade secundárias
        panel.border = element_blank(),      # Remover borda do painel
        axis.line = element_line(size = 0.5, colour = "black"))  # Ajustar linhas dos eixos
