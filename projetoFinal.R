library(tidyr)
library(ggplot2)
library(dplyr)

# Definir o diretório de trabalho (coloque o caminho que voce usa no seu PC)
setwd("C:/Users/vitor/OneDrive/Documentos/Materias_UNB/CE1/ProjetoFinal_CE1")

#### Lendo os dados
rawWages <- read.csv("raw_wages.csv")
rawWages$Begins
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


#### Tópico 4 
nacionalidades <- rawWages %>%
  group_by(Nat) %>%
  summarise(
    count = length(Nat)
  )

#filtrar os jogadores por idade 
rawWages_clean <- rawWages %>%
  filter(!is.na(Salary) & is.numeric(Salary) & Age >= 25 & Age <= 35)

#Identificar as 5 nacionalidades mais predominantes
top_5_nationalities <- rawWages_clean %>%
  group_by(Nat) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  top_n(5, count) %>%
  pull(Nat)


#Obter os jogadores das 5 nacionalidades mais predominantes
rawWages_clean <- rawWages_clean %>%
  filter(Nat %in% top_5_nationalities)

# Executar a ANOVA
anova_result <- aov(Salary ~ Nat, data = rawWages_clean)

# Ver os resultados da ANOVA
summary(anova_result)

# Teste Tukey HSD (Honest Significant Difference)
tukey_result <- TukeyHSD(anova_result)

# Ver os resultados do teste Tukey
print(tukey_result)

# Extrair resultados do teste de Tukey
tukey_data <- as.data.frame(tukey_result$Nat)
tukey_data$comparison <- rownames(tukey_data)

# Gráfico dos Resultados do Teste de Tukey HSD
ggplot(tukey_data, aes(x = comparison, y = diff, ymin = lwr, ymax = upr)) +
  geom_errorbar(width = 0.2) +
  geom_point() +
  labs(title = "Resultados do Teste de Tukey HSD",
       x = "Comparação de Nacionalidades",
       y = "Diferença de Salário") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


##### TOPICO 6 (4)
# Adicionar a coluna Based_rich_nation ao dataframe rawWages
rawWages["Based_rich_nation"] = wagesClaned$Based_rich_nation

# Filtrar jogadores por nações ricas e pobres
ricas <- rawWages %>% filter(Based_rich_nation == 1)
pobres <- rawWages %>% filter(Based_rich_nation == 0)

# Calcular o terceiro quartil para cada grupo
Q3_ricas <- quantile(ricas$Salary, 0.75, na.rm = TRUE)
Q3_pobres <- quantile(pobres$Salary, 0.75, na.rm = TRUE)

# Filtrar salários até o terceiro quartil
rawWages_filtered <- rawWages %>%
  filter((Based_rich_nation == 1 & Salary <= Q3_ricas) |
           (Based_rich_nation == 0 & Salary <= Q3_pobres))

# Criar um fator com base na riqueza da nação
fator_ordenados <- factor(rawWages_filtered$Based_rich_nation, levels = c(0, 1))

# Plotar o boxplot ajustado
ggplot(data = rawWages_filtered, aes(x = fator_ordenados, y = Salary, fill = as.factor(Based_rich_nation))) +
  geom_boxplot() +
  labs(title = "Salários por Pertencimento a Nações Ricas",
       x = "Riqueza da Nação",
       y = "Salário") +
  scale_fill_manual(values = c("0" = "red", "1" = "blue"), 
                    name = "Riqueza da Nação",
                    labels = c("Pobre", "Rica")) +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

#### TOPICO 5
# Calcular a duração do contrato em anos, arredondar para inteiros e garantir que sejam não-negativos
rawWages <- rawWages %>%
  mutate(Contract_Duration = pmax(0, round(as.numeric(difftime(Expires, Begins, units = "days")) / 365)))

# Criar a coluna categórica para duração do contrato
rawWages <- rawWages %>%
  mutate(Contract_Duration_Category = factor(Contract_Duration))

# Remover linhas com valores ausentes ou não numéricos nas colunas Salary e Contract_Duration
rawWages_clean <- rawWages %>%
  filter(!is.na(Salary) & !is.na(Contract_Duration) & is.finite(Salary))

# Executar a ANOVA
anova_Contract <- aov(Salary ~ Contract_Duration_Category, data = rawWages_clean)

# Ver os resultados da ANOVA
summary(anova_Contract)

# Criar o gráfico boxplot
ggplot(data = rawWages_clean, aes(x = factor(Contract_Duration_Category), y = Salary)) +
  geom_boxplot(outlier.shape = NA) +  # Remover outliers para focar no terceiro quartil
  stat_summary(fun = median, geom = "point", shape = 20, size = 2, color = "red", fill = "red") +  # Adicionar ponto da mediana
  labs(title = "Distribuição dos Salários por Duração do Contrato",
       x = "Duração do Contrato (anos)",
       y = "Salário") +
  scale_fill_brewer(palette = "Set3") +  # Adicionar uma paleta de cores
  theme_grey() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 6),  # Diminuir a fonte do eixo x
        axis.text = element_text(size = 10)) +
  coord_cartesian(ylim = c(0, quantile(rawWages_clean$Salary, 0.80)))  # Limitar o eixo y ao terceiro quartil


# Calcular a correlação de Pearson entre salário e duração do contrato
salarios <- rawWages_clean$Salary
duracao <- rawWages_clean$Contract_Duration

# Calcula a correlação de Pearson usando a função do R
correlacaoPearson <- cor(salarios, duracao, method = "pearson")
correlacaoPearson


#### TOPICO 6 falta dados
# Remover linhas com valores ausentes ou não numéricos nas colunas Salary e Position
rawWages_clean <- rawWages %>%
  filter(!is.na(Salary) & !is.na(Position) & is.finite(Salary))

# Certificar que Position é um fator
rawWages_clean$Position <- as.factor(rawWages_clean$Position)

# Executar a ANOVA
anova_Position <- aov(Salary ~ Position, data = rawWages_clean)

# Ver os resultados da ANOVA
summary(anova_Position)


# Criar o gráfico boxplot
ggplot(data = rawWages_clean, aes(x = Position, y = Salary, fill = Position)) +
  geom_boxplot(outlier.shape = NA) +  # Remover outliers para focar no terceiro quartil
  stat_summary(fun = median, geom = "point", shape = 20, size = 2, color = "red", fill = "red") +  # Adicionar ponto da mediana
  labs(title = "Distribuição dos Salários por Posição",
       x = "Posição",
       y = "Salário") +
  scale_fill_brewer(palette = "Set3") +  # Adicionar uma paleta de cores
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text.x = element_text(size = 6, angle = 45, hjust = 1),  # Diminuir a fonte do eixo x e inclinar os textos
        axis.text = element_text(size = 10)) +
  coord_cartesian(ylim = c(0, quantile(rawWages_clean$Salary, 0.80)))  # Limitar o eixo y ao terceiro quartil


# Contar o número de jogadores por posição
jogadores_por_posicao <- rawWages_clean %>%
  group_by(Position) %>%
  summarise(count = n())

# Exibir o resultado
print(jogadores_por_posicao)


#### VERIFICAÇÃO DE POPULAÇÃO NO BD
View(rawWages)

#### TOPICO 6
# Contar o número de jogadores por divisão e selecionar as 5 mais predominantes
top_divisions <- names(sort(table(rawWages$Division), decreasing = TRUE))[1:5]

rawWages_top_divisions <- rawWages[rawWages$Division %in% top_divisions, ]

# Remover caracteres não numéricos e converter para numérico
rawWages_top_divisions$Salary <- as.numeric(gsub("[^0-9]", "", rawWages_top_divisions$Salary))

# Criar uma coluna para categorizar as divisões
rawWages_top_divisions$Division_Category <- factor(rawWages_top_divisions$Division, levels = top_divisions)

#Executar ANOVA e Tukey HSD (substitua com seus dados reais)
anova_result <- aov(Salary ~ Division_Category, data = rawWages_top_divisions)
tukey_result <- TukeyHSD(anova_result)

# Extrair resultados do teste de Tukey HSD e preparar os dados para o gráfico
tukey_data <- as.data.frame(tukey_result$Division_Category)
tukey_data$comparison <- rownames(tukey_data)

# Verificar a estrutura de tukey_data para garantir que contém as colunas necessárias
#str(tukey_data)

print(tukey_result)

# Gráfico dos Resultados do Teste de Tukey HSD com ajustes
ggplot(tukey_data, aes(x = comparison, y = diff, ymin = lwr, ymax = upr)) +
  geom_errorbar(width = 0.2, color = "blue") +
  geom_point(size = 3, color = "red") +
  labs(title = "Resultados do Teste de Tukey HSD",
       x = "Comparação de Divisões",
       y = "Diferença de Salário") +
  theme_minimal(base_size = 10) +  # Ajustar tamanho do texto base
  theme(axis.text.x = element_text(angle = 60, hjust = 0.5, vjust = 0.5),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Centralizar e ajustar tamanho do título
        legend.key.size = unit(0.15, "cm")) +  # Reduzir o tamanho da legenda
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")  # Adicionar linha de referência

