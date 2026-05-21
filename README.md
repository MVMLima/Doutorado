# Previsão de Agravos Tropicais com Modelos de Séries Temporais

Repositório com os scripts, dados e resultados das análises que compõem a tese de doutorado **"Modelos de Previsão Aplicados a Agravos Tropicais"**, desenvolvida no **Programa de Ciências da Saúde do Centro Universitário do ABC (FMABC)**, Santo André -- SP. O projeto contou com o apoio financeiro do **Acre Project -- Health in the Western Amazonia** (acordo multi-institucional n. 007/2015, SESACRE--UFAC--FMABC).

## Doenças e Dados

O estudo aplica modelos de previsão de séries temporais à incidência mensal de três doenças tropicais em estados brasileiros:

| Doença | Estados | Período |
|---|---|---|
| **Dengue** | Acre, Alagoas, Amazonas, Amapá, Bahia, Ceara, Distrito Federal, Espirito Santo, Goiais, Maranhão, Mato Grosso, Mato Grosso do Sul, Minas Gerais, Para, Paraíba, Pernambuco, Piaui, Paraná, Rio de Janeiro, Rio Grande do Norte, Rondônia, Roraima, Rio Grande do Sul, Santa Catarina, Sergipe, São Paulo, Tocantins  | 2000--2018 |
| **Leishmaniose** | Bahia, Ceará, Maranhão, Minas Gerais, Mato Grosso do Sul, Pará, Piauí, Rio Grande do Norte e Tocantins | 2001--2018 |
| **Malária** | Estados da Amazônia Legal Brasileira 1997--2016 |

## Modelos Utilizados

Para cada doença e estado, 10 modelos de previsão são comparados em três horizontes de previsão (3, 6 e 12 meses à frente), usando o logaritmo dos casos mensais. Os modelos são ranqueados pelo **MASE** (*Mean Absolute Scaled Error*).

| Modelo | Função R |
|---|---|
| Naïve (baseline) | `naive()` |
| ARIMA automático | `auto.arima()` |
| Suavização Exponencial (ETS) | `ets()` |
| Rede Neural (NNETAR) | `nnetar()` |
| TBATS | `tbats()` |
| BATS | `bats()` |
| Decomposição STL + ETS | `stlm()` |
| Modelo Estrutural | `StructTS()` |
| Extreme Learning Machine | `elm()` |
| Multilayer Perceptron | `mlp()` |

## Estrutura do Repositório

```
.
├── README.md
├── Banco Dengue.xlsx                         # Dados brutos de Dengue
├── Banco Leishmania.RData                    # Dados de Leishmaniose (R binário)
├── Banco Malária.RData                       # Dados de Malária (R binário)
├── Script Análise Dengue Final + Gráficos.R      # Dengue: AC e SC (2000--2018)
├── Script Análise Dengue Final + Gráficos2000.R  # Dengue: MA e TO (2000--2018)
├── Script Análise Dengue Final + Gráficos2001.R  # Dengue: PB e AP (2001--2018)
├── Script Análise Leishmania Final + Gráficos.R  # Leishmaniose: MS (2001--2018)
└── Script Malária Final.R                        # Malária: TO e outro estado (1997--2016)
```

## Requisitos

- **R** ≥ 3.5
- Pacotes R:
  ```r
  install.packages(c("forecast", "TSA", "urca", "tseries", "ggplot2",
                     "seasonal", "tsoutliers", "expsmooth", "fma", "nnfor"))
  ```

## Como Executar

1. Clone o repositório:
   ```bash
   git clone https://github.com/MVMLima/Doutorado.git
   ```

2. **Ajuste os caminhos** nos scripts: os arquivos `.R` contêm caminhos absolutos (`C:/Users/Lucas/...`). Altere `setwd()` e `load()` para apontar para o diretório local do repositório.

3. **Arquivo de dados da Dengue**: os scripts de Dengue carregam `Banco Dengue Base.RData`, mas o repositório fornece `Banco Dengue.xlsx`. Converta o arquivo Excel para `.RData` (ex.: com `readxl::read_excel()` e `save()`) ou ajuste o script para ler diretamente o `.xlsx`.

4. Execute os scripts no R/RStudio:
   ```r
   source("Script Análise Dengue Final + Gráficos.R")
   ```

## Resultados

Cada script gera:
- Tabela de acurácia (MASE) comparando os 10 modelos nos horizontes de 3, 6 e 12 meses.
- Gráficos em PNG com as previsões sobrepostas aos valores reais.

## Autor

**Marcos Venicius Malveira de Lima** -- [enf.econ@gmail.com](mailto:mvmlima@hotmail.com)

Orientação: **Prof. Dr. Gabriel Zorello Laporta** Programa de Ciências da Saúde -- Centro Universitário do ABC (FMABC)
