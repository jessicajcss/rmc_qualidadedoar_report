# Relatório Técnico Interativo — Qualidade do Ar (RMC)

Este repositório contém:
- Livro Quarto com análise meteorológica e de qualidade do ar
- Integração com dados remotos (repositório ScheduledUpdate)
- Código do dashboard Shiny (app.R) para visualização dinâmica
- Publicação automatizada em GitHub Pages via GitHub Actions

## Estrutura

```
rmc-qualidade-ar-relatorio/
├─ _quarto.yml
├─ index.qmd
├─ 01-dados.qmd
├─ 02-meteorologia.qmd
├─ 03-shiny-dashboard.qmd
├─ 04-qualidade-ar.qmd
├─ 05-fontes-meteo.qmd
├─ 06-mapas.qmd
├─ 07-estatisticas.qmd
├─ 08-anexos.qmd
├─ styles.scss
├─ app.R
├─ scripts/
│  ├─ install_packages.R
│  ├─ 00-basic_functions_meteo.R
│  ├─ 00-load_data.R
│  ├─ helpers_aqi.R
├─ .github/workflows/publish.yml
├─ .gitignore
```

## Uso

1. Instale pacotes:
```r
source("scripts/install_packages.R")
```
2. Render:
```bash
quarto render
```
3. Publicação automática via Actions (branch gh-pages).

## Dados

- Carregados de:  
  - `https://github.com/jessicajcss/ScheduledUpdate/...`
- Se quiser trabalhar offline, coloque os arquivos equivalentes dentro de `data/`.

## Shiny

O arquivo `app.R` utiliza os mesmos objetos carregados (ou baixa via URL). Renderize localmente:
```r
shiny::runApp()
```

## Licença

Sugestão: MIT. Crie `LICENSE`.

## Contato

[jessicajcss](https://github.com/jessicajcss)
