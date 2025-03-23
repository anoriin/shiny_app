# Shiny App

A modular Shiny application for exploring and visualizing metagenomic data from FMT clinical trials.

## 📦 Features

- 🖥️ User-friendly interface
- 📊 Metagenomic statistical analysis tools
- 📚 Educational content for researchers and students

## 🚀 Getting Started

### Clone the Repository and Create Docker Image

```bash
git clone https://github.com/anoriin/shiny_app.git
cd shiny_app
docker build -t shiny_app_image .
docker run -d -p 3838:3838 shiny_app_image
