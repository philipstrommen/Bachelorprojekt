# Bachelorprojekt
Læs nedenstående før egen test af kode

I projektet anvendes forskelligt data, alt efter hvordan koden er kørt.

I starten af projektet anvendes datataen DNK.xlsx som er fra den 6. februar.

Efter det anvendes dataen DNK_17_3.xlsx som er fra den 17. marts, her er grundspillet færdiggjort og der er 10 runder tilbage.

Til sidst bruges dataen Superliga_slut.xlsx som er taget fra når superligaen er færdigspillet. 

# Henvisning
I få tilfælde er følgende to Github-links brugt som inspiration til kode, samt teori om Fisher Scoring.
https://andrewcharlesjones.github.io/journal/fisher-scoring.html
https://github.com/mariegudum/Bachelorprojekt/tree/main

## Indlæsning af data
Hvis du vil indlæse data når du tilpasse koden "read_excel("/Users/philipstrommen/Desktop/Bachelor/DNK.xlsx")" til hvor du har dit data liggende. Mit ligger på skrivebordet i min Bachelor mappe. 

## Indlæsning af pakker
Der er brugt følgende pakker, så kør dem i startet af dit dokument
library(readxl)
library(tidyverse)
library(stats)
library(patchwork)

