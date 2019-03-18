# Programmablauf

## Data-Input
- Sonnenwinkel zu vorgegebenem Ort und Zeitpunkt
- Einstrahlmegen zu vorgegebenem Ort und Zeitpunkt
- Datenblatt von Solarpanel

## Variabel
- einzustellender Höhenwinkel


----

## Einstrahlfläche  -> berechne_relative_einstrahlflaeche
- Wie viel relative "Einstrahlfläche" man zu welchem Zeitpunkt hat
- **input** sonnenwinkelelevation, sonnenwinkelazimuth, eingestellterazimuthwinkel
- **output** double Wert
- **zu klären:** Wie reagiert Funktion auf vektorisierte Sonnenwinkel aber atomarer Einstellwinkel?

## Einstrahlenergie
- Wieviel Strahlungsenergie bei einer gegebenen Stellung des Panels zu einem gegbenen Ort und Zeitpunkt aufgefangen wird
- **input** gemessene Stahlungsenergie/m²_flach und Einstrahlfläche (output aus f_einstrahlflaeche)
- **output** double wert

## Produzierte Energie
- Wieviel Energie das gegebene Solarpanel bei einer gewissen Strahlungsenergie/m² produziert
- **input** Datenblatt PV-Panel und Einstrahlenergie (output aus f_einsrahlenergie)
- **output** double Wert

---

## Bilanzierung der Leistung
- Addierung der produzierten Enegiermenge/m²_Solarpanel über einen Zeitraum (d.h. Integral berechnen)
- **input** Produzierte Energie über Zeitraum (output von f_prodEnergie und dazugehörige Zeitpunkte)
- **output** double Wert
- **Bemerkung** Hier zum ersten mal der Schritt von atomaren Inputs zu transformierter Datenklasse (df zu atomic vector)

## Bestvergleich von Leistungen
- Leistungswerte verschiedener Kippeinstellungen und die beste auswählen
- **input** Leistungswerte verschiedener Kippeinstellungen
- **output** Kippeinstellung mit dazugehörigem Leistungswert

## Vergleich zu Leistung
- Leistungswerte von 2 Einstellungen vergleichen, d.h. prozentuale Unterschiede etc.
 
---

## Automatische Tests
- Den eben beschriebenen Durchlauf mehrmals durchführen und dabei algorithmisch den besten festen Kippwinkel finden