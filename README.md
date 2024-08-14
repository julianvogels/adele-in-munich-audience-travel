# Adele in Munich Audience Travel Analysis

## Project Overview
In August 2024, Adele is scheduled to perform 10 concerts in Munich as part of a residency, rather than a European tour. While this setup appears sustainable at first glance — requiring only a single stage setup and minimizing the need for extensive logistics — it overlooks a significant environmental impact: the CO₂ emissions from audience travel. An estimated 750,000 fans are expected to attend, with a substantial number likely to travel by air.

This project, supported by the European Climate Pact Secretariat of the European Commission, aims to address this oversight by collecting "activist data" on the travel choices of concert-goers. Activists from Music Declares Emergency Germany, in collaboration with a specially assembled Local Climate Action Group, will survey attendees on their travel to the venue. These data will be legally collected in public areas without misrepresenting the nature of the survey to participants.

## Repository Structure
Below is the structure of this repository, which includes both data and scripts used in the analysis of the environmental impact of travel to the concerts.

ADELE-IN-MUNICH-AUDIENCE-TRAVEL
│
├── data
│ ├── 2024-08-11-adele-2024-Publikumsumfrage-v1-4-0.csv
│ └── LICENSE (DL-DE-BY-2.0 license text)
├── output
│
└── scripts
├── load
│ ├── load_data.R
│ ├── load_libraries.R
│ └── load_plot_styles.R
│
├── 0_load_all.R
├── 1_analyze_sample_size.R
├── 2_plot_modal_split_transport_performance.R
├── 3_modal_split_main_choice_of_transport.R
├── 4_plot_emissions_by_transport_group.R
├── 5_plot_distance_flights_vs_other_ratio.R
├── 6_plot_allocation.R
├── 7_aggregate_emissions.R
└── LICENSE (MIT)
│
├── .gitignore
└── README.md


## Licensing
- **Data:** The data files in the `data/` directory are licensed under the [Datenlizenz Deutschland – Namensnennung – Version 2.0 (DL-DE-BY-2.0)](https://www.govdata.de/dl-de/by-2-0). Please refer to the `LICENSE` file in the `data/` directory for more details.
- **Scripts:** The R scripts in the `scripts/` directory are licensed under the [MIT License](https://opensource.org/licenses/MIT). Please refer to the `LICENSE` file in the `scripts/` directory for more details.

## How to Use
To replicate the analysis or adapt it to a similar context:
1. Ensure R and necessary packages are installed.
2. Run scripts in the `load` directory to prepare the environment and data.
3. Execute analysis scripts to generate insights and visualizations.
4. Review results in the `output` directory.

This project serves as a foundation for understanding and mitigating the environmental impact of large-scale events in terms of audience transportation choices.