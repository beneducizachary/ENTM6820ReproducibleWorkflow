# ENTM6820: Reproducible Workflow

## Best Management Practices for Wildflower Plantings

This project is an example analysis of data collected as part of the Best Management Practices for Wildflower Plantings research project at the Auburn University Bee Lab. The overarching goal of the project is to assess how well several fall management treatments (mowing, lightly-disking, and their combination) impact the trajectory of supplemental wildflower plantings for pollintators. Specifically, we are interested in the ability of each treatment to promote high wildflower density and diversity, as well as how the composition of wildflowers differs between treatments.

## Sections

### Data

This dataset contains counts of wildflowers (both planted and volunteers), visual estimates of ground cover (bareground, rock/gravel, litter, forbs, and gramminoids), as well as litter depth and height of the tallest living vegetation in 1 m^2 quadrats. Each row represents data collected from a single quadrat. Three sites with 16 subplots each had data collectd in 10 quadrats per subplot within 4 sampling rounds.

- [Data Files](https://github.com/beneducizachary/ENTM6820ReproducibleWorkflow/tree/main/Data)

### Wildflower Density Analysis

Wildflower density at the subplot level was calculated (per 10 m^2) and subplots were assigned with their respective treatments (mowing, light disking, combination, or control). Model selection was carried out, and a best model was selected to test the effect of the treatments, compared to control, on wildflower density per 10 m^2.

- [Wildflower Density Results](https://github.com/beneducizachary/ENTM6820ReproducibleWorkflow/tree/main/Results)
  - [Density Code](https://github.com/beneducizachary/ENTM6820ReproducibleWorkflow/blob/main/Code/WildflowerDensity.R)
  - [Density Markdown](https://github.com/beneducizachary/ENTM6820ReproducibleWorkflow/blob/main/Results/WildflowerDensity.Rmd)
  - [Density html](https://rawgit.com/beneducizachary/ENTM6820ReproducibleWorkflow/blob/main/Results/WildflowerDensity.html)
- Outputs
  - [Density Figure](https://github.com/beneducizachary/ENTM6820ReproducibleWorkflow/blob/main/Figures/DensFig.png)
  - [Density AIC Table](https://github.com/beneducizachary/ENTM6820ReproducibleWorkflow/tree/main/Tables)
  
### Wildflower Diversity Analysis

The effect of treatments on wildflower density was investigated. Asymptotic estimates of wildflower "effective numbers of species" were generated and compared between treatments, sites, and sampling rounds.

- [Wildflower Diversity Results]()
  - [Diversity Code](https://github.com/beneducizachary/ENTM6820ReproducibleWorkflow/blob/main/Code/DiversityEstimation.R)
  - [Diversity Markdown](https://github.com/beneducizachary/ENTM6820ReproducibleWorkflow/blob/main/Results/WildflowerDiversityEstimation.Rmd)
  - [Diversity html](https://rawgit.com/beneducizachary/ENTM6820ReproducibleWorkflow/blob/main/Results/WildflowerDiversityEstimation.html)
- Figures
  - [Treatment Diversity](https://github.com/beneducizachary/ENTM6820ReproducibleWorkflow/blob/main/Figures/TrtDivFig.png)
  - [Site Diversity](https://github.com/beneducizachary/ENTM6820ReproducibleWorkflow/blob/main/Figures/SiteDivFig.png)
  - [Round Diversity](https://github.com/beneducizachary/ENTM6820ReproducibleWorkflow/blob/main/Figures/RoundDivFig.png)
