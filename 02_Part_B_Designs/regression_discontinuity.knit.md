







## Regression Discontinuity

Regression discontinuity designs exploit substantive knowledge that treatment is assigned in a particular way: everyone above a threshold is assigned to treatment and everyone below it is not. Even though researchers do not control the assignment, substantive knowledge about the threshold serves as a basis for a strong identification claim.

Thistlewhite and Campbell introduced the regression discontinuity design in the 1960s to study the impact of scholarships on academic success. Their insight was that students with a test score just above a scholarship cutoff were plausibly comparable to students whose scores were just below the cutoff, so any differences in future academic success could be attributed to the scholarship itself.

Regression discontinuity designs identify a *local* average treatment effect: the average effect of treatment *exactly at the cutoff*. The main trouble with the design is that there is vanishingly little data exactly at the cutoff, so any answer strategy needs to use data that is some distance away from the cutoff. The further away from the cutoff we move, the larger the threat of bias.

We'll consider an application of the regression discontinuity design that examines party incumbency advantage -- the effect of a party winning an election on its vote margin in the next election.

### Design Declaration

  - **M**odel: Regression discontinuity designs have four components: A running variable, a cutoff, a treatment variable, and an outcome. The cutoff determines which units are treated depending on the value of the running variable.
  
    In our example, the running variable $X$ is the Democratic party's margin of victory at time $t-1$; and the treatment, $Z$, is whether the Democratic party won the election in time $t-1$. The outcome, $Y$, is the Democratic vote margin at time $t$. We'll consider a population of 1,000 of these pairs of elections.
    
    A major assumption required for regression discontinuity is that the conditional expectation functions for both treatment and control potential outcomes are continuous at the cutoff.^[An alternative motivation for some designs that do not rely on continuity at the cutoff is "local randomization".] To satisfy this assumption, we specify two smooth conditional expectation functions, one for each potential outcome. The figure plots $Y$ (the Democratic vote margin at time $t$) against $X$ (the margin at time $t-1$). We've also plotted the true conditional expectation functions for the treated and control potential outcomes. The solid lines correspond to the observed data and the dashed lines correspond to the unobserved data.
    
    








