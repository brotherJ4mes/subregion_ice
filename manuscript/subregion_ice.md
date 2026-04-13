# subregion_ice
**poster:**
- [x] reorganize code and `git init`
- [x] SMR move to superior
- [ ] scalebars
- [ ] labels size
stats: STL, loess, met data trends
trends for each metric
**known issues:**
    - station quality (look into better filtering?... existing packages on gh?)
        - multiple phases of filtering in GLSHFS (from station files but also during aggregation?)
            - copy/paste/rework algorithms in fortran code
            - compare to GLSEA? (lake/land diffrences)
    - ice duration (small regions below 10%)
- [x] aggregate bathy (bar plot or spatial colorplot)
- bar plot use area for width and height for bathy?
- [x] debug difference plots
- [x] look at Ayumis (deaggregated data? or maybe just use GLSHFS?)
- [x] github sooner rather than later?
- [x] correlation between metrics? AMIC vs JFM? (maybe drop one)
- [x] mannkendal
- [x] start writing?
scatterplots dependent vars: AMIC JFM duration
wind \propto ice breakup dates? look at onset offset dates?
independent vars: GLSEA
            - JFM avg LST
GLSHFS FDD for each subregion
GLSHFS summary data (station based but for each major lake)



feedback from july meeting:
- [ ] send David paper
- [ ] cleanup/git/HPC location
- [ ] cld cover and other var quality
- [ ] **FIRST**  develop hypothesis for each var
      - which months might have +/- correlations
      - variance by shoreline proximity, bathy, etc.


ideas:
 - spatial map of correlations (after hypo)
 - pie chart or indicators instead of chloropleth
 - number of storms
 - lag time with mixing
 - Granger causality between time series


- sfc currents $\propto$ mixing?
- # of storms
- wind direction importance
    - relative to fetch
    - warm air advection
