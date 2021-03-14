### What Was Implemented

Our dashboard has three pages: an overview, a comparison of red and white wines, and a comparison of  different wine quality levels. 
The “Overview” page provides information on how to use the app and includes some references and further information for those interested in wine aspects the dashboard does not cover. We are particularly proud of the aesthetics and color scheme of this page.
The second page is “Wine Type”, primarily intended to demonstrate differences between red and white wines. This page has a good layout, without leaving any large white spaces. It also demonstrates a cohesive narrative, with users able to distinguish high correlation variables from the correlation matrix and then investigating deeper using the scatter plot and density plot.
The third page is “Quality Factors”, where users can explore features of wines in different quality groups. Users can subset the data range by selecting areas on the scatter plot, which immediately updates the other two plots. The bar plot allows users to visualize quality factor proportions in their selections. The “drag and drop” functionality makes this page particularly interactive. 

### Team Dynamics

Our team did a good job of distributing workload, with some members focusing on visualizations and others focusing on the app layout and deployment. Our biggest issue was not finalizing the data wrangling and app structure from the beginning. There was a lot of unnecessary work as we all did our own wrangling and then later had to alter code to follow a uniform terminology and working app structure.

### Limitations for the Dashboard

Our main limitation is the absence of machine learning (ML) visualizations. This data set was popularized through a Kaggle competition in machine learning, and interactive ML visualizations would expand the audience to this crowd as well. While we incorporated basic linear regression, there are no error bars or other ML methods, which ultimately limits our insight into wine features.

We wanted to build some functionality for users to input values for each physicochemical property and have the app predict the quality of a wine with those properties. This would have required more ML work (unsupervised clustering), which we did not have the time for. We also could not figure out an effective way for users to visualize interactions between variables (aside from the interaction of “red” vs. “white” on each property).

While we believe we did a very good job with the app layout given our limited experience and time frame, a lot of work could still be done to make the app look more professional and user friendly.

### Future Considerations

- Addition of an ML focused tab for ML visualizations.
- Further improvements to the layout and aesthetic of the dashboard (tabs, cards, organization, themes and styles (CSS)).
- Fix density plot rendering when using “any” wine quality (plot is very jagged instead of smoothed).
- Include a page focused on statistics for a deeper look at raw data
- External links and additional resources should be included to other articles, papers, and datasets.
- Inclusion of user friendly instructions to help users learn how to use the dashboard.
