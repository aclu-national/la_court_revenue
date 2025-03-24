# Louisiana Unified Court Budget Document Analysis

## Summary
This tool was created to analyze the criminal funding in every court in Louisiana in 2024. 

## Code
In the code we first name every court with criminal funding in Louisiana. We then define a function to go through a PDF of the court document [(like this)](https://www.lasc.org/Act116/Year_2024/District,%20Family,%20and%20Juvenile%20Courts/1st%20Judicial%20District%20Court.pdf) and create a dataframe with the variables: Criminal Court Funds; Revenue; Expenses; Court cost fees, bond forfeitures, bail fees; Criminal service, processing, and administrative fees; Supervision and special program fees; Special revenue fees; Criminal contempt, other fines; and total criminal spending. We then use this function for all possible courts (providing NA's for non-existent PDFs). This gives us a final dataframe of courts with the above variables.
