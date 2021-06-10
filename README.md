Predicting Winning Bids at Classic & Enthusiast Car Auctions


Background

With just over 47,300 auction records available on the site and over 120,000 registered users, BaT presents a wealth of data with which to ask questions and attempt predictions. While there is a plethora of machine learning research pertaining to auctions, there are limited examples from the automotive sector. This is largely due to the fact that salvage and commercial vehicle auctions, like Manheim, are owned by parent companies (Cox Automotive) that leverage auction data to inform other analytics services in their product suite.  Gaffney (2020) developed a predictive pricing model using data scraped from BaT, but the data collection barrier has prevented this from becoming an overworked data set.  

The variety of data (images, lot descriptions, user comments, bids, datetime stamps) captured prompts a seemingly endless list of questions. 

Considerations:

Lack/Ambiguity of Some Data Points:
Auctions on BaT.com last between 7 - 21 days, however the actual timestamp of when an auction begins is not available in the HTML code. This precludes questions based on how long an item is live before the first bid occurs, as the first bid timestamp must be used as a proxy for the auction open.  Further, there is no visibility on an item’s reserve price set by the seller.  BaT policy states a seller may lower their reserve price at any time during the auction based on the bids placed.  It is worth noting that BaT works with sellers to set a realistic reserve price, as it is not in BaT’s business interests for a large number of auctions to end in ‘reserve not met status.’

Considerations Regarding BaT’s Auction Policies
BaT has implemented some policies to limit the gamification of the auction format.  Auctions do not end until 2 minutes has elapsed since the highest bid was placed, in order to prevent last-minute bidders from swooping in the final seconds of the auction.  Further, a user cannot place a bid that is drastically higher than the second-highest bid.  It is unclear what measures BaT has taken to identify and/or prevent shill bidders, and not all policies are published on the site.  Some must be inferred from the response field messages contained in the site’s HTML/Javascript  source code.  It is likely there are other policies influencing auction dynamics that are unknown and as such, cannot be identified as features in a machine learning model.

