
data Config = Config {discountRate::Float,
                      currencySym::String}
     
appCfg = (Config 10 "R")

discount :: Float -> Reader Config Float
discount amt = do
    discountRate' <- asks discountRate
    return (amt * (1 - discountRate' /100))